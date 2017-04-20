-module(gen_load_balancer).
-behaviour(gen_server).

%% API
-export([start_link/2,
         start_link/3,
         pid/1,
         resize/2,
         size/1,
         stop/1]).

%% gen_server callbacks
-export([init/1
         , handle_call/3
         , handle_cast/2
         , handle_info/2
         , terminate/2
         , code_change/3]).

-type child() :: 'undefined' | pid().
-type mfargs() :: {M :: module(), F :: atom(), A :: [term()] | undefined}.
-type strategy() :: round_robin.
-type lb_name() :: {'local', Name :: atom()}
| {'global', Name :: atom()}
| {'via', Module :: module(), Name :: any()}.
-type startlink_err() :: {'already_started', pid()}
| {'shutdown', term()}
| term().
-type startlink_ret() :: {'ok', pid()}
| 'ignore'
| {'error', startlink_err()}.
-type lb_ref() :: (Name :: atom())
| {Name :: atom(), Node :: node()}
| {'global', Name :: atom()}
| {'via', Module :: module(), Name :: any()}
| pid().
-type shutdown() :: 'brutal_kill' | timeout().

-type child_spec() :: #{start := mfargs(),
                        shutdown => shutdown()}.      % optional
-type lb_flags() :: #{strategy => strategy(),         % optional
                      intensity => non_neg_integer(), % optional
                      period => non_neg_integer(),    % optional
                      size => non_neg_integer()}.     % optional

-define(DEFAULT_FLAGS, #{strategy => round_robin,
                         intensity => 1,
                         period => 5,
                         size => 5}).
-define(DEFAULT_CHILD_SPEC, #{restart  => permanent}).

-callback init(Args :: term()) ->
  {ok, LbFlags :: lb_flags(), ChildSpec :: child_spec()}
  | {ok, ChildSpec :: child_spec()}
  | ignore.

-record(child, {pid = undefined :: child()
                | {restarting, pid() | undefined}
                | pid(),
                mfargs          :: mfargs(),
                shutdown        :: shutdown()}).

-record(state, {
          name,
          children = [],

          strategy,
          size,
          intensity,
          period,
          restarts = [],
          module,
          args,
          child_spec
         }).

% @doc
% Create a load balancer supervisor process as part of the supervision tree.
% @end
-spec start_link(Mod :: module(), Args :: list()) -> startlink_ret().
start_link(Mod, Args) ->
  gen_server:start_link(?MODULE, {self, Mod, Args}, []).

% @doc
% Create a load balancer supervisor process as part of the supervision tree.
% @end
-spec start_link(LbName :: lb_name(), Mod :: module(), Args :: list()) -> startlink_ret().
start_link(LbName, Mod, Args) ->
  gen_server:start_link(LbName, ?MODULE, {LbName, Mod, Args}, []).

% @doc
% Orders a generic load balancer to exit and waits for it to terminate.
% @end
-spec stop(LoadBalancer :: lb_ref()) -> ok.
stop(LoadBalancer) ->
  gen_server:stop(LoadBalancer).

% @doc
% Return the next available Pid.
% @end
-spec pid(LoadBalancer :: lb_ref()) -> pid().
pid(LoadBalancer) ->
  gen_server:call(LoadBalancer, pid, infinity).

% @doc
% Resize the list of processes supervised by the load balancer.
% @end
-spec resize(LoadBalancer :: lb_ref(), Size :: non_neg_integer()) ->
  {ok, non_neg_integer()}
  | {error, term()}.
resize(LoadBalancer, Size) ->
  if Size > 0 -> gen_server:call(LoadBalancer, {resize, Size});
     true -> {error, invalid_size}
  end.

% @doc
% Return the size of the list of processes supervised by the load balancer.
% @end
-spec size(LoadBalancer :: lb_ref()) -> non_neg_integer().
size(LoadBalancer) ->
  gen_server:call(LoadBalancer, size, infinity).

% @hidden
handle_call(pid, _From, #state{children = Children, strategy = Strategy} = State) ->
  case pid(Children, Strategy) of
    {ok, Reply, NChildren} ->
      {reply, {ok, Reply}, State#state{children = NChildren}};
    {error, Reason} ->
      {reply, {error, Reason}, State}
  end;
handle_call(size, _From, #state{children = Children} = State) ->
  {reply, get_size(Children), State};
handle_call({resize, Size}, _From, #state{children = Children,
                                          name = LbName,
                                          child_spec = Child} = State) ->
  case if
    Size > length(Children) -> scale_up(LbName, Child, Children, Size - length(Children));
    Size < length(Children) -> scale_down(LbName, Children, length(Children) - Size);
    true -> {ok, Children}
  end of
    {ok, NChildren} ->
      {reply, {ok, length(NChildren)}, State#state{children = NChildren}};
    {error, Reason} ->
      {reply, {error, Reason}, State}
  end;
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

% @hidden
handle_cast(_Msg, State) ->
  {noreply, State}.

% @hidden
handle_info({'EXIT', Pid, Reason}, State) ->
  case restart_child(Pid, Reason, State) of
    {ok, State1} ->
      {noreply, State1};
    {shutdown, State1} ->
      {stop, shutdown, State1}
  end;
handle_info(_Info, State) ->
  {noreply, State}.

% @hidden
terminate(_Reason, State) ->
  terminate_children(State#state.children, State#state.name).

% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% @hidden
init({LbName, Mod, Args}) ->
  process_flag(trap_exit, true),
  case Mod:init(Args) of
    {ok, LbFlags, ChildSpec} ->
      start_childs(LbName, Mod, Args, LbFlags, ChildSpec);
    {ok, ChildSpec} ->
      start_childs(LbName, Mod, Args, ?DEFAULT_FLAGS, ChildSpec);
    ignore ->
      ignore;
    Error ->
      {stop, {bad_return, {Mod, init, Error}}}
  end.

% @hidden
start_childs(LbName, Mod, Args, LbFlags, ChildSpec) ->
  case init_state(LbName, LbFlags, Mod, Args) of
    {ok, State} ->
      init_children(State, ChildSpec);
    Error ->
      {stop, {gen_load_balancer_data, Error}}
  end.

% @hidden
init_state(LbName, LbFlags, Mod, Args) ->
  set_flags(LbFlags, #state{
                        name = lbname(LbName, Mod),
                        module = Mod,
                        args = Args
                       }).

set_flags(LbFlags, State) ->
  try check_flags(LbFlags) of
    #{strategy := Strategy, intensity := Intensity, period := Period, size := Size} ->
      {ok, State#state{strategy = Strategy,
                       intensity = Intensity,
                       period = Period,
                       size = Size}}
  catch
    Thrown -> Thrown
  end.

check_flags(LbFlags) when is_map(LbFlags) ->
  do_check_flags(maps:merge(?DEFAULT_FLAGS, LbFlags));
check_flags(What) ->
  throw({invalid_type, What}).

do_check_flags(#{strategy := Strategy,
                 intensity := Intensity,
                 period := Period,
                 size := Size} = LbFlags) ->
  validStrategy(Strategy),
  validIntensity(Intensity),
  validPeriod(Period),
  validSize(Size),
  LbFlags.

validStrategy(round_robin) -> true;
validStrategy(What) -> throw({invalid_strategy, What}).

validIntensity(Intensity) when is_integer(Intensity), Intensity >= 0-> true;
validIntensity(What) -> throw({invalid_intensity, What}).

validPeriod(Period) when is_integer(Period), Period > 0-> true;
validPeriod(What) -> throw({invalid_period, What}).

validSize(Size) when is_integer(Size), Size >= 0-> true;
validSize(What) -> throw({invalid_size, What}).

lbname(self, Mod) -> {self(), Mod};
lbname(N, _)      -> N.

% @hidden
init_children(#state{name = LbName, size = Size} = State, ChildSpec) ->
  case check_childspec(ChildSpec) of
    {ok, Child} ->
      case start_childrens(Child, LbName, Size) of
        {ok, Children} ->
          {ok, State#state{children = Children, child_spec = Child}};
        {error, Children, Reason} ->
          _ = terminate_children(Children, LbName),
          {stop, {shutdown, Reason}}
      end;
    Error ->
      {stop, {start_spec, Error}}
  end.

start_childrens(Child, LbName, Size) ->
  start_childrens(Child, LbName, Size, []).

start_childrens(_, _, 0, Acc) ->
  {ok, lists:reverse(Acc)};
start_childrens(Child, LbName, N, Acc) ->
  case do_start_child(LbName, Child) of
    {ok, Pid} ->
      start_childrens(Child, LbName, N - 1, [Child#child{pid = Pid}|Acc]);
    {ok, Pid, _Extra} ->
      start_childrens(Child, LbName, N - 1, [Child#child{pid = Pid}|Acc]);
    {error, Reason} ->
      {error, lists:reverse(Acc), {failed_to_start_child, Reason}}
  end.

do_start_child(_LbName, #child{mfargs = {M, F, A}}) ->
  case catch apply(M, F, A) of
    {ok, Pid} -> {ok, Pid};
    {ok, Pid, Extra} -> {ok, Pid, Extra};
    ignore -> {ok, undefined};
    {error, What} -> {error, What};
    What -> {error, What}
  end.

terminate_children(Children, LbName) ->
  terminate_children(Children, LbName, []).

terminate_children([], _LbName, Acc) ->
  Acc;
terminate_children([Child|Children], LbName, Acc) ->
  terminate_children(Children, LbName, [do_terminate(Child, LbName)|Acc]).

do_terminate(Child, _LbName) when is_pid(Child#child.pid) ->
  _ = shutdown(Child#child.pid, Child#child.shutdown),
  Child#child{pid = undefined};
do_terminate(Child, _LbName) ->
  Child#child{pid = undefined}.

shutdown(Pid, brutal_kill) ->
  case monitor_child(Pid) of
    ok ->
      exit(Pid, kill),
      receive
        {'DOWN', _MRef, process, Pid, killed} ->
          ok;
        {'DOWN', _MRef, process, Pid, OtherReason} ->
          {error, OtherReason}
      end;
    {error, Reason} ->
      {error, Reason}
  end;
shutdown(Pid, Time) ->
  case monitor_child(Pid) of
    ok ->
      exit(Pid, shutdown), %% Try to shutdown gracefully
      receive
        {'DOWN', _MRef, process, Pid, shutdown} ->
          ok;
        {'DOWN', _MRef, process, Pid, OtherReason} ->
          {error, OtherReason}
      after Time ->
              exit(Pid, kill),  %% Force termination.
              receive
                {'DOWN', _MRef, process, Pid, OtherReason} ->
                  {error, OtherReason}
              end
      end;
    {error, Reason} ->
      {error, Reason}
  end.

monitor_child(Pid) ->
  erlang:monitor(process, Pid),
  unlink(Pid),

  receive
    {'EXIT', Pid, Reason} ->
      receive
        {'DOWN', _, process, Pid, _} ->
          {error, Reason}
      end
  after 0 ->
          ok
  end.

check_childspec(ChildSpec) when is_map(ChildSpec) ->
  Func = case ChildSpec of
           #{start := F} -> F;
           _ -> throw(missing_start)
         end,
  validFunc(Func),
  Shutdown = case ChildSpec of
               #{shutdown := S} -> S;
               #{type := worker} -> 5000;
               #{type := supervisor} -> infinity
             end,
  validShutdown(Shutdown),
  {ok, #child{mfargs = Func,
              shutdown = Shutdown}};
check_childspec(What) -> throw({invalid_child_spec, What}).

validFunc({M, F, A}) when is_atom(M),
                          is_atom(F),
                          is_list(A) -> true;
validFunc(What) -> throw({invalid_mfa, What}).

validShutdown(Shutdown) when is_integer(Shutdown), Shutdown > 0 -> true;
validShutdown(infinity) -> true;
validShutdown(brutal_kill) -> true;
validShutdown(Shutdown) -> throw({invalid_shutdown, Shutdown}).

restart_child(Pid, Reason, #state{children = Children} = State) ->
  case lists:keyfind(Pid, #child.pid, Children) of
    false ->
      {ok, State};
    Child ->
      do_restart(Reason, Child, State)
  end.

do_restart(normal, Child, State) ->
  {ok, state_del_child(Child, State)};
do_restart(shutdown, Child, State) ->
  {ok, state_del_child(Child, State)};
do_restart({shutdown, _Term}, Child, State) ->
  {ok, state_del_child(Child, State)};
do_restart(_Reason, Child, State) ->
  restart(Child, State).

restart(_Child, State) -> % TODO
  {ok, State}.

state_del_child(Child, State) ->
  State#state{children = del_child(Child#child.pid, State#state.children)}.

del_child(_Pid, []) ->
  [];
del_child(Pid, [Child|Children]) when Child#child.pid =:= Pid ->
  Children;
del_child(Pid, [Child|Children]) ->
  [Child|del_child(Pid, Children)].

%% API function

pid([#child{pid = Pid} = Child|Children], round_robin) ->
  {ok, Pid, Children ++ [Child]}.

get_size([]) ->
  0;
get_size([#child{pid = undefined}|Children]) ->
  get_size(Children);
get_size([#child{pid = Pid}|Children]) when is_pid(Pid) ->
  1 + get_size(Children).

scale_up(LbName, Child, Children, Size) ->
  case start_childrens(Child, LbName, Size) of
    {ok, NChildren} ->
      {ok, lists:merge(Children, NChildren)};
    {error, NChildren, {failed_to_start_child, Reason}} ->
      _ = terminate_children(NChildren, LbName),
      {error, Reason}
  end.

scale_down(LbName, Children, Size) ->
  [Terminate, Keep] = split(Children, Size),
  _ = terminate_children(Terminate, LbName),
  {ok, Keep}.

split([], _)->
  [];
split([H|T], 1)->
  [[H], T];
split([H|T], X)->
  [RH, RT] = split(T, X-1),
  [[H|RH], RT].

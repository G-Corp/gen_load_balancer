-module(gen_load_balanger_tests).
-include_lib("eunit/include/eunit.hrl").

common_test_() ->
  {setup,
   fun() ->
       test_lb:start_link()
   end,
   fun(_) ->
       test_lb:stop()
   end,
   [
    fun() ->
        ?assertEqual(5, test_lb:size())
    end,
    fun() ->
        {ok, Pid1} = test_lb:pid(),
        {ok, Pid2} = test_lb:pid(),
        [?assertNotEqual(PidA, PidB) || {PidA, PidB} <- couples([Pid1, Pid2])],
        {ok, Pid3} = test_lb:pid(),
        [?assertNotEqual(PidA, PidB) || {PidA, PidB} <- couples([Pid1, Pid2, Pid3])],
        {ok, Pid4} = test_lb:pid(),
        [?assertNotEqual(PidA, PidB) || {PidA, PidB} <- couples([Pid1, Pid2, Pid3, Pid4])],
        {ok, Pid5} = test_lb:pid(),
        [?assertNotEqual(PidA, PidB) || {PidA, PidB} <- couples([Pid1, Pid2, Pid3, Pid4, Pid5])],
        {ok, Pid6} = test_lb:pid(),
        ?assertEqual(Pid1, Pid6),
        [?assertNotEqual(PidA, PidB) || {PidA, PidB} <- couples([Pid2, Pid3, Pid4, Pid5, Pid6])]
    end
   ]}.

scale_up_test_() ->
  {setup,
   fun() ->
       test_lb:start_link()
   end,
   fun(_) ->
       test_lb:stop()
   end,
   [
    fun() ->
        ?assertEqual(5, test_lb:size()),
        [?assertNotEqual(PidA, PidB) || {PidA, PidB} <- couples(test_lb_pids(5))],
        ?assertEqual({ok, 7}, test_lb:resize(7)),
        ?assertEqual(7, test_lb:size()),
        [?assertNotEqual(PidA, PidB) || {PidA, PidB} <- couples(test_lb_pids(7))]
    end
   ]}.

scale_down_test_() ->
  {setup,
   fun() ->
       test_lb:start_link()
   end,
   fun(_) ->
       test_lb:stop()
   end,
   [
    fun() ->
        ?assertEqual(5, test_lb:size()),
        [?assertNotEqual(PidA, PidB) || {PidA, PidB} <- couples(test_lb_pids(5))],
        ?assertEqual({ok, 3}, test_lb:resize(3)),
        ?assertEqual(3, test_lb:size()),
        [?assertNotEqual(PidA, PidB) || {PidA, PidB} <- couples(test_lb_pids(3))]
    end
   ]}.

test_lb_pids(N) ->
  [begin
     {ok, Pid} = test_lb:pid(),
     Pid
   end || _ <- lists:seq(1, N)].

couples([]) ->
  [];
couples([E|List]) ->
  lists:merge([{E, O} || O <- List], couples(List)).

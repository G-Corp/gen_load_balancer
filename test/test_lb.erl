-module(test_lb).
-behaviour(gen_load_balancer).

-export([start_link/0, stop/0, pid/0, size/0, resize/1]).
-export([init/1]).

-define(LB, ?MODULE).

start_link() ->
  gen_load_balancer:start_link({local, ?LB}, ?MODULE, []).

stop() ->
  gen_load_balancer:stop(?LB).

pid() ->
  gen_load_balancer:pid(?LB).

size() ->
  gen_load_balancer:size(?LB).

resize(N) ->
  gen_load_balancer:resize(?LB, N).

init(_Args) ->
  {ok,
   #{strategy => round_robin,
     size => 5,
     intensity => 1,
     period => 5},
   #{start => {test_server, start_link, []},
     shutdown => 1000}}.


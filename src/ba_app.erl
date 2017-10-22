-module(ba_app).

-behaviour(application).

%% API
-export([]).

%% Callbacks
-export([start/2, stop/1]).

%%====================================================================
%% Callbacks
%%====================================================================

start(_StartType, _StartArgs) ->
  io:format("args: ~p ~p ~p ~n", [_StartType, _StartArgs, bake_cookie()]),
  ok = ensure_node_is_alive(),
  true = erlang:set_cookie(node(), bake_cookie()),
  ba_sup:start_link().

stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

ensure_node_is_alive() ->
    case is_alive() of
        false -> {ok, _} = net_kernel:start(['mynode', shortnames])
    end,
    ok.

bake_cookie() ->
    case application:get_env(cookie) of
        {ok, Value} when erlang:is_atom(Value) -> Value;
        _ -> 'NotSoSecretCookie'
    end.

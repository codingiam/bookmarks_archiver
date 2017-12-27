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
  ba_sup:start_link().

stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

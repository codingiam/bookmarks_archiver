%%%-------------------------------------------------------------------
%% @doc bookmarks_archiver public API
%% @end
%%%-------------------------------------------------------------------

-module(bookmarks_archiver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    bookmarks_archiver_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
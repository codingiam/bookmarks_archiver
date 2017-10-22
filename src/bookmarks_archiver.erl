-module(bookmarks_archiver).

-author("doru").

%% API
-export([run/0]).

%%====================================================================
%% API
%%====================================================================

run() ->
  ok = application:start(bookmarks_archiver).

-module(ba_stm).

-behaviour(gen_statem).

%% API
-export([start/0, start_link/0, connect/0, get_json/0]).

%% Callbacks
-export([
  init/1,
  callback_mode/0,
  closed/3,
  disconnected/3,
  connected/3,
  terminate/3,
  code_change/4
]).

-define(SERVER, ?MODULE).

-define(WAIT_TIMEOUT, 10000).

-record(data, {conn_pid}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
  gen_statem:start({local, ?SERVER}, ?MODULE, [], []).

connect() ->
  gen_statem:cast(?SERVER, connect).

get_json() ->
  gen_statem:call(?SERVER, get_json).

%%%===================================================================
%%% Callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {CallbackMode, StateName, Data} |
%%                     {CallbackMode, StateName, Data, Actions} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  {ok, closed, #data{conn_pid = undefined}}.

%%--------------------------------------------------------------------
callback_mode() ->
  state_functions.

%%--------------------------------------------------------------------
closed(_, connect, Data) ->
  {ok, ConnPid} = gun:open("localhost", 9222, #{http_opts => #{keepalive => infinity}}),
  {next_state, disconnected, Data#data{conn_pid = ConnPid}, ?WAIT_TIMEOUT};

closed(EventType, EventContent, Data) ->
  handle_event(closed, EventType, EventContent, Data).

%%--------------------------------------------------------------------
disconnected(timeout, _, #data{conn_pid = ConnPid} = Data) ->
  gun:close(ConnPid),
  {next_state, closed, Data#data{conn_pid = undefined}};

disconnected(info, {gun_up, ConnPid, http}, #data{conn_pid = ConnPid} = Data) ->
  {next_state, connected, Data};

disconnected(EventType, EventContent, Data) ->
  handle_event(disconnected, EventType, EventContent, Data).

%%--------------------------------------------------------------------
connected({call, From}, get_json, #data{conn_pid = ConnPid}) ->
  Response = gun_get(ConnPid, "/json", [{<<"accept">>, "application/json"}]),
  {keep_state_and_data, [{reply, From, Response}]};

connected(info, {gun_down, ConnPid, http, closed, _, _}, #data{conn_pid = ConnPid} = Data) ->
  gun:close(ConnPid),
  {next_state, disconnected, Data#data{conn_pid = undefined}};

connected(EventType, EventContent, Data) ->
  handle_event(connected, EventType, EventContent, Data).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, Data) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _Data) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, Data, Extra) ->
%%                   {ok, StateName, NewData}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, Data, _Extra) ->
  {ok, StateName, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_event(StateName, EventType, EventContent, Data) ->
  io:format("handle_event... [~p, ~p, ~p, ~p]~n", [StateName, EventType, EventContent, Data]),
  erlang:error(unhandled_event).

gun_get(ConnPid, Path, Headers) ->
  StreamRef = gun:get(ConnPid, Path, Headers),
  case gun:await(ConnPid, StreamRef) of
    {response, fin, _Status, _Headers} ->
      no_data;
    {response, nofin, _Status, _Headers} ->
      {ok, Body} = gun:await_body(ConnPid, StreamRef),
      Body
  end.

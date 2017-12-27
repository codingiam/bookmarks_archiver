-module(ba_stm).

-behaviour(gen_statem).

%% API
-export([start/0, start_link/0, connect/0, get/0]).

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

-record(state, {conn_pid}).

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

get() ->
  gen_statem:call(?SERVER, get).


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
%% @spec init(Args) -> {CallbackMode, StateName, State} |
%%                     {CallbackMode, StateName, State, Actions} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  {ok, closed, #state{conn_pid = undefined}}.

callback_mode() ->
  state_functions.

closed(EventType, EventContent = connect, State) ->
  io:format("closed... [~p, ~p, ~p] ~n", [EventType, EventContent, State]),
  {ok, ConnPid} = gun:open("localhost", 9222, #{http_opts => #{keepalive => infinity}}),
  NextStateName = disconnected,
  {next_state, NextStateName, State#state{conn_pid = ConnPid}, 10000}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name.  If callback_mode is statefunctions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
%%
%% @spec disconnected(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% @end
%%--------------------------------------------------------------------
disconnected(EventType = timeout, EventContent, #state{conn_pid = ConnPid} = State) ->
  io:format("disconnected... [~p, ~p, ~p] ~n", [EventType, EventContent, State]),
  gun:close(ConnPid),
  NextStateName = closed,
  {next_state, NextStateName, State#state{conn_pid = undefined}};
disconnected(EventType = info, EventContent = {gun_up, ConnPid, http}, #state{conn_pid = ConnPid} = State) ->
  io:format("disconnected... [~p, ~p, ~p] ~n", [EventType, EventContent, State]),
  NextStateName = connected,
  {next_state, NextStateName, State}.

connected({call, From} = EventType, EventContent = get, #state{conn_pid = ConnPid} = State) ->
  io:format("connected... [~p, ~p, ~p] ~n", [EventType, EventContent, State]),
  StreamRef = gun:get(ConnPid, "/json", [{<<"accept">>, "application/json"}]),
  Data = case gun:await(ConnPid, StreamRef) of
           {response, fin, _Status, _Headers} ->
             no_data;
           {response, nofin, _Status, _Headers} ->
             {ok, Body} = gun:await_body(ConnPid, StreamRef),
             Body
         end,
  {keep_state_and_data, [{reply, From, Data}]};
connected(EventType = info, EventContent = {gun_down, ConnPid, http, closed, _, _}, #state{conn_pid = ConnPid} = State) ->
  io:format("connected... [~p, ~p, ~p] ~n", [EventType, EventContent, State]),
  NextStateName = disconnected,
  {next_state, NextStateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

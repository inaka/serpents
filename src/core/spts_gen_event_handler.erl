%% @doc behaviour for spts_core event handlers
-module(spts_gen_event_handler).
-author('elbrujohalcon@inaka.net').

-behaviour(gen_event).

-callback notify(pid(), spts_core:event()) -> _.

-export(
  [ subscribe/3
  , unsubscribe/3
  ]).
-export(
  [ init/1
  , handle_event/2
  , handle_call/2
  , handle_info/2
  , terminate/2
  , code_change/3
  ]).

-record(state, {process :: pid(), module :: module()}).
-type state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec subscribe(spts_games:id(), module(), pid()) -> ok.
subscribe(GameId, Module, Process) ->
  spts_core:subscribe(GameId, {?MODULE, {Module, Process}}, {Module, Process}).

-spec unsubscribe(spts_games:id(), module(), pid()) -> ok.
unsubscribe(GameId, Module, Process) ->
  spts_core:call_handler(GameId, {?MODULE, {Module, Process}}, remove).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EVENT CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init({pid(), ok|error}) -> {ok, state()}.
init({{Module, Process}, _TerminateResult}) ->
  {ok, #state{module = Module, process = Process}}.

-spec handle_event(spts_core:event(), state()) -> {ok, state()}.
handle_event(Event, State) ->
  (State#state.module):notify(State#state.process, Event),
  {ok, State}.

-spec handle_call(remove, state()) -> {remove_handler, ok}.
handle_call(remove, _State) -> {remove_handler, ok}.

-spec handle_info(term(), state()) -> {ok, state()}.
handle_info(Info, State) -> State#state.process ! Info, {ok, State}.

-spec terminate(term(), state()) -> ok.
terminate(Reason, State) ->
  lager:notice("~p for ~p terminating: ~p", [?MODULE, State, Reason]).

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% @doc game events handler for spts_news_handler
-module(spts_news_event_handler).
-author('elbrujohalcon@inaka.net').

-behaviour(gen_event).

-export(
  [ subscribe/2
  , unsubscribe/2
  ]).
-export(
  [ init/1
  , handle_event/2
  , handle_call/2
  , handle_info/2
  , terminate/2
  , code_change/3
  ]).

-record(state, {process :: pid()}).
-type state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec subscribe(spts_games:id(), pid()) -> ok.
subscribe(GameId, Process) ->
  spts_core:subscribe(GameId, {?MODULE, Process}, Process).

-spec unsubscribe(spts_games:id(), pid()) -> ok.
unsubscribe(GameId, TestProcess) ->
  spts_core:call_handler(GameId, {?MODULE, TestProcess}, remove).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EVENT CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init(pid()) -> {ok, state()}.
init(Process) -> {ok, #state{process = Process}}.

-spec handle_event(spts_core:event(), state()) -> {ok, state()}.
handle_event(Event, State) ->
  lasse_handler:notify(State#state.process, Event),
  {ok, State}.

-spec handle_call(remove, state()) -> {remove_handler, ok}.
handle_call(remove, _State) -> {remove_handler, ok}.

-spec handle_info(term(), state()) -> {ok, state()}.
handle_info(Info, State) ->
  State#state.process ! Info,
  {ok, State}.

-spec terminate(term(), state()) -> ok.
terminate(Reason, State) ->
  lager:notice("~p for ~p terminating: ~p", [?MODULE, State, Reason]).

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

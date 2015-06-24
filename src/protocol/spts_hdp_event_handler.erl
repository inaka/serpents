-module(spts_hdp_event_handler).
-author('elbrujohalcon@inaka.net').

-behaviour(gen_event).

-export(
  [ subscribe/1
  , unsubscribe/1
  ]).
-export(
  [ init/1
  , handle_event/2
  , handle_call/2
  , handle_info/2
  , terminate/2
  , code_change/3
  ]).

-record(state, {}).
-type state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec subscribe(spts_games:id()) -> ok.
subscribe(GameId) -> spts_core:subscribe(GameId, ?MODULE, noargs).

-spec unsubscribe(spts_games:id()) -> ok.
unsubscribe(GameId) -> spts_core:call_handler(GameId, ?MODULE, remove).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EVENT CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init({noargs, ok | error}) -> {ok, state()}.
init({noargs, _TerminateResult}) -> {ok, #state{}}.

-spec handle_event(spts_core:event(), state()) -> {ok, state()}.
handle_event(Event, State) ->
  spts_hdp_game_handler ! {event, Event}, {ok, State}.

-spec handle_call(remove, state()) -> {remove_handler, ok}.
handle_call(remove, _State) -> {remove_handler, ok}.

-spec handle_info(term(), state()) -> {ok, state()}.
handle_info(Info, State) -> spts_hdp_game_handler ! {info, Info}, {ok, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) -> ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

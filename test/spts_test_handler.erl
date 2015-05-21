-module(spts_test_handler).

-behaviour(gen_event).

-export(
  [ subscribe/2
  , unsubscribe/2
  , wait_for/1
  , no_events/0
  , flush/0
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
subscribe(GameId, TestProcess) ->
  spts_core:subscribe(GameId, {?MODULE, TestProcess}, TestProcess).

-spec unsubscribe(spts_games:id(), pid()) -> ok.
unsubscribe(GameId, TestProcess) ->
  spts_core:call_handler(GameId, {?MODULE, TestProcess}, remove).

-spec wait_for(spts_core:event()) -> ok.
wait_for(Event) ->
  receive
    {event, Event} -> ok;
    {event, OtherEvent} -> ct:fail("Unexpected Event: ~p", [OtherEvent]);
    {info, Info} -> ct:fail("Unexpected Info: ~p", [Info]);
    Thing -> ct:fail("Unexpected Thing: ~p", [Thing])
  after 1000 -> ct:fail("Missing Event")
  end.

-spec no_events() -> ok.
no_events() ->
  receive
    {event, Event} -> ct:fail("Unexpected Event: ~p", [Event]);
    {info, Info} -> ct:fail("Unexpected Info: ~p", [Info]);
    Thing -> ct:fail("Unexpected Thing: ~p", [Thing])
  after 1000 -> ok
  end.

-spec flush() -> ok.
flush() ->
  receive
    _ -> flush()
  after 0 -> ok
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EVENT CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init(pid()) -> {ok, state()}.
init(Process) -> {ok, #state{process = Process}}.

-spec handle_event(spts_core:event(), state()) -> {ok, state()}.
handle_event(Event, State) -> State#state.process ! {event, Event}, {ok, State}.

-spec handle_call(remove, state()) -> {remove_handler, ok}.
handle_call(remove, _State) -> {remove_handler, ok}.

-spec handle_info(term(), state()) -> {ok, state()}.
handle_info(Info, State) -> State#state.process ! {info, Info}, {ok, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) -> ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

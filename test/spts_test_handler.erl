-module(spts_test_handler).

-behaviour(spts_gen_event_handler).

-export(
  [ subscribe/2
  , unsubscribe/2
  , wait_for/1
  , wait_for/2
  , no_events/0
  , flush/0
  ]).
-export(
  [ notify/2
  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec subscribe(spts_games:id(), pid()) -> ok.
subscribe(GameId, TestProcess) ->
  spts_gen_event_handler:subscribe(GameId, ?MODULE, TestProcess).

-spec unsubscribe(spts_games:id(), pid()) -> ok.
unsubscribe(GameId, TestProcess) ->
  spts_gen_event_handler:unsubscribe(GameId, ?MODULE, TestProcess).

-spec wait_for(spts_core:event()) -> ok.
wait_for(Event) -> wait_for(Event, [ignore_others]).

-spec wait_for(spts_core:event(), [ignore_others]) -> ok.
wait_for(Event, Options) ->
  IgnoreOthers = lists:member(ignore_others, Options),
  receive
    {event, Event} -> ok;
    {event, OtherEvent} when IgnoreOthers ->
      ct:pal("Ignored Event: ~p", [OtherEvent]),
      wait_for(Event, Options);
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
-spec notify(pid(), spts_core:event()) -> {event, spts_core:event()}.
notify(Pid, Event) -> Pid ! {event, Event}.

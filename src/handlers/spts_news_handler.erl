%%% @doc /games/:game_id/news handler
-module(spts_news_handler).
-author('elbrujohalcon@inaka.net').

-behavior(lasse_handler).

-export([ init/3
        , handle_notify/2
        , handle_info/2
        , handle_error/3
        , terminate/3
        ]).
-export([ notify/2
        ]).

-type state() :: #{game => spts_games:id()}.

%% @doc sends an event to a listener
-spec notify(pid(), spts_core:event()) -> ok.
notify(Pid, Event) -> lasse_handler:notify(Pid, Event).

-spec init([], _, cowboy_req:req()) ->
  {ok, cowboy_req:req(), [lasse_handler:event()], state()} |
  {shutdown, cowboy:http_status(), cowboy:http_headers(), iodata(),
    cowboy_req:req()}.
init([], _LastEventId, Req) ->
  Req0 = spts_web_utils:announce_req(Req, []),
  {GameId, Req1} = cowboy_req:binding(game_id, Req0),
  case spts_core:is_game(GameId) of
    false ->
      {shutdown, 404, [], [], Req1, #{}};
    true ->
      Game = spts_core:fetch_game(GameId),
      FirstEvent =
        [ {data, spts_json:encode(spts_games:to_json(Game))}
        , {name, <<"game_status">>}
        ],
      ok = spts_news_event_handler:subscribe(GameId, self()),
      {ok, Req1, [FirstEvent], #{game => GameId}}
  end.

-spec handle_notify(spts_core:event(), state()) -> lasse_handler:result().
handle_notify({Type, Serpent}, State) when Type == serpent_added;
                                           Type == collision_detected ->
  Event =
    [ {name, atom_to_binary(Type, utf8)}
    , {data, spts_json:encode(spts_serpents:to_json(Serpent))}
    ],
  {send, Event, State};
handle_notify({Type, Game}, State) when Type == game_countdown;
                                        Type == game_started;
                                        Type == game_updated;
                                        Type == game_finished ->
  Event =
    [ {name, atom_to_binary(Type, utf8)}
    , {data, spts_json:encode(spts_games:to_json(Game))}
    ],
  {send, Event, State};
handle_notify(Event, State) ->
  lager:warning("Ignored Event: ~p on ~p", [Event, State]),
  {nosend, State}.

-spec handle_info(any(), state()) -> lasse_handler:result().
handle_info(Info, State) ->
  lager:notice("~p received at ~p", [Info, State]),
  {nosend, State}.

-spec handle_error(lasse_handler:event(), term(), state()) -> state().
handle_error(Event, Error, State) ->
  lager:warning("Couldn't send ~p in ~p: ~p", [Event, State, Error]),
  State.

-spec terminate(any(), cowboy_req:req(), state()) -> ok.
terminate(Reason, _Req, #{game := GameId}) ->
  lager:notice("News for ~p terminating: ~p", [GameId, Reason]),
  catch spts_news_event_handler:unsubscribe(GameId, self()),
  ok;
terminate(_Reason, _Req, _State) -> ok.

%%% @doc The core of the game
-module(serpents_core).
-author('elbrujohalcon@inaka.net').

-export(
  [ register_player/1
  , create_game/1
  , join_game/2
  , start_game/1
  , turn/3
  , fetch_game/1
  , game_dispatcher/1
  ]).

-type options() :: #{ rows => pos_integer()
                    , cols => pos_integer()
                    }.
-type row() :: pos_integer().
-type col() :: pos_integer().
-type position() :: {row(), col()}.
-type direction() :: left | right | up | down.
-export_type(
  [ options/0
  , position/0
  , direction/0
  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec register_player(serpents_players:name()) -> serpents_players:player().
register_player(Name) ->
  serpents_players_repo:register(Name).

-spec create_game(options()) -> serpents_games:game().
create_game(Options) ->
  serpents_game_sup:start_child(Options).

-spec join_game(serpents_games:id(), serpents_players:id()) -> position().
join_game(GameId, PlayerId) ->
  call(GameId, {join, GameId, PlayerId}).

-spec start_game(serpents_games:id()) -> ok.
start_game(GameId) ->
  cast(GameId, start).

-spec turn(serpents_games:id(), serpents_players:id(), direction()) -> ok.
turn(GameId, PlayerId, Direction) ->
  cast(GameId, {turn, PlayerId, Direction}).

-spec fetch_game(serpents_games:id()) -> serpents_games:game().
fetch_game(GameId) ->
  call(GameId, fetch).

-spec game_dispatcher(serpents_games:id()) -> pid() | atom().
game_dispatcher(GameId) ->
  call(GameId, dispatcher).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
call(GameId, Event) ->
  Process = binary_to_atom(<<?MODULE_STRING, $:, GameId/binary>>, utf8),
  try do_call(Process, Event) of
    ok -> ok;
    {ok, Result} -> Result;
    {error, Error} -> throw(Error)
  catch
    _:Exception ->
      lager:error(
        "Couldn't send ~p to ~p (~p): ~p~nStack: ~p",
        [Event, GameId, Process, Exception, erlang:get_stacktrace()]),
      throw(Exception)
  end.
do_call(Process, fetch) ->
  gen_fsm:sync_send_all_state_event(Process, fetch);
do_call(Process, dispatcher) ->
  gen_fsm:sync_send_all_state_event(Process, dispatcher);
do_call(Process, Event) ->
  gen_fsm:sync_send_event(Process, Event).

cast(GameId, Event) ->
  Process = binary_to_atom(<<?MODULE_STRING, $:, GameId/binary>>, utf8),
  gen_fsm:send_event(Process, Event).

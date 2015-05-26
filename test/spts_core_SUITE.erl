-module(spts_core_SUITE).
-author('elbrujohalcon@inaka.net').

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {spts_test_utils,
         [ init_per_suite/1
         , end_per_suite/1
         ]}
       ]).

-export([ all/0
        , init_per_testcase/2
        , end_per_testcase/2
        ]).
-export([ player_registration/1
        , game_creation_default/1
        , game_creation_with_options/1
        , game_creation_bad_rows/1
        , game_creation_bad_cols/1
        , game_creation_bad_ticktime/1
        , game_creation_bad_countdown/1
        , join_game_ok/1
        , join_game_wrong/1
        , too_many_joins/1
        , start_game/1
        , turn_wrong/1
        , turn_ok/1
        ]).

-spec all() -> [atom()].
all() -> spts_test_utils:all(?MODULE).

-spec init_per_testcase(atom(), spts_test_utils:config()) ->
  spts_test_utils:config().
init_per_testcase(turn_ok, Config0) ->
  Config = init_per_testcase(turn_wrong, Config0),
  {game, Game} = lists:keyfind(game, 1, Config),
  {player1, Player1} = lists:keyfind(player1, 1, Config),
  {player2, Player2} = lists:keyfind(player2, 1, Config),
  GameId = spts_games:id(Game),
  Player1Id = spts_players:id(Player1),
  Player2Id = spts_players:id(Player2),
  {_, _} = spts_core:join_game(GameId, Player1Id),
  {_, _} = spts_core:join_game(GameId, Player2Id),
  Config;
init_per_testcase(JoinGameTest, Config)
  when JoinGameTest == join_game_ok
     ; JoinGameTest == join_game_wrong
     ; JoinGameTest == start_game
     ; JoinGameTest == turn_wrong ->
  Game = spts_core:create_game(#{cols => 10, rows => 10, countdown => 2}),
  Player1 = spts_core:register_player(<<"1">>),
  Player2 = spts_core:register_player(<<"2">>),
  Player3 = spts_core:register_player(<<"3">>),
  [ {player1, Player1}
  , {player2, Player2}
  , {player3, Player3}
  , {game, Game}
  | Config];
init_per_testcase(_Test, Config) -> Config.

-spec end_per_testcase(atom(), spts_test_utils:config()) ->
  spts_test_utils:config().
end_per_testcase(JoinGameTest, Config)
  when JoinGameTest == join_game_ok
     ; JoinGameTest == join_game_wrong
     ; JoinGameTest == start_game
     ; JoinGameTest == turn_wrong ->
  {game, Game} = lists:keyfind(game, 1, Config),
  GameId = spts_games:id(Game),
  ok = spts_core:stop_game(GameId),
  lists:filter(
    fun ({K, _}) -> not lists:member(K, [game, player1, player2, player3]) end,
    Config);
end_per_testcase(_Test, Config) -> Config.

-spec player_registration(spts_test_utils:config()) -> {comment, string()}.
player_registration(_Config) ->
  ct:comment("A player is created"),
  Player1 = spts_core:register_player(<<"name 1">>),
  <<"name 1">> = spts_players:name(Player1),
  Id1 = spts_players:id(Player1),

  ct:comment("Another player is created"),
  Player2 = spts_core:register_player(<<"name 2">>),
  <<"name 2">> = spts_players:name(Player2),
  case spts_players:id(Player2) of
    Id1 -> ct:fail("Duplicated id: ~p", [Id1]);
    _ -> ok
  end,

  ct:comment("Even with the same name, no validations"),
  Player3 = spts_core:register_player(<<"name 3">>),
  <<"name 3">> = spts_players:name(Player3),
  case spts_players:id(Player3) of
    Id1 -> ct:fail("Duplicated id: ~p", [Id1]);
    _ -> ok
  end,

  {comment, ""}.

-spec game_creation_default(spts_test_utils:config()) ->
  {comment, string()}.
game_creation_default(_Config) ->
  ct:comment("Create a game with default options"),
  Game = spts_core:create_game(),
  20 = spts_games:rows(Game),
  20 = spts_games:cols(Game),
  250 = spts_games:ticktime(Game),
  10 = spts_games:countdown(Game),
  created = spts_games:state(Game),
  [] = spts_games:serpents(Game),
  {comment, ""}.

-spec game_creation_with_options(spts_test_utils:config()) ->
  {comment, string()}.
game_creation_with_options(_Config) ->
  ct:comment("Create a game with more rows"),
  Game0 = spts_core:create_game(#{rows => 40}),
  40 = spts_games:rows(Game0),
  20 = spts_games:cols(Game0),
  250 = spts_games:ticktime(Game0),
  10 = spts_games:countdown(Game0),

  ct:comment("Create a game with more cols"),
  Game1 = spts_core:create_game(#{cols => 30}),
  20 = spts_games:rows(Game1),
  30 = spts_games:cols(Game1),
  250 = spts_games:ticktime(Game1),
  10 = spts_games:countdown(Game1),

  ct:comment("Create a game with more ticktime"),
  Game2 = spts_core:create_game(#{ticktime => 300}),
  20 = spts_games:rows(Game2),
  20 = spts_games:cols(Game2),
  300 = spts_games:ticktime(Game2),
  10 = spts_games:countdown(Game2),

  ct:comment("Create a game with less cols and rows"),
  Game3 = spts_core:create_game(#{cols => 10, rows => 10}),
  10 = spts_games:rows(Game3),
  10 = spts_games:cols(Game3),
  250 = spts_games:ticktime(Game3),
  10 = spts_games:countdown(Game3),

  Game4 = spts_core:create_game(#{countdown => 25}),
  20 = spts_games:rows(Game4),
  20 = spts_games:cols(Game4),
  250 = spts_games:ticktime(Game4),
  25 = spts_games:countdown(Game4),

  Game5 = spts_core:create_game(#{ticktime => 400, countdown => 25}),
  20 = spts_games:rows(Game5),
  20 = spts_games:cols(Game5),
  400 = spts_games:ticktime(Game5),
  25 = spts_games:countdown(Game5),

  ct:comment("Create a game with less cols, rows and ticktime"),
  GameF =
    spts_core:create_game(
      #{cols => 10, rows => 10, ticktime => 500, countdown => 0}),
  10 = spts_games:rows(GameF),
  10 = spts_games:cols(GameF),
  500 = spts_games:ticktime(GameF),
  0 = spts_games:countdown(GameF),

  {comment, ""}.

-spec game_creation_bad_rows(spts_test_utils:config()) ->
  {comment, string()}.
game_creation_bad_rows(_Config) ->
  TryWith =
    fun(R) ->
      try spts_core:create_game(#{rows => R}) of
        G -> ct:fail("Unexpected game with ~p rows: ~p", [R, G])
      catch
        throw:invalid_rows -> ok
      end
    end,

  ct:comment("Negative rows fails"),
  TryWith(-10),

  ct:comment("0 rows fails"),
  TryWith(0),

  ct:comment("Less than 5 rows fails"),
  TryWith(4),

  {comment, ""}.

-spec game_creation_bad_cols(spts_test_utils:config()) -> {comment, string()}.
game_creation_bad_cols(_Config) ->
  TryWith =
    fun(R) ->
      try spts_core:create_game(#{cols => R}) of
        G -> ct:fail("Unexpected game with ~p cols: ~p", [R, G])
      catch
        throw:invalid_cols -> ok
      end
    end,

  ct:comment("Negative cols fails"),
  TryWith(-10),

  ct:comment("0 cols fails"),
  TryWith(0),

  ct:comment("Less than 5 cols fails"),
  TryWith(4),

  {comment, ""}.

-spec game_creation_bad_ticktime(spts_test_utils:config()) ->
  {comment, string()}.
game_creation_bad_ticktime(_Config) ->
  TryWith =
    fun(T) ->
      try spts_core:create_game(#{ticktime => T}) of
        G -> ct:fail("Unexpected game with ticktime == ~p: ~p", [T, G])
      catch
        throw:invalid_ticktime -> ok
      end
    end,

  ct:comment("Negative ticktime fails"),
  TryWith(-10),

  ct:comment("0 ticktime fails"),
  TryWith(0),

  ct:comment("Less than 100 ticktime fails"),
  TryWith(4),

  {comment, ""}.

-spec game_creation_bad_countdown(spts_test_utils:config()) ->
  {comment, string()}.
game_creation_bad_countdown(_Config) ->
  ct:comment("Negative countdown fails"),
  try spts_core:create_game(#{countdown => -15}) of
    G -> ct:fail("Unexpected game with negative countdown: ~p", [G])
  catch
    throw:invalid_countdown -> ok
  end,

  {comment, ""}.

-spec join_game_ok(spts_test_utils:config()) -> {comment, string()}.
join_game_ok(Config) ->
  {game, Game} = lists:keyfind(game, 1, Config),
  {player1, Player1} = lists:keyfind(player1, 1, Config),
  {player2, Player2} = lists:keyfind(player2, 1, Config),
  {player3, Player3} = lists:keyfind(player3, 1, Config),
  GameId = spts_games:id(Game),
  Player1Id = spts_players:id(Player1),
  Player2Id = spts_players:id(Player2),
  Player3Id = spts_players:id(Player3),

  ct:comment("There is noone on the game"),
  [] = spts_games:serpents(spts_core:fetch_game(GameId)),

  ct:comment("Player1 joins"),
  {P1, D1} = spts_core:join_game(GameId, Player1Id),
  {Row1, Col1} = P1,
  true = lists:member(D1, [up, down, left, right]),
  true = Row1 > 0,
  true = Row1 < 11,
  true = Col1 > 0,
  true = Col1 < 11,
  [Serpent1] = spts_games:serpents(spts_core:fetch_game(GameId)),
  Player1Id = spts_serpents:owner(Serpent1),

  ct:comment("Player2 joins"),
  {P2, D2} = spts_core:join_game(GameId, Player2Id),
  {Row2, Col2} = P2,
  true = lists:member(D2, [up, down, left, right]),
  true = Row2 > 0,
  true = Row2 < 11,
  true = Col2 > 0,
  true = Col2 < 11,
  case P2 of
    P1 -> ct:fail("Duplicated position: ~p", [P1]);
    P2 -> ok
  end,
  [Serpent2] = spts_games:serpents(spts_core:fetch_game(GameId)) -- [Serpent1],
  Player2Id = spts_serpents:owner(Serpent2),

  ct:comment("Player3 joins"),
  {P3, D3} = spts_core:join_game(GameId, Player3Id),
  {Row3, Col3} = P3,
  true = lists:member(D3, [up, down, left, right]),
  true = Row3 > 0,
  true = Row3 < 11,
  true = Col3 > 0,
  true = Col3 < 11,
  case P3 of
    P1 -> ct:fail("Duplicated position: ~p", [P1]);
    P2 -> ct:fail("Duplicated position: ~p", [P2]);
    P3 -> ok
  end,
  [Serpent3] =
    spts_games:serpents(spts_core:fetch_game(GameId)) --
      [Serpent1, Serpent2],
  Player3Id = spts_serpents:owner(Serpent3),

  {comment, ""}.

-spec join_game_wrong(spts_test_utils:config()) -> {comment, string()}.
join_game_wrong(Config) ->
  {game, Game} = lists:keyfind(game, 1, Config),
  {player1, Player1} = lists:keyfind(player1, 1, Config),
  {player2, Player2} = lists:keyfind(player2, 1, Config),
  GameId = spts_games:id(Game),
  Player1Id = spts_players:id(Player1),
  Player2Id = spts_players:id(Player2),

  ct:comment("There is noone on the game"),
  [] = spts_games:serpents(spts_core:fetch_game(GameId)),

  ct:comment("Player1 joins"),
  spts_core:join_game(GameId, Player1Id),
  [Serpent1] = spts_games:serpents(spts_core:fetch_game(GameId)),
  Player1Id = spts_serpents:owner(Serpent1),

  ct:comment("Player1 tries to join the game again"),
  try spts_core:join_game(GameId, Player1Id) of
    R2 -> ct:fail("Duplicated join ~p in ~p (~p)", [Player1Id, GameId, R2])
  catch
    throw:already_joined -> ok
  end,
  [Serpent1] = spts_games:serpents(spts_core:fetch_game(GameId)),

  ct:comment("invalid player tries to join the game"),
  try spts_core:join_game(GameId, <<"not-a-real-player">>) of
    R3 -> ct:fail("Unexpected join in ~p: ~p", [GameId, R3])
  catch
    throw:invalid_player -> ok
  end,
  [Serpent1] = spts_games:serpents(spts_core:fetch_game(GameId)),

  ct:comment("Player2 can't join while on countdown"),
  ok = spts_core:start_game(GameId),
  ktn_task:wait_for(
    fun() ->
      spts_games:state(spts_core:fetch_game(GameId))
    end, countdown),
  try spts_core:join_game(GameId, Player2Id) of
    R4 -> ct:fail("Unexpected join in ~p: ~p", [GameId, R4])
  catch
    throw:invalid_state -> ok
  end,
  [Serpent1] = spts_games:serpents(spts_core:fetch_game(GameId)),

  ct:comment("Player2 can't join after game starts"),
  ktn_task:wait_for(
    fun() ->
      spts_games:state(spts_core:fetch_game(GameId))
    end, started),
  try spts_core:join_game(GameId, Player2Id) of
    R5 -> ct:fail("Unexpected join in ~p: ~p", [GameId, R5])
  catch
    throw:invalid_state -> ok
  end,
  [Serpent1] = spts_games:serpents(spts_core:fetch_game(GameId)),

  {comment, ""}.

-spec too_many_joins(spts_test_utils:config()) -> {comment, string()}.
too_many_joins(_Config) ->
  ct:comment("A small game is created"),
  Game = spts_core:create_game(#{cols => 5, rows => 5}),
  GameId = spts_games:id(Game),

  ct:comment("Enough players to fill the whole board are registered"),
  Chars = lists:seq($a, $a + 24),
  Players = [spts_players:id(spts_core:register_player(<<C>>)) || C <- Chars],

  ct:comment("They all try to join the game, at least one must fail"),
  JoinResults =
    lists:map(
      fun(PlayerId) ->
        try spts_core:join_game(GameId, PlayerId)
        catch
          throw:game_full -> game_full
        end
      end, Players),

  {[_|_], PlayersInGame} =
    lists:partition(fun(X) -> X == game_full end, JoinResults),

  ct:comment("All players should be free to move"),
  NewGame = spts_core:fetch_game(GameId),
  lists:foreach(
    fun({Position, Direction}) ->
      NewPosition = spts_test_utils:move(Position, Direction),
      true = spts_games:is_empty(NewGame, NewPosition),
      in = spts_test_utils:check_bounds(NewGame, NewPosition)
    end, PlayersInGame),

  {comment, ""}.

-spec start_game(spts_test_utils:config()) -> {comment, string()}.
start_game(Config) ->
  {game, Game} = lists:keyfind(game, 1, Config),
  {player1, Player1} = lists:keyfind(player1, 1, Config),
  GameId = spts_games:id(Game),
  Player1Id = spts_players:id(Player1),

  ct:comment("The game can't start if there is noone in it"),
  [] = spts_games:serpents(spts_core:fetch_game(GameId)),
  ok = spts_core:start_game(GameId),
  created = spts_games:state(spts_core:fetch_game(GameId)),

  ct:comment("The game can start after the first join"),
  spts_core:join_game(GameId, Player1Id),
  ok = spts_core:start_game(GameId),
  countdown = spts_games:state(spts_core:fetch_game(GameId)),

  ct:comment("Trying to start the game again has no effect"),
  ok = spts_core:start_game(GameId),
  countdown = spts_games:state(spts_core:fetch_game(GameId)),

  ct:comment("We wait for the game to actually start"),
  ktn_task:wait_for(
    fun() ->
      spts_games:state(spts_core:fetch_game(GameId))
    end, started),

  ct:comment("Trying to start the game again has no effect"),
  ok = spts_core:start_game(GameId),
  started = spts_games:state(spts_core:fetch_game(GameId)),

  {comment, ""}.

-spec turn_wrong(spts_test_utils:config()) -> {comment, string()}.
turn_wrong(Config) ->
  {game, Game} = lists:keyfind(game, 1, Config),
  {player1, Player1} = lists:keyfind(player1, 1, Config),
  {player2, Player2} = lists:keyfind(player2, 1, Config),
  GameId = spts_games:id(Game),
  Player1Id = spts_players:id(Player1),
  Player2Id = spts_players:id(Player2),

  ct:comment("Turn is inconsequential if the player hasn't joined"),
  [] = spts_games:serpents(spts_core:fetch_game(GameId)),
  ok = spts_core:turn(GameId, Player1Id, right),
  [] = spts_games:serpents(spts_core:fetch_game(GameId)),

  ct:comment("A player that's outside the game can't turn his snake"),
  {_, D1} = spts_core:join_game(GameId, Player1Id),
  ok = spts_core:turn(GameId, Player2Id, right),
  D1 =
    spts_serpents:direction(
      spts_games:serpent(
        spts_core:fetch_game(GameId), Player1Id)),

  {comment, ""}.

-spec turn_ok(spts_test_utils:config()) -> {comment, string()}.
turn_ok(Config) ->
  {game, Game} = lists:keyfind(game, 1, Config),
  {player1, Player1} = lists:keyfind(player1, 1, Config),
  {player2, Player2} = lists:keyfind(player2, 1, Config),
  GameId = spts_games:id(Game),
  Player1Id = spts_players:id(Player1),
  Player2Id = spts_players:id(Player2),

  TryWith =
    fun(PlayerId, Direction) ->
      spts_core:turn(GameId, PlayerId, Direction),
      { spts_serpents:direction(
          spts_games:serpent(
            spts_core:fetch_game(GameId), Player1Id))
      , spts_serpents:direction(
          spts_games:serpent(
            spts_core:fetch_game(GameId), Player2Id))
      }
    end,

  FullRound =
    fun() ->
      {up, _} = TryWith(Player1Id, up),
      {up, up} = TryWith(Player2Id, up),
      {down, up} = TryWith(Player1Id, down),
      {down, down} = TryWith(Player2Id, down),
      {down, left} = TryWith(Player2Id, left),
      {left, left} = TryWith(Player1Id, left),
      {left, right} = TryWith(Player2Id, right),
      {right, right} = TryWith(Player1Id, right)
    end,

  ct:comment("Before the game starts, players can turn"),
  FullRound(),

  ct:comment("After the game starts, players can turn"),
  spts_core:start_game(GameId),
  FullRound(),

  {comment, ""}.

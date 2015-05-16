-module(serpents_core_SUITE).
-author('elbrujohalcon@inaka.net').

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {serpents_test_utils,
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
        , join_game_ok/1
        , join_game_wrong/1
        , too_many_joins/1
        , start_game/1
        , turn_wrong/1
        , turn_ok/1
        ]).

-spec all() -> [atom()].
all() -> serpents_test_utils:all(?MODULE).

-spec init_per_testcase(atom(), serpents_test_utils:config()) ->
  serpents_test_utils:config().
init_per_testcase(turn_ok, Config0) ->
  Config = init_per_testcase(turn_wrong, Config0),
  {game, Game} = lists:keyfind(game, 1, Config),
  {player1, Player1} = lists:keyfind(player1, 1, Config),
  {player2, Player2} = lists:keyfind(player2, 1, Config),
  GameId = serpents_games:id(Game),
  Player1Id = serpents_players:id(Player1),
  Player2Id = serpents_players:id(Player2),
  {_, _} = serpents_core:join_game(GameId, Player1Id),
  {_, _} = serpents_core:join_game(GameId, Player2Id),
  Config;
init_per_testcase(JoinGameTest, Config)
  when JoinGameTest == join_game_ok
     ; JoinGameTest == join_game_wrong
     ; JoinGameTest == start_game
     ; JoinGameTest == turn_wrong ->
  Game = serpents_core:create_game(#{cols => 10, rows => 10}),
  Player1 = serpents_core:register_player(<<"1">>),
  Player2 = serpents_core:register_player(<<"2">>),
  Player3 = serpents_core:register_player(<<"3">>),
  [ {player1, Player1}
  , {player2, Player2}
  , {player3, Player3}
  , {game, Game}
  | Config];
init_per_testcase(_Test, Config) -> Config.

-spec end_per_testcase(atom(), serpents_test_utils:config()) ->
  serpents_test_utils:config().
end_per_testcase(JoinGameTest, Config)
  when JoinGameTest == join_game_ok
     ; JoinGameTest == join_game_wrong
     ; JoinGameTest == start_game
     ; JoinGameTest == turn_wrong ->
  lists:filter(
    fun ({K, _}) -> not lists:member(K, [game, player1, player2, player3]) end,
    Config);
end_per_testcase(_Test, Config) -> Config.

-spec player_registration(serpents_test_utils:config()) -> {comment, string()}.
player_registration(_Config) ->
  ct:comment("A player is created"),
  Player1 = serpents_core:register_player(<<"name 1">>),
  <<"name 1">> = serpents_players:name(Player1),
  Id1 = serpents_players:id(Player1),

  ct:comment("Another player is created"),
  Player2 = serpents_core:register_player(<<"name 2">>),
  <<"name 2">> = serpents_players:name(Player2),
  case serpents_players:id(Player2) of
    Id1 -> ct:fail("Duplicated id: ~p", [Id1]);
    _ -> ok
  end,

  ct:comment("Even with the same name, no validations"),
  Player3 = serpents_core:register_player(<<"name 3">>),
  <<"name 3">> = serpents_players:name(Player3),
  case serpents_players:id(Player3) of
    Id1 -> ct:fail("Duplicated id: ~p", [Id1]);
    _ -> ok
  end,

  {comment, ""}.

-spec game_creation_default(serpents_test_utils:config()) ->
  {comment, string()}.
game_creation_default(_Config) ->
  ct:comment("Create a game with default options"),
  Game = serpents_core:create_game(),
  20 = serpents_games:rows(Game),
  20 = serpents_games:cols(Game),
  250 = serpents_games:ticktime(Game),
  created = serpents_games:state(Game),
  [] = serpents_games:serpents(Game),
  {comment, ""}.

-spec game_creation_with_options(serpents_test_utils:config()) ->
  {comment, string()}.
game_creation_with_options(_Config) ->
  ct:comment("Create a game with more rows"),
  Game0 = serpents_core:create_game(#{rows => 40}),
  40 = serpents_games:rows(Game0),
  20 = serpents_games:cols(Game0),
  250 = serpents_games:ticktime(Game0),

  ct:comment("Create a game with more cols"),
  Game1 = serpents_core:create_game(#{cols => 30}),
  20 = serpents_games:rows(Game1),
  30 = serpents_games:cols(Game1),
  250 = serpents_games:ticktime(Game1),

  ct:comment("Create a game with more ticktime"),
  Game2 = serpents_core:create_game(#{ticktime => 300}),
  20 = serpents_games:rows(Game2),
  20 = serpents_games:cols(Game2),
  300 = serpents_games:ticktime(Game2),

  ct:comment("Create a game with less cols and rows"),
  Game3 = serpents_core:create_game(#{cols => 10, rows => 10}),
  10 = serpents_games:rows(Game3),
  10 = serpents_games:cols(Game3),
  250 = serpents_games:ticktime(Game3),

  ct:comment("Create a game with less cols, rows and ticktime"),
  Game4 = serpents_core:create_game(#{cols => 10, rows => 10, ticktime => 500}),
  10 = serpents_games:rows(Game4),
  10 = serpents_games:cols(Game4),
  500 = serpents_games:ticktime(Game4),

  {comment, ""}.

-spec game_creation_bad_rows(serpents_test_utils:config()) ->
  {comment, string()}.
game_creation_bad_rows(_Config) ->
  TryWith =
    fun(R) ->
      try serpents_core:create_game(#{rows => R}) of
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

-spec game_creation_bad_cols(serpents_test_utils:config()) ->
  {comment, string()}.
game_creation_bad_cols(_Config) ->
  TryWith =
    fun(R) ->
      try serpents_core:create_game(#{cols => R}) of
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

-spec game_creation_bad_ticktime(serpents_test_utils:config()) ->
  {comment, string()}.
game_creation_bad_ticktime(_Config) ->
  TryWith =
    fun(T) ->
      try serpents_core:create_game(#{ticktime => T}) of
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

-spec join_game_ok(serpents_test_utils:config()) ->
  {comment, string()}.
join_game_ok(Config) ->
  {game, Game} = lists:keyfind(game, 1, Config),
  {player1, Player1} = lists:keyfind(player1, 1, Config),
  {player2, Player2} = lists:keyfind(player2, 1, Config),
  {player3, Player3} = lists:keyfind(player3, 1, Config),
  GameId = serpents_games:id(Game),
  Player1Id = serpents_players:id(Player1),
  Player2Id = serpents_players:id(Player2),
  Player3Id = serpents_players:id(Player3),

  ct:comment("There is noone on the game"),
  [] = serpents_games:serpents(serpents_core:fetch_game(GameId)),

  ct:comment("Player1 joins"),
  {P1, D1} = serpents_core:join_game(GameId, Player1Id),
  {Row1, Col1} = P1,
  true = lists:member(D1, [up, down, left, right]),
  true = Row1 > 0,
  true = Row1 < 11,
  true = Col1 > 0,
  true = Col1 < 11,
  [Serpent1] = serpents_games:serpents(serpents_core:fetch_game(GameId)),
  Player1Id = serpents_serpents:owner(Serpent1),

  ct:comment("Player2 joins"),
  {P2, D2} = serpents_core:join_game(GameId, Player2Id),
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
  [Serpent2] =
    serpents_games:serpents(serpents_core:fetch_game(GameId)) -- [Serpent1],
  Player2Id = serpents_serpents:owner(Serpent2),

  ct:comment("Player3 joins"),
  {P3, D3} = serpents_core:join_game(GameId, Player3Id),
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
    serpents_games:serpents(serpents_core:fetch_game(GameId)) --
      [Serpent1, Serpent2],
  Player3Id = serpents_serpents:owner(Serpent3),

  {comment, ""}.

-spec join_game_wrong(serpents_test_utils:config()) ->
  {comment, string()}.
join_game_wrong(Config) ->
  {game, Game} = lists:keyfind(game, 1, Config),
  {player1, Player1} = lists:keyfind(player1, 1, Config),
  {player2, Player2} = lists:keyfind(player2, 1, Config),
  GameId = serpents_games:id(Game),
  Player1Id = serpents_players:id(Player1),
  Player2Id = serpents_players:id(Player2),

  ct:comment("There is noone on the game"),
  [] = serpents_games:serpents(serpents_core:fetch_game(GameId)),

  ct:comment("Player1 joins"),
  serpents_core:join_game(GameId, Player1Id),
  [Serpent1] = serpents_games:serpents(serpents_core:fetch_game(GameId)),
  Player1Id = serpents_serpents:owner(Serpent1),

  ct:comment("Player1 tries to join the game again"),
  try serpents_core:join_game(GameId, Player1Id) of
    R2 -> ct:fail("Duplicated join ~p in ~p (~p)", [Player1Id, GameId, R2])
  catch
    throw:already_joined -> ok
  end,
  [Serpent1] = serpents_games:serpents(serpents_core:fetch_game(GameId)),

  ct:comment("invalid player tries to join the game"),
  try serpents_core:join_game(GameId, <<"not-a-real-player">>) of
    R3 -> ct:fail("Unexpected join in ~p: ~p", [GameId, R3])
  catch
    throw:invalid_player -> ok
  end,
  [Serpent1] = serpents_games:serpents(serpents_core:fetch_game(GameId)),

  ct:comment("Player2 can't join after game starts"),
  ok = serpents_core:start_game(GameId),
  ktn_task:wait_for(
    fun() ->
      serpents_games:state(serpents_core:fetch_game(GameId))
    end, started),
  try serpents_core:join_game(GameId, Player2Id) of
    R4 -> ct:fail("Unexpected join in ~p: ~p", [GameId, R4])
  catch
    throw:invalid_state -> ok
  end,
  [Serpent1] = serpents_games:serpents(serpents_core:fetch_game(GameId)),

  {comment, ""}.

-spec too_many_joins(serpents_test_utils:config()) ->
  {comment, string()}.
too_many_joins(_Config) ->
  ct:comment("A small game is created"),
  Game = serpents_core:create_game(#{cols => 5, rows => 5}),
  GameId = serpents_games:id(Game),

  ct:comment("Enough players to fill the whole board are registered"),
  Chars = lists:seq($a, $a + 24),
  Players =
    [serpents_players:id(serpents_core:register_player(<<C>>)) || C <- Chars],

  ct:comment("They all join the game"),
  lists:foreach(
    fun(PlayerId) ->
      serpents_core:join_game(GameId, PlayerId)
    end, Players),

  ct:comment("Another player tries to join the game but it's rejected"),
  Homero = serpents_players:id(serpents_core:register_player(<<"homero">>)),
  try serpents_core:join_game(GameId, Homero) of
    Result -> ct:fail("Unexpected join in ~p: ~p", [GameId, Result])
  catch
    throw:game_full -> ok
  end,

  {comment, ""}.

-spec start_game(serpents_test_utils:config()) ->
  {comment, string()}.
start_game(Config) ->
  {game, Game} = lists:keyfind(game, 1, Config),
  {player1, Player1} = lists:keyfind(player1, 1, Config),
  GameId = serpents_games:id(Game),
  Player1Id = serpents_players:id(Player1),

  ct:comment("The game can't start if there is noone in it"),
  [] = serpents_games:serpents(serpents_core:fetch_game(GameId)),
  ok = serpents_core:start_game(GameId),
  created = serpents_games:state(serpents_core:fetch_game(GameId)),

  ct:comment("The game can start after the first join"),
  serpents_core:join_game(GameId, Player1Id),
  ok = serpents_core:start_game(GameId),
  started = serpents_games:state(serpents_core:fetch_game(GameId)),

  ct:comment("Trying to start the game again has no effect"),
  ok = serpents_core:start_game(GameId),
  started = serpents_games:state(serpents_core:fetch_game(GameId)),

  {comment, ""}.

-spec turn_wrong(serpents_test_utils:config()) ->
  {comment, string()}.
turn_wrong(Config) ->
  {game, Game} = lists:keyfind(game, 1, Config),
  {player1, Player1} = lists:keyfind(player1, 1, Config),
  {player2, Player2} = lists:keyfind(player2, 1, Config),
  GameId = serpents_games:id(Game),
  Player1Id = serpents_players:id(Player1),
  Player2Id = serpents_players:id(Player2),

  ct:comment("Turn is inconsequential if the game hasn't started"),
  [] = serpents_games:serpents(serpents_core:fetch_game(GameId)),
  ok = serpents_core:turn(GameId, Player1Id, right),
  [] = serpents_games:serpents(serpents_core:fetch_game(GameId)),

  ct:comment("A player that's outside the game can't turn his snake"),
  {_, D1} = serpents_core:join_game(GameId, Player1Id),
  ok = serpents_core:turn(GameId, Player2Id, right),
  D1 =
    serpents_serpents:direction(
      serpents_games:serpent(
        serpents_core:fetch_game(GameId), Player1Id)),

  {comment, ""}.

turn_ok(Config) ->
  {game, Game} = lists:keyfind(game, 1, Config),
  {player1, Player1} = lists:keyfind(player1, 1, Config),
  {player2, Player2} = lists:keyfind(player2, 1, Config),
  GameId = serpents_games:id(Game),
  Player1Id = serpents_players:id(Player1),
  Player2Id = serpents_players:id(Player2),

  TryWith =
    fun(PlayerId, Direction) ->
      serpents_core:turn(GameId, PlayerId, Direction),
      { serpents_serpents:direction(
          serpents_games:serpent(
            serpents_core:fetch_game(GameId), Player1Id))
      , serpents_serpents:direction(
          serpents_games:serpent(
            serpents_core:fetch_game(GameId), Player2Id))
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
  serpents_core:start_game(GameId),
  FullRound(),

  {comment, ""}.
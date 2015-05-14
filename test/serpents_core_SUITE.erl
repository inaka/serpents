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
        , join_game_ok/1
        , join_game_wrong/1
        ]).

-spec all() -> [atom()].
all() -> serpents_test_utils:all(?MODULE).

-spec init_per_testcase(atom(), serpents_test_utils:config()) ->
  serpents_test_utils:config().
init_per_testcase(JoinGameTest, Config)
  when JoinGameTest == join_game_ok; JoinGameTest == join_game_wrong ->
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
  when JoinGameTest == join_game_ok; JoinGameTest == join_game_wrong ->
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
  created = serpents_games:state(Game),
  [] = serpents_games:players(Game),
  {comment, ""}.

-spec game_creation_with_options(serpents_test_utils:config()) ->
  {comment, string()}.
game_creation_with_options(_Config) ->
  ct:comment("Create a game with more rows"),
  Game0 = serpents_core:create_game(#{rows => 40}),
  40 = serpents_games:rows(Game0),
  20 = serpents_games:cols(Game0),

  ct:comment("Create a game with more cols"),
  Game1 = serpents_core:create_game(#{cols => 30}),
  20 = serpents_games:rows(Game1),
  30 = serpents_games:cols(Game1),

  ct:comment("Create a game with less cols and rows"),
  Game2 = serpents_core:create_game(#{cols => 10, rows => 10}),
  10 = serpents_games:rows(Game2),
  10 = serpents_games:cols(Game2),

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
  [] = serpents_games:players(serpents_core:fetch_game(GameId)),

  ct:comment("Player1 joins"),
  P1 = {Row1, Col1} = serpents_core:join_game(GameId, Player1Id),
  true = Row1 > 0,
  true = Row1 < 11,
  true = Col1 > 0,
  true = Col1 < 11,
  [Player1Id] = serpents_games:players(serpents_core:fetch_game(GameId)),

  ct:comment("Player2 joins"),
  P2 = {Row2, Col2} = serpents_core:join_game(GameId, Player2Id),
  true = Row2 > 0,
  true = Row2 < 11,
  true = Col2 > 0,
  true = Col2 < 11,
  case P2 of
    P1 -> ct:fail("Duplicated position: ~p", [P1]);
    P2 -> ok
  end,
  [] =
    [Player1Id, Player2Id] --
      serpents_games:players(serpents_core:fetch_game(GameId)),

  ct:comment("Player3 joins"),
  P3 = {Row3, Col3} = serpents_core:join_game(GameId, Player3Id),
  true = Row3 > 0,
  true = Row3 < 11,
  true = Col3 > 0,
  true = Col3 < 11,
  case P3 of
    P1 -> ct:fail("Duplicated position: ~p", [P1]);
    P2 -> ct:fail("Duplicated position: ~p", [P2]);
    P3 -> ok
  end,
  [] =
    [Player1Id, Player2Id, Player3Id] --
      serpents_games:players(serpents_core:fetch_game(GameId)),

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
  [] = serpents_games:players(serpents_core:fetch_game(GameId)),

  ct:comment("Player1 joins"),
  serpents_core:join_game(GameId, Player1Id),
  [Player1Id] = serpents_games:players(serpents_core:fetch_game(GameId)),

  ct:comment("Player1 tries to join the game again"),
  try serpents_core:join_game(GameId, Player1Id) of
    P2 -> ct:fail("Duplicated join ~p in ~p (~p)", [Player1Id, GameId, P2])
  catch
    throw:already_joined -> ok
  end,
  [Player1Id] = serpents_games:players(serpents_core:fetch_game(GameId)),

  ct:comment("invalid player tries to join the game"),
  try serpents_core:join_game(GameId, <<"not-a-real-player">>) of
    P3 -> ct:fail("Unexpected join in ~p: ~p", [GameId, P3])
  catch
    throw:invalid_player -> ok
  end,
  [Player1Id] = serpents_games:players(serpents_core:fetch_game(GameId)),

  ct:comment("Player2 can't join after game starts"),
  ok = serpents_core:start_game(GameId),
  ktn_task:wait_for(
    fun() ->
      serpents_games:state(serpents_core:fetch_game(GameId))
    end, started),
  try serpents_core:join_game(GameId, Player2Id) of
    P4 -> ct:fail("Unexpected join in ~p: ~p", [GameId, P4])
  catch
    throw:invalid_state -> ok
  end,
  [Player1Id] = serpents_games:players(serpents_core:fetch_game(GameId)),

  {comment, ""}.

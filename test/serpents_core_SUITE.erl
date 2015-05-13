-module(serpents_core_SUITE).
-author('elbrujohalcon@inaka.net').

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {serpents_test_utils,
         [ init_per_suite/1
         , end_per_suite/1
         ]}
       ]).

-export([all/0]).
-export([ player_registration/1
        , game_creation_default/1
        , game_creation_with_options/1
        , game_creation_bad_rows/1
        , game_creation_bad_cols/1
        ]).

-spec all() -> [atom()].
all() -> serpents_test_utils:all(?MODULE).

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

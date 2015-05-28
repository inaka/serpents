-module(spts_live_SUITE).
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
-export([ serpent_movement/1
        , collision_detection_left_wall/1
        , collision_detection_right_wall/1
        , collision_detection_up_wall/1
        , collision_detection_down_wall/1
        , collision_with_serpent_body/1
        , collision_with_serpent_head/1
        , always_a_fruit/1
        , fruit_reapears/1
        , fruit_feeds/1
        , countdown/1
        , no_countdown/1
        , timeout/1
        ]).

-spec all() -> [atom()].
all() -> spts_test_utils:all(?MODULE).

-spec init_per_testcase(atom(), spts_test_utils:config()) ->
  spts_test_utils:config().
init_per_testcase(timeout, Config) ->
  GameId =
    spts_games:id(
      spts_core:create_game(
        #{ cols => 50
         , rows => 50
         , ticktime => 1000
         , countdown => 2
         , timeout => 10000
         })),
  Serpent = spts_core:add_serpent(GameId, <<"serp1">>),
  Direction =
    case spts_serpents:body(Serpent) of
      [{Row, _Col}] when Row < 25 -> down;
      [{Row, _Col}] when Row >= 25 -> up
    end,
  ok = spts_core:turn(GameId, <<"serp1">>, Direction),
  [{game, GameId} | Config];
init_per_testcase(serpent_movement, Config) ->
  GameId =
    spts_games:id(
      spts_core:create_game(
        #{cols => 5, rows => 5, ticktime => 600000, countdown => 0})),
  spts_core:add_serpent(GameId, <<"serp1">>),
  spts_core:add_serpent(GameId, <<"serp2">>),
  spts_core:add_serpent(GameId, <<"serp3">>),
  [{game, GameId} | Config];
init_per_testcase(Test, Config) when Test == collision_with_serpent_body;
                                     Test == collision_with_serpent_head;
                                     Test == fruit_reapears;
                                     Test == fruit_feeds ->
  Game = spts_games_repo:create(#{cols => 5, rows => 5}),
  [{game, Game} | Config];
init_per_testcase(countdown, Config) ->
  GameId =
    spts_games:id(
      spts_core:create_game(
        #{cols => 5, rows => 5, ticktime => 600000, countdown => 5})),
  spts_core:add_serpent(GameId, <<"serp1">>),
  [{game, GameId} | Config];
init_per_testcase(_Test, Config) ->
  GameId =
    spts_games:id(
      spts_core:create_game(
        #{cols => 5, rows => 5, ticktime => 600000, countdown => 0})),
  spts_core:add_serpent(GameId, <<"serp1">>),
  [{game, GameId} | Config].

-spec end_per_testcase(atom(), spts_test_utils:config()) ->
  spts_test_utils:config().
end_per_testcase(_Test, Config) ->
  case lists:keyfind(game, 1, Config) of
    {game, GameId} when is_binary(GameId) ->
      ok = spts_core:stop_game(GameId);
    _ -> ok
  end,
  lists:keydelete(game, 1, Config).

-spec serpent_movement(spts_test_utils:config()) -> {comment, []}.
serpent_movement(Config) ->
  {game, GameId} = lists:keyfind(game, 1, Config),

  ct:comment("Check the game board and start the game"),
  Game = spts_core:fetch_game(GameId),
  Serpents = spts_games:serpents(Game),
  ok = spts_core:start_game(GameId),

  ct:comment("After a tick, all serpents should've moved in their directions"),
  spts_games:process_name(GameId) ! tick,
  NewGame = spts_core:fetch_game(GameId),

  FoundSerpents =
    lists:map(
      fun(Serpent) ->
        NewSerpent =
          spts_games:serpent(NewGame, spts_serpents:name(Serpent)),

        ExpectedDirection = spts_serpents:direction(Serpent),
        ExpectedDirection = spts_serpents:direction(NewSerpent),

        [Head|_] = spts_serpents:body(Serpent),
        ExpectedPosition =
          spts_test_utils:move(Head, spts_serpents:direction(Serpent)),
        [ExpectedPosition|_] = spts_serpents:body(NewSerpent),

        NewSerpent
      end, Serpents),

  [] = FoundSerpents -- spts_games:serpents(NewGame),
  [] = spts_games:serpents(NewGame) -- FoundSerpents,

  {comment, ""}.

-spec collision_detection_left_wall(spts_test_utils:config()) ->
  {comment, []}.
collision_detection_left_wall(Config) ->
  [{_, 0} | _] = collision_detection_wall(left, Config),
  {comment, ""}.

-spec collision_detection_right_wall(spts_test_utils:config()) ->
  {comment, []}.
collision_detection_right_wall(Config) ->
  [{_, 6} | _] = collision_detection_wall(right, Config),
  {comment, ""}.

-spec collision_detection_up_wall(spts_test_utils:config()) ->
  {comment, []}.
collision_detection_up_wall(Config) ->
  [{0, _} | _] = collision_detection_wall(up, Config),
  {comment, ""}.

-spec collision_detection_down_wall(spts_test_utils:config()) ->
  {comment, []}.
collision_detection_down_wall(Config) ->
  [{6, _} | _] = collision_detection_wall(down, Config),
  {comment, ""}.

collision_detection_wall(Direction, Config) ->
  {game, GameId} = lists:keyfind(game, 1, Config),

  ct:comment("Pick the first serpent and get it to move ~p", [Direction]),
  ok = spts_core:turn(GameId, <<"serp1">>, Direction),
  spts_core:start_game(GameId),

  ct:comment(
    "After moving the serpent 5 times ~p, it should've died", [Direction]),
  lists:foreach(
    fun(_) -> spts_games:process_name(GameId) ! tick end,
    lists:seq(1, 5)),

  NewGame = spts_core:fetch_game(GameId),
  finished = spts_games:state(NewGame),
  Serpent = spts_games:serpent(NewGame, <<"serp1">>),
  dead = spts_serpents:status(Serpent),
  spts_serpents:body(Serpent).

-spec collision_with_serpent_body(spts_test_utils:config()) ->
  {comment, []}.
collision_with_serpent_body(Config) ->
  {game, Game} = lists:keyfind(game, 1, Config),

  ct:comment("Serpents are placed in proper positions"),
  Serpent1 = spts_serpents:feed(spts_serpents:new(<<"serp1">>, {2, 2}, down)),
  Serpent2 = spts_serpents:new(<<"serp2">>, {2, 3}, left),
  GameWithSerpents =
    spts_games:add_serpent(spts_games:add_serpent(Game, Serpent1), Serpent2),

  ct:comment("When serpent 2 moves to the left it should collide with S1"),
  NewGame = spts_games_repo:advance(GameWithSerpents),

  ct:comment("S1 should be alive and its body should be 2 cells long"),
  alive = spts_serpents:status(spts_games:serpent(NewGame, <<"serp1">>)),

  ct:comment("S2 should've died in ~p", [NewGame]),
  dead = spts_serpents:status(spts_games:serpent(NewGame, <<"serp2">>)),

  {comment, ""}.

-spec collision_with_serpent_head(spts_test_utils:config()) ->
  {comment, []}.
collision_with_serpent_head(Config) ->
  {game, Game} = lists:keyfind(game, 1, Config),

  ct:comment("Serpents are placed in proper positions"),
  Serpent1 = spts_serpents:new(<<"serp1">>, {2, 2}, right),
  Serpent2 = spts_serpents:new(<<"serp2">>, {2, 4}, left),
  GameWithSerpents =
    spts_games:add_serpent(
      spts_games:add_serpent(Game, Serpent1), Serpent2),

  ct:comment("When serpents move they simultaenously collide"),
  NewGame = spts_games_repo:advance(GameWithSerpents),

  ct:comment("Both serpents are dead"),
  dead = spts_serpents:status(spts_games:serpent(NewGame, <<"serp1">>)),
  dead = spts_serpents:status(spts_games:serpent(NewGame, <<"serp2">>)),

  ct:comment("The game finished"),
  finished = spts_games:state(NewGame),

  {comment, ""}.

-spec always_a_fruit(spts_test_utils:config()) ->
  {comment, []}.
always_a_fruit(Config) ->
  {game, GameId} = lists:keyfind(game, 1, Config),

  Cycle =
    case spts_serpents:body(
          spts_games:serpent(
            spts_core:fetch_game(GameId), <<"serp1">>)) of
      [{1, 1} | _] -> [right, down, left, up];
      [{1, _} | _] -> [down, left, up, right];
      [{5, 5} | _] -> [left, up, right, down];
      [{5, _} | _] -> [up, right, down, left];
      [_|_] -> [up, right, down, left]
    end,

  spts_core:start_game(GameId),

  Advance =
    fun(Direction) ->
      ct:comment("Pick the serpent and get it to move ~p", [Direction]),
      ok = spts_core:turn(GameId, <<"serp1">>, Direction),
      spts_games:process_name(GameId) ! tick,
      NewGame = spts_core:fetch_game(GameId),
      ct:comment("Fruit is still there: ~p / ~p", [NewGame, Cycle]),
      [_|_] = spts_games:find(NewGame, fruit)
    end,

  lists:foreach(Advance, Cycle),

  {comment, ""}.

-spec fruit_reapears(spts_test_utils:config()) ->
  {comment, []}.
fruit_reapears(Config) ->
  {game, Game} = lists:keyfind(game, 1, Config),

  ct:comment("Serpent and fruit are placed in proper positions"),
  Serpent1 = spts_serpents:new(<<"serp1">>, {2, 2}, right),
  GameWithSerpentAndFruit =
    spts_games:add_serpent(
      spts_games:content(Game, {2, 3}, fruit), Serpent1),

  ct:comment("When serpent moves it feeds"),
  NewGame = spts_games_repo:advance(GameWithSerpentAndFruit),

  ct:comment("Serpent is alive and its head is where the fruit was"),
  NewSerpent1 = spts_games:serpent(NewGame, <<"serp1">>),
  alive = spts_serpents:status(NewSerpent1),
  [{2, 3}] = spts_serpents:body(NewSerpent1),

  ct:comment("Fruit is somewhere else"),
  case spts_games:find(NewGame, fruit) of
    [{2, 3}] -> ct:fail("Fruit did not move");
    [_NewFruit] -> ok
  end,

  {comment, ""}.

-spec fruit_feeds(spts_test_utils:config()) ->
  {comment, []}.
fruit_feeds(Config) ->
  {game, Game} = lists:keyfind(game, 1, Config),

  ct:comment("Serpent and fruit are placed in proper positions"),
  Serpent1 = spts_serpents:new(<<"serp1">>, {2, 2}, right),
  [{2, 2}] = spts_serpents:body(Serpent1),
  GameWithSerpentAndFruit =
    spts_games:add_serpent(
      spts_games:content(Game, {2, 3}, fruit), Serpent1),

  ct:comment("When serpent moves it feeds"),
  NewGame = spts_games_repo:advance(GameWithSerpentAndFruit),

  ct:comment("Serpent is alive and its head is where the fruit was"),
  NewSerpent1 = spts_games:serpent(NewGame, <<"serp1">>),
  alive = spts_serpents:status(NewSerpent1),
  [{2, 3}] = spts_serpents:body(NewSerpent1),

  ct:comment("Serpent advances again, it's body is extended"),
  {Direction, NewerHead, FinalHead} =
    case spts_games:find(NewGame, fruit) of
      [{2, 4}] -> {down, {3, 3}, {4, 3}};
      [_NewFruit] -> {right, {2, 4}, {2, 5}}
    end,
  NewerGame =
    spts_games_repo:advance(
      spts_games_repo:turn(NewGame, <<"serp1">>, Direction)),
  NewerSerpent1 = spts_games:serpent(NewerGame, <<"serp1">>),
  alive = spts_serpents:status(NewerSerpent1),
  [NewerHead, {2, 3}] = spts_serpents:body(NewerSerpent1),

  ct:comment("Serpent advances yet again, it's body is not extended"),
  FinalGame = spts_games_repo:advance(NewerGame),

  FinalSerpent1 = spts_games:serpent(FinalGame, <<"serp1">>),
  alive = spts_serpents:status(FinalSerpent1),
  [FinalHead, NewerHead] = spts_serpents:body(FinalSerpent1),

  {comment, ""}.

-spec countdown(spts_test_utils:config()) -> {comment, []}.
countdown(Config) ->
  {game, GameId} = lists:keyfind(game, 1, Config),

  ct:comment("Before start, there are 5 rounds of countdown to go"),
  Game = spts_core:fetch_game(GameId),
  5 = spts_games:countdown(Game),
  created = spts_games:state(Game),

  ct:comment("Before really starting the game, we spend 5 rounds of countdown"),
  ok = spts_core:start_game(GameId),
  lists:foreach(
    fun(Round) ->
      G = spts_core:fetch_game(GameId),
      Round = spts_games:countdown(G),
      countdown = spts_games:state(G),
      spts_games:process_name(GameId) ! tick
    end, lists:seq(4, 1, -1)),

  ct:comment("After the countdown the game should start"),
  spts_games:process_name(GameId) ! tick,
  FinalGame = spts_core:fetch_game(GameId),
  0 = spts_games:countdown(FinalGame),
  started = spts_games:state(FinalGame),

  {comment, ""}.

-spec no_countdown(spts_test_utils:config()) -> {comment, []}.
no_countdown(Config) ->
  {game, GameId} = lists:keyfind(game, 1, Config),

  ct:comment("Before start, there are 0 rounds of countdown to go"),
  Game = spts_core:fetch_game(GameId),
  0 = spts_games:countdown(Game),
  created = spts_games:state(Game),

  ct:comment("After start, the game jumped straight to started"),
  ok = spts_core:start_game(GameId),
  FinalGame = spts_core:fetch_game(GameId),
  0 = spts_games:countdown(FinalGame),
  started = spts_games:state(FinalGame),

  {comment, ""}.

-spec timeout(spts_test_utils:config()) -> {comment, []}.
timeout(Config) ->
  {game, GameId} = lists:keyfind(game, 1, Config),
  ok = spts_core:start_game(GameId),

  Tick = fun(_) -> spts_games:process_name(GameId) ! tick end,
  ct:comment("2 rounds of countdown"),
  lists:foreach(Tick, [1, 2]),

  ct:comment("after 9 game rounds, the game should still be started"),
  lists:foreach(Tick, lists:seq(1, 9)),
  started = spts_games:state(spts_core:fetch_game(GameId)),

  ct:comment("after the 10th round, the game should finish due to timeout"),
  spts_games:process_name(GameId) ! tick,
  FinalGame = spts_core:fetch_game(GameId),
  ct:comment("~p", [FinalGame]),
  finished = spts_games:state(FinalGame),
  alive = spts_serpents:status(spts_games:serpent(FinalGame, <<"serp1">>)),

  {comment, ""}.

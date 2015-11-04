-module(spts_coverage_SUITE).
-author('elbrujohalcon@inaka.net').

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {spts_test_utils,
         [ init_per_suite/1
         , end_per_suite/1
         ]}
       ]).

-export([ all/0
        , spts_web/1
        , spts_single_game_handler/1
        , spts_news_handler/1
        , spts_gen_event_handler/1
        , spts_core/1
        , spts_games/1
        , spts_hdp_handler/1
        , spts_hdp_game_handler/1
        ]).

-spec all() -> [atom()].
all() -> spts_test_utils:all(?MODULE).

-spec spts_hdp_game_handler(spts_test_utils:config()) -> {comment, []}.
spts_hdp_game_handler(_Config) ->
  ct:comment("spts_hdp_game_handler:code_change"),
  {ok, state} = spts_hdp_game_handler:code_change(oldvsn, state, extra),
  {comment, ""}.

-spec spts_hdp_handler(spts_test_utils:config()) -> {comment, []}.
spts_hdp_handler(_Config) ->
  ct:comment("spts_hdp_handler:terminate returns ok"),
  ok = spts_hdp_handler:terminate(reason, state),

  ct:comment("spts_hdp_handler:code_change"),
  {ok, state} = spts_hdp_handler:code_change(oldvsn, state, extra),
  {comment, ""}.

-spec spts_web(spts_test_utils:config()) -> {comment, []}.
spts_web(_Config) ->
  ct:comment("spts_web:handle_exception with unknown exception"),
  {halt, req, state} =
    spts_web:handle_exception({unknown, reason}, req, state),
  {comment, ""}.

-spec spts_single_game_handler(spts_test_utils:config()) -> {comment, []}.
spts_single_game_handler(_Config) ->
  ct:comment("spts_single_game_handler:handle_get with unknown exception"),
  {halt, req, state} = spts_single_game_handler:handle_get(req, state),

  ct:comment("spts_single_game_handler:delete_resource with unknown exception"),
  {halt, req, state} = spts_single_game_handler:delete_resource(req, state),
  {comment, ""}.

-spec spts_news_handler(spts_test_utils:config()) -> {comment, []}.
spts_news_handler(_Config) ->
  ct:comment("spts_news_handler:handle_error with unknown exception"),
  state = spts_news_handler:handle_error(event, error, state),
  {comment, ""}.

-spec spts_gen_event_handler(spts_test_utils:config()) -> {comment, []}.
spts_gen_event_handler(_Config) ->
  ct:comment("spts_gen_event_handler:code_change"),
  {ok, state} = spts_gen_event_handler:code_change(oldvsn, state, extra),
  {comment, ""}.

-spec spts_games(spts_test_utils:config()) -> {comment, []}.
spts_games(_Config) ->
  ct:comment("a game is created"),
  Game1NumId = spts_games:numeric_id(spts_core:create_game()),

  ct:comment("a new game is created, num id should not be ~p", [Game1NumId]),
  case spts_games:numeric_id(spts_core:create_game()) of
    Game1NumId -> ct:fail("Duplicated numeric_id: ~p", [Game1NumId]);
    _Game2NumId -> ok
  end,

  {comment, ""}.

-spec spts_core(spts_test_utils:config()) -> {comment, []}.
spts_core(_Config) ->
  ct:comment("spts_core:code_change"),
  {ok, state_name, state} =
    spts_core:code_change(oldvsn, state_name, state, extra),

  ct:comment("turn in closed"),
  GameId =
    spts_games:id(
      spts_core:create_game(
        #{max_serpents => 1, countdown => 1, rounds => 100})),
  spts_core:add_serpent(GameId, <<"tic">>),
  ok = spts_core:turn(GameId, <<"tic">>, right),
  ok = spts_core:turn(GameId, <<"no-tic">>, right),

  ct:comment("start in closed"),
  ok = spts_core:start_game(GameId),

  ct:comment("turn in countdown"),
  ok = spts_core:turn(GameId, <<"tic">>, left),
  ok = spts_core:turn(GameId, <<"no-tic">>, left),

  ct:comment("impossible turn"),
  try spts_core:add_serpent(<<"no-game-id">>, <<"it">>) of
    RR -> ct:fail("Unexpected result: ~p", [RR])
  catch
    _:{badgame, _} -> ok
  end,

  ct:comment("turn in started"),
  spts_games:process_name(GameId) ! tick,
  ok = spts_core:turn(GameId, <<"tic">>, up),
  ok = spts_core:turn(GameId, <<"no-tic">>, up),

  ct:comment("add_serpent in finished"),
  lists:foreach(
    fun(_) -> spts_games:process_name(GameId) ! tick end, lists:seq(1, 100)),
  try spts_core:add_serpent(GameId, <<"asif">>) of
    R -> ct:fail("Unexpected serpent in ~p: ~p", [GameId, R])
  catch
    throw:invalid_state -> ok
  end,

  ct:comment("start in finished"),
  ok = spts_core:start_game(GameId),

  ct:comment("turn in finished"),
  ok = spts_core:turn(GameId, <<"tic">>, left),
  ok = spts_core:turn(GameId, <<"no-tic">>, left),

  {comment, ""}.

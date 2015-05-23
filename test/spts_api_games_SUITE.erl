-module(spts_api_games_SUITE).
-author('elbrujohalcon@inaka.net').

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {spts_test_utils,
         [ init_per_suite/1
         , end_per_suite/1
         ]}
       ]).

-export([all/0]).
-export([ post_games_wrong/1
        , post_games_ok/1
        , get_games_ok/1
        , get_game_wrong/1
        , get_game_created/1
        , get_game_countdown/1
        , get_game_started/1
        , get_game_finished/1
        , put_game_wrong/1
        , put_game_ok/1
        % , delete_games/1
        ]).

-spec all() -> [atom()].
all() -> spts_test_utils:all(?MODULE).

-spec post_games_wrong(spts_test_utils:config()) -> {comment, []}.
post_games_wrong(_Config) ->

  ct:comment("POST without content-type fails"),
  #{status_code := 415} = spts_test_utils:api_call(post, "/games"),

  ct:comment("Something that's not json fails as well"),
  BadHeaders = #{<<"content-type">> => <<"text/plain">>},
  #{status_code := 415} = spts_test_utils:api_call(post, "/games", BadHeaders),

  ct:comment("Even with the right type"),
  Headers = #{<<"content-type">> => <<"application/json">>},
  #{status_code := 400} = spts_test_utils:api_call(post, "/games", Headers),

  ct:comment("Broken json fails"),
  #{status_code := 400,
           body := Body0} =
    spts_test_utils:api_call(post, "/games", Headers, "{"),
  #{<<"error">> := <<"bad_json">>} = spts_json:decode(Body0),

  ct:comment("Invalid rows fails"),
  #{status_code := 400,
           body := Body1} =
    spts_test_utils:api_call(post, "/games", Headers, "{\"rows\":-10}"),
  #{<<"error">> := <<"invalid_rows">>} = spts_json:decode(Body1),

  ct:comment("Invalid cols fails"),
  #{status_code := 400,
           body := Body2} =
    spts_test_utils:api_call(post, "/games", Headers, "{\"cols\":-10}"),
  #{<<"error">> := <<"invalid_cols">>} = spts_json:decode(Body2),

  ct:comment("Invalid ticktime fails"),
  #{status_code := 400,
           body := Body3} =
    spts_test_utils:api_call(post, "/games", Headers, "{\"ticktime\":-10}"),
  #{<<"error">> := <<"invalid_ticktime">>} = spts_json:decode(Body3),

  ct:comment("Invalid countdown fails"),
  #{status_code := 400,
           body := Body4} =
    spts_test_utils:api_call(post, "/games", Headers, "{\"countdown\":-10}"),
  #{<<"error">> := <<"invalid_countdown">>} = spts_json:decode(Body4),

  {comment, ""}.

-spec post_games_ok(spts_test_utils:config()) -> {comment, []}.
post_games_ok(_Config) ->
  ct:comment("Start a game, default values"),
  Headers = #{<<"content-type">> => <<"application/json">>},
  ReqBody1 = spts_json:encode(#{}),
  #{status_code := 201,
           body := Body1} =
    spts_test_utils:api_call(post, "/games", Headers, ReqBody1),
  #{ <<"id">> := Id1
   , <<"rows">> := 20
   , <<"cols">> := 20
   , <<"ticktime">> := 250
   , <<"countdown">> := 10
   , <<"serpents">> := #{}
   , <<"state">> := <<"created">>
   , <<"cells">> := []
   } = spts_json:decode(Body1),

  ct:comment("Start a game, no default values"),
  ReqBody2 = spts_json:encode(#{rows => 5, cols => 5, ticktime => 1000}),
  #{status_code := 201,
           body := Body2} =
    spts_test_utils:api_call(post, "/games", Headers, ReqBody2),
  #{ <<"id">> := Id2
   , <<"rows">> := 5
   , <<"cols">> := 5
   , <<"ticktime">> := 1000
   , <<"countdown">> := 10
   , <<"serpents">> := #{}
   , <<"state">> := <<"created">>
   , <<"cells">> := []
   } = spts_json:decode(Body2),

  case Id2 of
    Id1 -> ct:fail("Duplicated game");
    Id2 -> ok
  end,

  {comment, ""}.

-spec get_games_ok(spts_test_utils:config()) -> {comment, []}.
get_games_ok(_Config) ->
  ct:comment("Create a game"),
  Game1 = spts_core:create_game(),
  Game1Id = spts_games:id(Game1),

  #{status_code := 200,
           body := Body1} = spts_test_utils:api_call(get, "/games"),
  Games1 = spts_json:decode(Body1),
  [Game1Id] = [Id || #{<<"id">> := Id} <- Games1, Id == Game1Id],

  ct:comment("Create another game"),
  Game2 = spts_core:create_game(),
  Game2Id = spts_games:id(Game2),

  #{status_code := 200,
           body := Body2} = spts_test_utils:api_call(get, "/games"),
  Games2 = spts_json:decode(Body2),
  [#{<<"id">> := Game2Id}] = Games2 -- Games1,

  {comment, ""}.

-spec get_game_wrong(spts_test_utils:config()) -> {comment, []}.
get_game_wrong(_Config) ->
  ct:comment("GET a game that doesn't exist, returns 404"),
  #{status_code := 404} = spts_test_utils:api_call(get, "/games/not-a-game"),

  {comment, ""}.

-spec get_game_created(spts_test_utils:config()) -> {comment, []}.
get_game_created(_Config) ->
  ct:comment("Create a game"),
  Game = spts_core:create_game(),
  GameId = spts_games:id(Game),

  #{status_code := 200,
           body := Body} =
    spts_test_utils:api_call(get, <<"/games/", GameId/binary>>),

  #{ <<"id">> := GameId
   , <<"rows">> := 20
   , <<"cols">> := 20
   , <<"ticktime">> := 250
   , <<"countdown">> := 10
   , <<"serpents">> := #{}
   , <<"state">> := <<"created">>
   , <<"cells">> := []
   } = spts_json:decode(Body),

  {comment, ""}.

-spec get_game_countdown(spts_test_utils:config()) -> {comment, []}.
get_game_countdown(_Config) ->
  ct:comment("Create a game and start it"),
  Game = spts_core:create_game(#{ticktime => 60000}),
  GameId = spts_games:id(Game),
  Player = spts_core:register_player(<<"ggcd">>),
  PlayerId = spts_players:id(Player),
  {{Row, Col}, _} = spts_core:join_game(GameId, PlayerId),
  ok = spts_core:start_game(GameId),

  ct:comment("The game should be in countdown"),
  #{status_code := 200,
           body := Body} =
    spts_test_utils:api_call(get, <<"/games/", GameId/binary>>),

  #{ <<"id">> := GameId
   , <<"rows">> := 20
   , <<"cols">> := 20
   , <<"ticktime">> := 60000
   , <<"countdown">> := 9
   , <<"serpents">> := Serpents
   , <<"state">> := <<"countdown">>
   , <<"cells">> := []
   } = spts_json:decode(Body),

  [{ PlayerId
   , #{ <<"owner">> := #{<<"id">> := PlayerId, <<"name">> := <<"ggcd">>}
      , <<"body">> := [[Row, Col]]
      , <<"status">> := <<"alive">>
      }
  }] = maps:to_list(Serpents),

  {comment, ""}.

-spec get_game_started(spts_test_utils:config()) -> {comment, []}.
get_game_started(_Config) ->
  ct:comment("Create a game and start it"),
  Game = spts_core:create_game(#{ticktime => 60000, countdown => 0}),
  GameId = spts_games:id(Game),
  Player = spts_core:register_player(<<"ggs">>),
  PlayerId = spts_players:id(Player),
  {{Row, Col}, _} = spts_core:join_game(GameId, PlayerId),
  ok = spts_core:start_game(GameId),

  ct:comment("The game should be started"),
  #{status_code := 200,
           body := Body} =
    spts_test_utils:api_call(get, <<"/games/", GameId/binary>>),

  #{ <<"id">> := GameId
   , <<"rows">> := 20
   , <<"cols">> := 20
   , <<"ticktime">> := 60000
   , <<"countdown">> := 0
   , <<"serpents">> := Serpents
   , <<"state">> := <<"started">>
   , <<"cells">> := []
   } = spts_json:decode(Body),

  [{ PlayerId
   , #{ <<"owner">> := #{<<"id">> := PlayerId, <<"name">> := <<"ggs">>}
      , <<"body">> := [[Row, Col]]
      , <<"status">> := <<"alive">>
      }
  }] = maps:to_list(Serpents),

  {comment, ""}.

-spec get_game_finished(spts_test_utils:config()) -> {comment, []}.
get_game_finished(_Config) ->
  ct:comment("Create a game and advance it till the end"),
  Game =
    spts_core:create_game(
      #{ticktime => 60000, countdown => 0, rows => 5, cols => 5}),
  GameId = spts_games:id(Game),

  Player1 = spts_core:register_player(<<"ggf1">>),
  Player1Id = spts_players:id(Player1),
  {{_, Col1}, _} = spts_core:join_game(GameId, Player1Id),

  Player2 = spts_core:register_player(<<"ggf2">>),
  Player2Id = spts_players:id(Player2),
  {{_, Col2}, _} = spts_core:join_game(GameId, Player2Id),

  case Col1 of
    Col1 when Col1 > Col2 ->
      ok = spts_core:turn(GameId, Player1Id, right),
      ok = spts_core:turn(GameId, Player2Id, left);
    Col1 when Col1 =< Col2 ->
      ok = spts_core:turn(GameId, Player1Id, left),
      ok = spts_core:turn(GameId, Player2Id, right)
  end,

  ok = spts_core:start_game(GameId),
  lists:foreach(
    fun(_) ->
      spts_games:process_name(GameId) ! tick,
      #{status_code := 200,
               body := Body} =
        spts_test_utils:api_call(get, <<"/games/", GameId/binary>>),
      case spts_json:decode(Body) of
        #{<<"state">> := <<"finished">>} = DecodedBody ->
          #{<<"serpents">> := Serpents} = DecodedBody,
          #{ <<"owner">> := #{<<"id">> := Player1Id, <<"name">> := <<"ggf1">>}
           , <<"status">> := <<"dead">>
           } = maps:get(Player1Id, Serpents),
          #{ <<"owner">> := #{<<"id">> := Player2Id, <<"name">> := <<"ggf2">>}
           , <<"status">> := <<"dead">>
           } = maps:get(Player2Id, Serpents);
        #{<<"state">> := <<"started">>} = DecodedBody ->
          #{ <<"serpents">> := Serpents
           , <<"cells">> := Cells
           } = DecodedBody,
          [<<"alive">>, _] =
            lists:sort(
              [Status || #{<<"status">> := Status} <- maps:values(Serpents)]),
          [ #{ <<"row">> := _
           , <<"col">> := _
           , <<"content">> := <<"fruit">>
           }
          ] = Cells;
        #{<<"state">> := State} ->
          ct:fail("Unexpected state: ~p", [State])
      end
    end, lists:seq(1, 7)),

  {comment, ""}.

-spec put_game_wrong(spts_test_utils:config()) -> {comment, []}.
put_game_wrong(_Config) ->
  ct:comment("Create a game"),
  Game = spts_core:create_game(#{ticktime => 60000}),
  GameId = spts_games:id(Game),
  Url = <<"/games/", GameId/binary>>,
  Player = spts_core:register_player(<<"pgw">>),
  PlayerId = spts_players:id(Player),

  ct:comment("PUT without content-type fails"),
  #{status_code := 415} = spts_test_utils:api_call(put, "/games/id"),

  ct:comment("Something that's not json fails as well"),
  BadHeaders = #{<<"content-type">> => <<"text/plain">>},
  #{status_code := 415} = spts_test_utils:api_call(put, "/games/i", BadHeaders),

  Headers = #{<<"content-type">> => <<"application/json">>},

  ct:comment("PUT a game that doesn't exist, returns 404"),
  #{status_code := 404} =
    spts_test_utils:api_call(put, "/games/not-a-game", Headers, "{}"),

  ct:comment("Before somebody joins fails"),
  #{status_code := 403} =
    spts_test_utils:api_call(put, Url, Headers, "{\"state\":\"started\"}"),

  {_, _} = spts_core:join_game(GameId, PlayerId),

  ct:comment("PUT without body fails"),
  #{status_code := 400} = spts_test_utils:api_call(put, Url, Headers),

  ct:comment("Broken json fails"),
  #{status_code := 400,
           body := Body0} =
    spts_test_utils:api_call(put, Url, Headers, "{"),
  #{<<"error">> := <<"bad_json">>} = spts_json:decode(Body0),

  ct:comment("Without state fails"),
  #{status_code := 400,
           body := Body1} =
    spts_test_utils:api_call(put, Url, Headers, "{\"rows\":-10}"),
  #{<<"error">> := <<"missing field: state">>} = spts_json:decode(Body1),

  ct:comment("With invalid state fails"),
  #{status_code := 400,
           body := Body2} =
    spts_test_utils:api_call(put, Url, Headers, "{\"state\":\"countdown\"}"),
  #{<<"error">> := <<"invalid_state">>} = spts_json:decode(Body2),

  {comment, ""}.

-spec put_game_ok(spts_test_utils:config()) -> {comment, []}.
put_game_ok(_Config) ->
  ct:comment("Create a game"),
  Game = spts_core:create_game(#{ticktime => 60000, countdown => 0}),
  GameId = spts_games:id(Game),
  Url = <<"/games/", GameId/binary>>,
  Player = spts_core:register_player(<<"pgo">>),
  PlayerId = spts_players:id(Player),
  {{Row, Col}, _} = spts_core:join_game(GameId, PlayerId),

  Headers = #{<<"content-type">> => <<"application/json">>},

  ct:comment("The game should be started"),
  #{status_code := 200,
           body := Body} =
    spts_test_utils:api_call(put, Url, Headers, "{\"state\":\"started\"}"),

  #{ <<"id">> := GameId
   , <<"rows">> := 20
   , <<"cols">> := 20
   , <<"ticktime">> := 60000
   , <<"countdown">> := 0
   , <<"serpents">> := Serpents
   , <<"state">> := <<"started">>
   , <<"cells">> := []
   } = spts_json:decode(Body),

  [{ PlayerId
   , #{ <<"owner">> := #{<<"id">> := PlayerId, <<"name">> := <<"pgo">>}
      , <<"body">> := [[Row, Col]]
      , <<"status">> := <<"alive">>
      }
  }] = maps:to_list(Serpents),

  {comment, ""}.

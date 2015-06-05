-module(spts_api_serpents_SUITE).
-author('elbrujohalcon@inaka.net').

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {spts_test_utils,
         [ init_per_suite/1
         , end_per_suite/1
         ]}
       ]).

-export([all/0]).
-export([ post_serpents_wrong/1
        , post_serpents_ok/1
        , put_serpent_wrong/1
        , put_serpent_ok/1
        ]).

-spec all() -> [atom()].
all() -> spts_test_utils:all(?MODULE).

-spec post_serpents_wrong(spts_test_utils:config()) -> {comment, []}.
post_serpents_wrong(_Config) ->

  Url =
    fun(GameId) ->
      binary_to_list(<<"/games/", GameId/binary, "/serpents">>)
    end,

  ct:comment("POST without content-type fails"),
  #{status_code := 415} = spts_test_utils:api_call(post, Url(<<"x">>)),

  ct:comment("Something that's not json fails as well"),
  BadHeaders = #{<<"content-type">> => <<"text/plain">>},
  #{status_code := 415} =
    spts_test_utils:api_call(post, Url(<<"x">>), BadHeaders),

  Headers = #{<<"content-type">> => <<"application/json">>},

  ct:comment("Empty body fails"),
  #{status_code := 400} = spts_test_utils:api_call(post, Url(<<"x">>), Headers),

  ct:comment("Broken json fails"),
  #{status_code := 400,
           body := Body0} =
    spts_test_utils:api_call(post, Url(<<"x">>), Headers, "{"),
  #{<<"error">> := <<"bad_json">>} = spts_json:decode(Body0),

  ct:comment("Missing field fails"),
  #{status_code := 400,
           body := Body1} =
    spts_test_utils:api_call(post, Url(<<"x">>), Headers, "{}"),
  #{<<"error">> := <<"missing field: name">>} = spts_json:decode(Body1),

  ct:comment("If the game doesn't exit, fails"),
  ReqBody1 = spts_json:encode(#{name => s1}),
  #{status_code := 404} =
    spts_test_utils:api_call(post, Url(<<"x">>), Headers, ReqBody1),

  {comment, ""}.

-spec post_serpents_ok(spts_test_utils:config()) -> {comment, []}.
post_serpents_ok(_Config) ->
  ct:comment("Start a game, default values"),
  GameId = spts_games:id(spts_core:create_game()),
  Url = binary_to_list(<<"/games/", GameId/binary, "/serpents">>),

  ct:comment("Add a serpent s1"),
  Headers = #{<<"content-type">> => <<"application/json">>},
  ReqBody1 = spts_json:encode(#{name => s1}),
  #{status_code := 201,
           body := Body1} =
    spts_test_utils:api_call(post, Url, Headers, ReqBody1),
  #{ <<"name">> := <<"s1">>
   , <<"token">> := Token1
   , <<"body">> := [[Row1, Col1]]
   , <<"direction">> := Dir1
   , <<"status">> := <<"alive">>
   } = spts_json:decode(Body1),
  Serpent1 = spts_games:serpent(spts_core:fetch_game(GameId), <<"s1">>),
  Token1 = spts_serpents:token(Serpent1),
  [{Row1, Col1}] = spts_serpents:body(Serpent1),
  Dir1 = atom_to_binary(spts_serpents:direction(Serpent1), utf8),

  ct:comment("Can't add serpent s1 again"),
  #{status_code := 400,
           body := Body2} =
    spts_test_utils:api_call(post, Url, Headers, ReqBody1),
  #{<<"error">> := <<"already_in">>} = spts_json:decode(Body2),

  ct:comment("Can't add serpent s2 once the game started"),
  spts_core:start_game(GameId),
  ReqBody3 = spts_json:encode(#{name => s2}),
  #{status_code := 400,
           body := Body3} =
    spts_test_utils:api_call(post, Url, Headers, ReqBody3),
  #{<<"error">> := <<"invalid_state">>} = spts_json:decode(Body3),

  {comment, ""}.

-spec put_serpent_wrong(spts_test_utils:config()) -> {comment, []}.
put_serpent_wrong(_Config) ->
  ct:comment("Create a game"),
  GameId = spts_games:id(spts_core:create_game()),
  Token = spts_serpents:token(spts_core:add_serpent(GameId, <<"pgw">>)),
  Url = <<"/games/", GameId/binary, "/serpents/", Token/binary>>,

  ct:comment("PUT without content-type fails"),
  #{status_code := 415} = spts_test_utils:api_call(put, Url),

  ct:comment("Something that's not json fails as well"),
  BadHeaders = #{<<"content-type">> => <<"text/plain">>},
  #{status_code := 415} = spts_test_utils:api_call(put, Url, BadHeaders),

  Headers = #{<<"content-type">> => <<"application/json">>},

  ct:comment("PUT a game that doesn't exist, returns 404"),
  #{status_code := 404} =
    spts_test_utils:api_call(put, "/games/not-game/serpents/x", Headers, "{}"),

  ct:comment("PUT a serpent that doesn't exist, returns 404"),
  BadUrl = <<"/games/", GameId/binary, "/serpents/x">>,
  #{status_code := 404} =
    spts_test_utils:api_call(put, BadUrl, Headers, "{}"),

  ct:comment("PUT without body fails"),
  #{status_code := 400} = spts_test_utils:api_call(put, Url, Headers),

  ct:comment("Broken json fails"),
  #{status_code := 400,
           body := Body0} =
    spts_test_utils:api_call(put, Url, Headers, "{"),
  #{<<"error">> := <<"bad_json">>} = spts_json:decode(Body0),

  ct:comment("Without direction fails"),
  #{status_code := 400,
           body := Body1} =
    spts_test_utils:api_call(put, Url, Headers, "{}"),
  #{<<"error">> := <<"missing field: direction">>} = spts_json:decode(Body1),

  ct:comment("With invalid direction fails"),
  #{status_code := 400,
           body := Body2} =
    spts_test_utils:api_call(put, Url, Headers, "{\"direction\":\"in\"}"),
  #{<<"error">> := <<"invalid_direction">>} = spts_json:decode(Body2),

  {comment, ""}.

-spec put_serpent_ok(spts_test_utils:config()) -> {comment, []}.
put_serpent_ok(_Config) ->
  ct:comment("Create a game"),
  GameId = spts_games:id(spts_core:create_game()),
  Token = spts_serpents:token(spts_core:add_serpent(GameId, <<"pgo">>)),
  Url = <<"/games/", GameId/binary, "/serpents/", Token/binary>>,

  Headers = #{<<"content-type">> => <<"application/json">>},
  Dir = fun(D) -> spts_json:encode(#{direction => D}) end,

  ct:comment("The direction should be up"),
  #{status_code := 200,
           body := BodyUp} =
    spts_test_utils:api_call(put, Url, Headers, Dir(up)),
  #{<<"direction">> := <<"up">>} = spts_json:decode(BodyUp),
  up =
    spts_serpents:direction(
      spts_games:serpent(
        spts_core:fetch_game(GameId), <<"pgo">>)),

  ct:comment("The direction should be down"),
  #{status_code := 200,
           body := BodyDown} =
    spts_test_utils:api_call(put, Url, Headers, Dir(down)),
  #{<<"direction">> := <<"down">>} = spts_json:decode(BodyDown),
  down =
    spts_serpents:direction(
      spts_games:serpent(
        spts_core:fetch_game(GameId), <<"pgo">>)),

  ct:comment("The direction should be left"),
  #{status_code := 200,
           body := BodyLeft} =
    spts_test_utils:api_call(put, Url, Headers, Dir(left)),
  #{<<"direction">> := <<"left">>} = spts_json:decode(BodyLeft),
  left =
    spts_serpents:direction(
      spts_games:serpent(
        spts_core:fetch_game(GameId), <<"pgo">>)),

  ct:comment("The direction should be right"),
  #{status_code := 200,
           body := BodyRight} =
    spts_test_utils:api_call(put, Url, Headers, Dir(right)),
  #{<<"direction">> := <<"right">>} = spts_json:decode(BodyRight),
  right =
    spts_serpents:direction(
      spts_games:serpent(
        spts_core:fetch_game(GameId), <<"pgo">>)),

  {comment, ""}.

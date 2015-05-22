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
        % , get_games_ok/1
        % , put_games_wrong/1
        % , put_games_ok/1
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
  #{<<"error">> := <<"invalid json">>} = spts_json:decode(Body0),

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
   , <<"status">> := <<"created">>
   , <<"cells">> := [#{ <<"row">> := _
                      , <<"col">> := _
                      , <<"content">> := <<"fruit">>
                      }
                    ]
   } = spts_json:decode(Body1),

  ct:comment("Start a game, no default values"),
  ReqBody2 = spts_json:encode(#{rows => 5, cols => 5, ticktime => 1000}),
  #{status_code := 201,
           body := Body2} =
    spts_test_utils:api_call(post, "/games", Headers, ReqBody2),
  #{ <<"id">> := Id2
   , <<"rows">> := 5
   , <<"cols">> := 5
   , <<"ticktime">> := 10000
   , <<"countdown">> := 10
   , <<"serpents">> := #{}
   , <<"status">> := <<"created">>
   , <<"cells">> := [#{ <<"row">> := _
                      , <<"col">> := _
                      , <<"content">> := <<"fruit">>
                      }
                    ]
   } = spts_json:decode(Body2),

  case Id2 of
    Id1 -> ct:fail("Duplicated game");
    Id2 -> ok
  end,

  {comment, ""}.

-module(spts_api_news_SUITE).
-author('elbrujohalcon@inaka.net').

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {spts_test_utils,
         [ init_per_suite/1
         , end_per_suite/1
         ]}
       ]).

-export([all/0]).
-export([ get_news_wrong/1
        , game_status/1
        , serpent_added/1
        ]).

-spec all() -> [atom()].
all() -> spts_test_utils:all(?MODULE).

-spec get_news_wrong(spts_test_utils:config()) -> {comment, []}.
get_news_wrong(_Config) ->
  ct:comment("Trying to get news from a game that doesn't exist"),
  #{status_code := 404} =
    spts_test_utils:api_call(get, "/games/not-a-game/news"),

  {comment, ""}.

-spec game_status(spts_test_utils:config()) -> {comment, []}.
game_status(_Config) ->
  ct:comment("A game is created"),
  Headers = #{<<"content-type">> => <<"application/json">>},
  ReqBody = spts_json:encode(#{}),
  #{status_code := 201,
           body := Body} =
    spts_test_utils:api_call(post, "/games", Headers, ReqBody),
  #{<<"id">> := GameId} = Game = spts_json:decode(Body),

  ct:comment("A client connects, the first event is game_status"),
  [{_, _, EventBin1}] =
    spts_test_utils:get_events(<<"/games/", GameId/binary, "/news">>),
  #{data := DataBin1} = shotgun:parse_event(EventBin1),
  Game = spts_json:decode(DataBin1),

  ct:comment("Another client connects, the first event is still game_status"),
  [{_, _, EventBin2}] =
    spts_test_utils:get_events(<<"/games/", GameId/binary, "/news">>),
  #{data := DataBin2} = shotgun:parse_event(EventBin2),
  Game = spts_json:decode(DataBin2),

  {comment, ""}.

-spec serpent_added(spts_test_utils:config()) -> {comment, []}.
serpent_added(_Config) ->
  ct:comment("A game is created"),
  Headers = #{<<"content-type">> => <<"application/json">>},
  ReqBody = spts_json:encode(#{}),
  #{status_code := 201,
           body := Body} =
    spts_test_utils:api_call(post, "/games", Headers, ReqBody),
  #{<<"id">> := GameId} = Game = spts_json:decode(Body),

  ct:comment("A serpent is added and the client receives an event"),
  Task = fun() -> spts_core:add_serpent(GameId, <<"sa">>) end,
  {Serpent, [{_, _, _}, {_, _, EventBin}]} =
    spts_test_utils:get_events_after(
      <<"/games/", GameId/binary, "/news">>, Task),

  #{event := <<"serpent_added">>, data := Data} =
    shotgun:parse_event(EventBin),

  [{Row, Col}] = spts_serpents:body(Serpent),
  #{ <<"name">> := <<"sa">>
   , <<"body">> := [[Row, Col]]
   , <<"status">> := <<"alive">>
   } = spts_json:decode(Data),

  {comment, ""}.

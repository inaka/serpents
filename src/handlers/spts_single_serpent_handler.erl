%%% @doc /games/:game_id/serpents/:token handler
-module(spts_single_serpent_handler).
-author('elbrujohalcon@inaka.net').

-include_lib("mixer/include/mixer.hrl").
-mixin([{ spts_single_base_handler
        , [ init/3
          , rest_init/2
          , allowed_methods/2
          , content_types_accepted/2
          , content_types_provided/2
          ]
        }]).

-export([ forbidden/2
        , handle_put/2
        , resource_exists/2
        , trails/0
        ]).

-type state() :: spts_single_base_handler:state().

-behaviour(trails_handler).

-spec trails() -> trails:trails().
trails() ->
  Metadata =
    #{ get =>
       #{ tags => ["serpents"]
        , description => "Returns the description of a game"
        , produces => ["application/json"]
        , parameters => [ spts_web:param(game_id)
                        , spts_web:param(token)
                        ]
        }
     , put =>
       #{ tags => ["serpents"]
        , description => "Updates a game"
        , consumes => ["application/json"]
        , produces => ["application/json"]
        , parameters => [ spts_web:param(request_body)
                        , spts_web:param(game_id)
                        ]
        }
     , delete =>
       #{ tags => ["serpents"]
        , description => "Deletes a game"
        , parameters => [spts_web:param(game_id)]
        }
     },
  Path = "/api/games/:game_id/serpents/:token",
  Opts = #{path => Path},
  [trails:trail(Path, ?MODULE, Opts, Metadata)].

-spec forbidden(cowboy_req:req(), term()) ->
  {boolean(), cowboy_req:req(), term()}.
forbidden(Req, State) ->
  {GameId, Req1} = cowboy_req:binding(game_id, Req),
  {Method, Req2} = cowboy_req:method(Req1),
  IsForbidden = Method == <<"PUT">>
        andalso spts_core:is_game(GameId)
        andalso not spts_core:can_start(GameId),
  {IsForbidden, Req2, State}.

-spec resource_exists(cowboy_req:req(), term()) ->
  {boolean(), cowboy_req:req(), term()}.
resource_exists(Req, State) ->
  {GameId, Req1} = cowboy_req:binding(game_id, Req),
  {Token, Req2} = cowboy_req:binding(token, Req1),
  Response =
    spts_core:is_game(GameId) andalso
      spts_games:serpent_by_token(
        spts_core:fetch_game(GameId), Token) =/= notfound,
  {Response, Req2, State}.

-spec handle_put(cowboy_req:req(), state()) ->
  {halt | boolean(), cowboy_req:req(), state()}.
handle_put(Req, State) ->
  try
    {GameId, Req1} = cowboy_req:binding(game_id, Req),
    {Token, Req2} = cowboy_req:binding(token, Req1),
    {ok, Body, Req3} = cowboy_req:body(Req2),

    case spts_core:is_game(GameId) of
      false -> throw(notfound);
      true -> ok
    end,
    Game = spts_core:fetch_game(GameId),
    Serpent =
      case spts_games:serpent_by_token(Game, Token) of
        notfound -> throw(notfound);
        OldSerpent ->
          Direction = parse_body(spts_json:decode(Body)),
          ok =
            spts_core:turn(GameId, spts_serpents:name(OldSerpent), Direction),
          NewGame = spts_core:fetch_game(GameId),
          spts_games:serpent_by_token(NewGame, Token)
      end,

    RespBody = spts_json:encode(spts_serpents:to_json(Serpent, private)),
    Req4 = cowboy_req:set_resp_body(RespBody, Req3),
    {true, Req4, State}
  catch
    _:Exception ->
      spts_web:handle_exception(Exception, Req, State)
  end.

parse_body(#{<<"direction">> := D}) when D == <<"up">>
                                       ; D == <<"down">>
                                       ; D == <<"left">>
                                       ; D == <<"right">> ->
  binary_to_atom(D, utf8);
parse_body(#{<<"direction">> := _}) -> throw(invalid_direction);
parse_body(_) -> throw({missing_field, <<"direction">>}).

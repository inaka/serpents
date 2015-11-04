%%% @doc /games/:game_id/serpents handler
-module(spts_serpents_handler).
-author('elbrujohalcon@inaka.net').

-include_lib("mixer/include/mixer.hrl").
-mixin([{ spts_base_handler
        , [ init/3
          , rest_init/2
          , allowed_methods/2
          , content_types_accepted/2
          , content_types_provided/2
          , resource_exists/2
          ]
        }]).

-export([ handle_post/2
        , trails/0
        ]).

-type state() :: spts_base_handler:state().

-behaviour(trails_handler).

-spec trails() -> trails:trails().
trails() ->
  Metadata =
    #{ post =>
       #{ tags => ["serpents"]
        , description => "Adds a serpent to a game"
        , consumes => ["application/json"]
        , produces => ["application/json"]
        , parameters => [ spts_web:param(request_body)
                        , spts_web:param(game_id)
                        ]
        }
     },
  Path = "/api/games/:game_id/serpents",
  Opts = #{path => Path},
  [trails:trail(Path, ?MODULE, Opts, Metadata)].

-spec handle_post(cowboy_req:req(), state()) ->
  {halt | {boolean(), binary()}, cowboy_req:req(), state()}.
handle_post(Req, State) ->
  try
    {GameId, Req1} = cowboy_req:binding(game_id, Req),
    {ok, Body, Req2} = cowboy_req:body(Req1),
    Name = parse_body(spts_json:decode(Body)),
    #{<<"name">> := Name} = spts_json:decode(Body),

    case spts_core:is_game(GameId) of
      false -> throw(notfound);
      true -> ok
    end,

    Serpent = spts_core:add_serpent(GameId, Name),

    RespBody = spts_json:encode(spts_serpents:to_json(Serpent, private)),
    Req3 = cowboy_req:set_resp_body(RespBody, Req2),
    {{true, <<"/api/games/", Name/binary>>}, Req3, State}
  catch
    _:Exception ->
      spts_web:handle_exception(Exception, Req, State)
  end.

parse_body(#{<<"name">> := Name}) -> Name;
parse_body(_) -> throw({missing_field, <<"name">>}).

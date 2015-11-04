%%% @doc /games handler
-module(spts_games_handler).
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
        , handle_get/2
        , trails/0
        ]).

-type state() :: spts_base_handler:state().

-behaviour(trails_handler).

-spec trails() -> trails:trails().
trails() ->
  Metadata =
    #{ get =>
       #{ tags => ["games"]
        , description => "Returns the list of running games"
        , produces => ["application/json"]
        }
     , post =>
       #{ tags => ["games"]
        , description => "Creates a new game"
        , consumes => ["application/json"]
        , produces => ["application/json"]
        , parameters => [spts_web:param(request_body)]
        }
     },
  Path = "/api/games",
  Opts = #{path => Path},
  [trails:trail(Path, ?MODULE, Opts, Metadata)].

-spec handle_post(cowboy_req:req(), state()) ->
  {halt | {true, binary()}, cowboy_req:req(), state()}.
handle_post(Req, State) ->
  try
    {ok, Body, Req1} = cowboy_req:body(Req),
    Options = parse_body(spts_json:decode(Body)),
    Game = spts_core:create_game(Options),
    GameId = spts_games:id(Game),
    RespBody = spts_json:encode(spts_games:to_json(Game)),
    Req2 = cowboy_req:set_resp_body(RespBody, Req1),
    {{true, <<"/api/games/", GameId/binary>>}, Req2, State}
  catch
    _:Exception ->
      spts_web:handle_exception(Exception, Req, State)
  end.

-spec handle_get(cowboy_req:req(), state()) ->
  {iodata(), cowboy_req:req(), state()}.
handle_get(Req, State) ->
  Games = spts_core:all_games(),
  RespBody = spts_json:encode([spts_games:to_json(Game) || Game <- Games]),
  {RespBody, Req, State}.

parse_body(Body) ->
  maps:from_list([parse(K, V) || {K, V} <- maps:to_list(Body)]).

parse(<<"flags">>, Flags) -> {flags, [binary_to_atom(F, utf8) || F <- Flags]};
parse(K, V) -> {binary_to_atom(K, utf8), V}.

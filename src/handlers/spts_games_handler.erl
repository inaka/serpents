%%% @doc /games handler
-module(spts_games_handler).
-author('elbrujohalcon@inaka.net').

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {spts_base_handler,
         [ init/3
         , rest_init/2
         , content_types_accepted/2
         , content_types_provided/2
         , resource_exists/2
         ]}
       ]).

-export([ allowed_methods/2
        , handle_post/2
        , handle_get/2
        ]).

-type state() :: spts_base_handler:state().

-spec init({atom(), atom()}, cowboy_req:req(), state()) ->
  {upgrade, protocol, cowboy_rest}.
-spec rest_init(cowboy_req:req(), state()) ->
  {ok, cowboy_req:req(), term()}.
-spec content_types_accepted(cowboy_req:req(), state()) ->
  {[term()], cowboy_req:req(), state()}.
-spec content_types_provided(cowboy_req:req(), state()) ->
  {[term()], cowboy_req:req(), state()}.
-spec resource_exists(cowboy_req:req(), term()) ->
  {boolean(), cowboy_req:req(), term()}.


-spec allowed_methods(cowboy_req:req(), state()) ->
  {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, State) ->
  {[<<"POST">>, <<"GET">>], Req, State}.

-spec handle_post(cowboy_req:req(), state()) ->
  {halt | {boolean(), binary()}, cowboy_req:req(), state()}.
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
      spts_web_utils:handle_exception(Exception, Req, State)
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

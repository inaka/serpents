%%% @doc /games/:game_id handler
-module(spts_single_game_handler).
-author('elbrujohalcon@inaka.net').

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {spts_base_handler,
         [ init/3
         , rest_init/2
         , content_types_accepted/2
         , content_types_provided/2
         ]}
       ]).

-export([ allowed_methods/2
        , handle_put/2
        , handle_get/2
        , delete_resource/2
        , resource_exists/2
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

-spec allowed_methods(cowboy_req:req(), state()) ->
  {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, State) ->
  {[<<"POST">>, <<"GET">>], Req, State}.

-spec resource_exists(cowboy_req:req(), term()) ->
  {boolean(), cowboy_req:req(), term()}.
resource_exists(Req, State) ->
  {GameId, Req1} = cowboy_req:binding(game_id, Req),
  Response = spts_core:is_game(GameId),
  {Response, Req, State}.

-spec handle_put(cowboy_req:req(), state()) ->
  {halt | boolean(), cowboy_req:req(), state()}.
handle_put(Req, State) ->
  try
    {GameId, Req1} = cowboy_req:binding(game_id, Req),
    {ok, Body, Req2} = cowboy_req:body(Req1),
    ok = check_body(spts_json:decode(Body)),
    ok = spts_core:start_game(GameId),
    {true, Req2, State}
  catch
    _:Exception ->
      spts_web_utils:handle_exception(Exception, Req, State)
  end.

-spec handle_get(cowboy_req:req(), state()) ->
  {halt | iodata(), cowboy_req:req(), state()}.
handle_get(Req, State) ->
  try
    {GameId, Req1} = cowboy_req:binding(game_id, Req),
    Game = spts_core:fetch_game(GameId),
    RespBody = spts_json:encode(spts_games:to_json(Game)),
    {RespBody, Req, State}
  catch
    _:Exception ->
      spts_web_utils:handle_exception(Exception, Req, State)
  end.

-spec delete_resource(cowboy_req:req(), state()) ->
  {halt | boolean(), cowboy_req:req(), state()}.
delete_resource(Req, State) ->
  try
    {GameId, Req1} = cowboy_req:binding(game_id, Req),
    ok = spts_core:stop_game(GameId),
    {true, Req1, State}
  catch
    _:Exception ->
      spts_web_utils:handle_exception(Exception, Req, State)
  end.

check_body(#{<<"state">> := <<"started">>}) -> ok;
check_body(_) -> throw({missing_field, <<"state">>}).

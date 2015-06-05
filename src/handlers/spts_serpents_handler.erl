%%% @doc /games/:game_id/serpents handler
-module(spts_serpents_handler).
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
allowed_methods(Req, State) -> {[<<"POST">>], Req, State}.

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
      spts_web_utils:handle_exception(Exception, Req, State)
  end.

parse_body(#{<<"name">> := Name}) -> Name;
parse_body(_) -> throw({missing_field, <<"name">>}).

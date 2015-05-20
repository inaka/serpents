%%% @doc /status handler
-module(serpents_status_handler).
-author('elbrujohalcon@inaka.net').

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {serpents_base_handler,
         [ init/3
         , rest_init/2
         , content_types_provided/2
         , resource_exists/2
         ]}
       ]).

-export([ allowed_methods/2
        , is_authorized/2
        , handle_get/2
        ]).

-type state() :: serpents_base_handler:state().

-spec init({atom(), atom()}, cowboy_req:req(), state()) ->
  {upgrade, protocol, cowboy_rest}.
-spec rest_init(cowboy_req:req(), state()) ->
  {ok, cowboy_req:req(), term()}.
-spec content_types_provided(cowboy_req:req(), state()) ->
  {[term()], cowboy_req:req(), state()}.
-spec resource_exists(cowboy_req:req(), term()) ->
  {boolean(), cowboy_req:req(), term()}.

-spec allowed_methods(cowboy_req:req(), state()) ->
  {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

-spec is_authorized(cowboy_req:req(), state()) ->
  {boolean(), cowboy_req:req(), state()}.
is_authorized(Req, State) ->
  {true, Req, State}.

-spec handle_get(cowboy_req:req(), state()) ->
  {halt | binary(), cowboy_req:req(), state()}.
handle_get(Req, State) ->
  Reply = serpents_json:encode(#{status => <<"ok">>}),
  {Reply, Req, State}.

%%% @doc Base handler implementation for single entity endpoints
-module(spts_single_base_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ spts_base_handler
        , [ init/3
          , allowed_methods/2
          , content_types_provided/2
          ]
        }]).

-export([ rest_init/2
        , content_types_accepted/2
        ]).

-type options() :: spts_base_handler:options().
-type state() :: #{ opts => options()
                  , id => binary()
                  }.
-export_type([state/0, options/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Cowboy Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec rest_init(cowboy_req:req(), options()) ->
  {ok, cowboy_req:req(), state()}.
rest_init(Req, Opts) ->
  Req1 = spts_web:announce_req(Req, []),
  {Id, Req2} = cowboy_req:binding(id, Req1),
  {ok, Req2, #{opts => Opts, id => Id}}.

-spec content_types_accepted(cowboy_req:req(), state()) ->
  {[{{binary(), binary(), '*'}, atom()}], cowboy_req:req(), state()}.
content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, '*'}, handle_put}], Req, State}.

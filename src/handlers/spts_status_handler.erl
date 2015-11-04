%%% @doc /status handler
-module(spts_status_handler).
-author('elbrujohalcon@inaka.net').

-include_lib("mixer/include/mixer.hrl").
-mixin([{ spts_base_handler
        , [ init/3
          , rest_init/2
          , allowed_methods/2
          , resource_exists/2
          , content_types_provided/2
          ]
        }]).

-export([ handle_get/2
        , trails/0
        ]).

-type state() :: spts_base_handler:state().

-behaviour(trails_handler).

-spec trails() -> trails:trails().
trails() ->
  Metadata =
    #{ get =>
       #{ tags => ["status"]
        , description => "Returns the system status"
        , produces => ["application/json"]
        }
     },
  Path = "/api/status",
  Opts = #{path => Path},
  [trails:trail(Path, ?MODULE, Opts, Metadata)].

-spec handle_get(cowboy_req:req(), state()) ->
  {iodata(), cowboy_req:req(), state()}.
handle_get(Req, State) ->
  Reply = spts_json:encode(#{status => <<"ok">>}),
  {Reply, Req, State}.

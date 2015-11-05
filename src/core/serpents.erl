%%% @doc Main Application Module
-module(serpents).
-author('elbrujohalcon@inaka.net').

-behaviour(application).

-export([ start/0
        , stop/0
        , start/2
        , start_phase/3
        , stop/1
        ]).

%% @todo remove when https://github.com/inaka/cowboy-trails/issues/43 is fixed
-dialyzer([{no_fail_call, start_phase/3}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Start / Stop
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Starts the Application
-spec start() -> {ok, [atom()]} | {error, term()}.
start() -> application:ensure_all_started(?MODULE).

%% @doc Stops the Application
-spec stop() -> ok | {error, term()}.
stop() -> application:stop(?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Behaviour Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
-spec start(application:start_type(), any()) -> {ok, pid()} | {error, term()}.
start(_StartType, _Args) ->
  spts_sup:start_link().

%% @private
-spec start_phase(atom(), application:start_type(), []) -> ok | {error, _}.
start_phase(cxy_ctl_setup, _StartType, []) ->
  true = cxy_ctl:init([{spts_hdp, unlimited, 1000, 100000}]),
  ok;
start_phase(start_cowboy_listeners, _StartType, []) ->
  Port = application:get_env(?MODULE, http_port, 8585),
  ListenerCount = application:get_env(?MODULE, http_listener_count, 10),

  hack_cowboy_swagger_path(),

  Handlers =
    [ spts_status_handler
    , spts_games_handler
    , spts_single_game_handler
    , spts_serpents_handler
    , spts_single_serpent_handler
    , spts_news_handler
    , cowboy_swagger_handler
    ],
  Trails =
    [ trails:trail("/", cowboy_static, {file, "www/index.html"})
    , trails:trail(
        "/favicon.ico", cowboy_static, {file, "www/assets/favicon.ico"})
    , trails:trail("/assets/[...]", cowboy_static, {dir, "www/assets"})
    , trails:trail("/game/:game_id", cowboy_static, {file, "www/game.html"})
    | trails:trails(Handlers)
    ],
  ok = trails:store(Trails),
  Dispatch = trails:single_host_compile(Trails),

  TransOpts = [{port, Port}],
  ProtoOpts = [{env, [{dispatch, Dispatch}, {compress, true}]}],
  case cowboy:start_http(
        spts_http_listener, ListenerCount, TransOpts, ProtoOpts) of
    {ok, _} -> ok;
    {error, {already_started, _}} -> ok
  end.

%% @private
-spec stop([]) -> ok.
stop([]) -> ok.

%% @doc Properly sets cowboy swagger priv path.
%% @hack Waiting for https://github.com/inaka/cowboy-swagger/issues/26
%% @todo revert cowboy-swagger workaround
%%       once https://github.com/inaka/cowboy-swagger/issues/26 is fixed
-spec hack_cowboy_swagger_path() -> ok.
hack_cowboy_swagger_path() ->
  CowboySwaggerPriv =
    case code:priv_dir(cowboy_swagger) of
      {error, bad_name} ->
        filename:join(
          [ filename:dirname(code:which(cowboy_swagger_handler))
          , ".."
          , "priv"
          ]);
      Path -> Path
    end,
  StaticFiles = filename:join(CowboySwaggerPriv, "swagger"),
  application:set_env(cowboy_swagger, static_files, StaticFiles),
  ok.

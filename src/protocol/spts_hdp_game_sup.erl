%%% @doc serpents HDP game supervisor
-module(spts_hdp_game_sup).
-behavior(supervisor).

-export([start_link/0, start_child/1, init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Start / Stop
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, noargs).

-spec start_child(pos_integer()) -> supervisor:startchild_ret().
start_child(GameId) ->
  supervisor:start_child(?MODULE, [GameId]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SUPERVISOR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init(noargs) ->
  {ok, {{simple_one_for_one, 5, 10}, [supervisor:child_spec()]}}.
init(_Args) ->
  GameMgr =
    {spts_hdp_game_handler, {spts_hdp_game_handler, start_link, []},
      transient, brutal_kill, worker, [spts_hdp_game_handler]},
  {ok, {{simple_one_for_one, 5, 10}, [GameMgr]}}.

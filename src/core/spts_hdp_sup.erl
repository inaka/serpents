%%% @doc main HDP supervisor
-module(spts_hdp_sup).
-behavior(supervisor).

-export([start_link/0, init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Start / Stop
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, noargs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SUPERVISOR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init(noargs) ->
  {ok, {{one_for_one, 5, 10}, [supervisor:child_spec()]}}.
init(noargs) ->
  Handler =
    {spts_hdp_handler, {spts_hdp_handler, start_link, []},
      permanent, 5000, worker, [spts_hdp_handler]},
  GameSup =
    {spts_hdp_game_sup, {spts_hdp_game_sup, start_link, []},
      permanent, 5000, supervisor, [spts_hdp_game_sup]},
  {ok, {{one_for_one, 5, 10}, [Handler, GameSup]}}.

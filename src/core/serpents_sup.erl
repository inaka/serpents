%%% @doc main serpents supervisor
-module(serpents_sup).
-behavior(supervisor).

-export([start_link/0, init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Start / Stop
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link() -> ok.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, noargs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SUPERVISOR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init(noargs) -> {ok, term()}.
init(noargs) ->
  KatanaRandom =
    {ktn_random, {ktn_random, start_link, []},
      permanent, 5000, worker, [ktn_random]},
  GameSup =
    {serpents_game_sup, {serpents_game_sup, start_link, []},
      permanent, 5000, supervisor, [serpents_game_sup]},
  {ok, {{one_for_one, 5, 10}, [KatanaRandom, GameSup]}}.

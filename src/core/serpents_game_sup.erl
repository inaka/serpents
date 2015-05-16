%%% @doc serpents game supervisor
-module(serpents_game_sup).
-behavior(supervisor).

-export([start_link/0, start_child/1, init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Start / Stop
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, noargs).

-spec start_child(serpents_games:game()) -> supervisor:startchild_ret().
start_child(Game) ->
  supervisor:start_child(?MODULE, [Game]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SUPERVISOR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init(noargs) ->
  {ok, {{simple_one_for_one, 5, 10}, [supervisor:child_spec()]}}.
init(_Args) ->
  GameCore =
    {serpents_core, {serpents_core, start_link, []},
      temporary, brutal_kill, worker, [serpents_core]},
  {ok, {{simple_one_for_one, 5, 10}, [GameCore]}}.

%%% @doc serpents game supervisor
-module(serpents_game_sup).
-behavior(supervisor).

-export([start_link/0, start_child/1, init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Start / Stop
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link() -> ok.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, noargs).

-spec start_child(serpents_games:game()) -> {ok, pid()}.
start_child(Game) ->
  supervisor:start_child(?MODULE, [Game]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SUPERVISOR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init(term()) -> {ok, term()}.
init(_Args) ->
  GameCore =
    {serpents_core, {serpents_core, start_link, []},
      transient, brutal_kill, worker, [serpents_core]},
  {ok, {{simple_one_for_one, 5, 10}, [GameCore]}}.

%%% @doc main serpents supervisor
-module(spts_sup).
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
  KatanaRandom =
    {ktn_random, {ktn_random, start_link, []},
      permanent, 5000, worker, [ktn_random]},
  GameSup =
    {spts_game_sup, {spts_game_sup, start_link, []},
      permanent, 5000, supervisor, [spts_game_sup]},
  UDPSup1 =
    {spts_udp_game_handler, {spts_udp_game_handler, start_link, []},
      permanent, 5000, worker, [spts_udp_game_handler]},
  UDPSup2 =
    {spts_udp_handler, {spts_udp_handler, start_link, []},
      permanent, 5000, worker, [spts_udp_handler]},
  {ok, {{one_for_one, 5, 10}, [KatanaRandom, GameSup, UDPSup1, UDPSup2]}}.

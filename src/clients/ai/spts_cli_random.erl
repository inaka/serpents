%% @doc dumb AI client that just moves in random directions
-module(spts_cli_random).

-behaviour(spts_cli).

%%% gen_server callbacks
-export([init/3, handle_update/4, terminate/4]).

%%% API
-export([play/2, quit/1]).

-record(state, {}).
-type state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec play(spts_games:id(), spts_serpents:name()) -> {ok, pid()}.
play(GameId, SerpentName) ->
  spts_cli:start(GameId, SerpentName, ?MODULE, noargs).

-spec quit(pid()) -> ok.
quit(Cli) -> spts_cli:stop(Cli).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callback implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init(pos_integer(), spts_hdp:game(), noargs) -> state().
init(_SerpentId, _Game, noargs) -> #state{}.

-spec handle_update(
  [spts_hdp:diff()], pos_integer(), spts_hdp:game(), state()) ->
  {none | spts_games:direction(), state()}.
handle_update([], _SerpentId, _Game, State) ->
  %NOTE: no changes, nothing to do
  {none, State};
handle_update(Diffs, SerpentId, Game, State) ->
  lager:notice(
    "\n\tDiffs = ~p,\n\tSerpentId = ~p,\n\tGame = ~p",
    [Diffs, SerpentId, Game]),
  Direction = ktn_random:pick([up, down, left, right]),
  {Direction, State}.

-spec terminate(term(), pos_integer(), spts_hdp:game(), state()) -> _.
terminate(_Reason, SerpentId, _Game, _State) ->
  lager:notice("I (~p) am dead!", [SerpentId]),
  ok.

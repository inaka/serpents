%% @doc Just go for the fruit, man!
%%      This client is awesome for empty (i.e. no walls, no serpents) games.
-module(spts_cli_greedy).

-behaviour(spts_cli).

%%% gen_server callbacks
-export([init/3, handle_update/4, terminate/4]).

%%% API
-export([play/2, quit/1]).

-record(state, {direction = left :: spts_games:direction()}).
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
  {spts_games:direction(), state()}.
handle_update([], _SerpentId, _Game, State) ->
  %NOTE: no changes, nothing to do
  {State#state.direction, State};
handle_update(_Diffs, SerpentId, #{fruit := {_, FRow, FCol}} = Game, State) ->
  #{serpents := Serpents} = Game,
  [[{HRow, HCol} | _]] =
    [Body || #{id := SId, body := Body} <- Serpents, SId == SerpentId],
  DesiredDirection =
    case {abs(FRow - HRow), abs(FCol - HCol)} of
      {VDelta, HDelta} when VDelta > HDelta, FRow > HRow -> down;
      {VDelta, HDelta} when VDelta > HDelta, FRow =< HRow -> up;
      {VDelta, HDelta} when VDelta =< HDelta, FCol > HCol -> right;
      {VDelta, HDelta} when VDelta =< HDelta, FCol =< HCol -> left
    end,
  Direction = avoid_collisions(DesiredDirection, HRow, HCol, Game),
  {Direction, State#state{direction = Direction}};
handle_update(_Diffs, _SerpentId, _Game, State) ->
  %NOTE: no fruit, nothing to do
  {State#state.direction, State}.

-spec terminate(term(), pos_integer(), spts_hdp:game(), state()) -> _.
terminate(_Reason, SerpentId, _Game, _State) ->
  _ = lager:notice("I (~p) am dead!", [SerpentId]),
  ok.

avoid_collisions(DesiredDirection, HRow, HCol, Game) ->
  #{serpents := Serpents, rows := Rows, cols := Cols} = Game,
  BadCells = [Cell || #{body := Body} <- Serpents, Cell <- Body],
  try_avoid_collision(
    [DesiredDirection, up, down, left, right],
    BadCells, HRow, HCol, Rows, Cols).

try_avoid_collision([], _BadCells, _Row, _Col, _Rows, _Cols) -> left;
try_avoid_collision([Dir | Rest], BadCells, Row, Col, Rows, Cols) ->
  case is_available(next_cell(Dir, Row, Col), BadCells, Rows, Cols) of
    true -> Dir;
    false -> try_avoid_collision(Rest, BadCells, Row, Col, Rows, Cols)
  end.

next_cell(up, Row, Col) -> {Row - 1, Col};
next_cell(down, Row, Col) -> {Row + 1, Col};
next_cell(left, Row, Col) -> {Row, Col - 1};
next_cell(right, Row, Col) -> {Row, Col + 1}.

is_available({0, _}, _, _, _) -> false;
is_available({_, 0}, _, _, _) -> false;
is_available({R, _}, _, Rows, _) when R > Rows -> false;
is_available({_, C}, _, _, Cols) when C > Cols -> false;
is_available(Cell, BadCells, _, _) -> not lists:member(Cell, BadCells).

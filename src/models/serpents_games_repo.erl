%%% @doc Games repository
-module(serpents_games_repo).
-author('elbrujohalcon@inaka.net').

-export([ create/1
        , join/2
        , start/1
        , turn/3
        ]).

%% @doc Creates a new game
-spec create(serpents_core:options()) -> serpents_games:game().
create(Options) ->
  Rows = maps:get(rows, Options, 20),
  Cols = maps:get(cols, Options, 20),
  TickTime = maps:get(ticktime, Options, 250),
  validate(Rows, Cols, TickTime),
  serpents_games:new(Rows, Cols, TickTime).

%% @doc Adds a player to a game
-spec join(serpents_games:game(), serpents_players:id()) ->
  serpents_games:game().
join(Game, PlayerId) ->
  case serpents_games:serpent(Game, PlayerId) of
    notfound ->
      Position = find_empty_position(Game),
      Direction = random_direction(Game, Position),
      serpents_games:add_player(Game, PlayerId, Position, Direction);
    _ -> throw(already_joined)
  end.

%% @doc Starts a game
-spec start(serpents_games:game()) -> serpents_games:game().
start(Game) -> serpents_games:state(Game, started).

%% @doc Registers a change in direction for a player
-spec turn(
  serpents_games:game(), serpents_players:id(),
  serpents_serpents:direction()) -> serpents_games:game().
turn(Game, PlayerId, Direction) ->
  case serpents_games:serpent(Game, PlayerId) of
    notfound -> throw(invalid_player);
    _Serpent -> serpents_games:turn(Game, PlayerId, Direction)
  end.

%% @todo wait for ktn_random:uniform/1 and remove the seeding
find_empty_position(Game) ->
  random:seed(erlang:now()),
  Rows = serpents_games:rows(Game),
  Cols = serpents_games:cols(Game),
  case try_random_fep(Game, Rows, Cols, 10) of
    notfound -> walkthrough_fep(Game, Rows, Cols);
    Position -> Position
  end.

%% @todo wait for ktn_random:uniform/1 and replace random:uniform here
try_random_fep(_Game, _Rows, _Cols, 0) ->
  notfound;
try_random_fep(Game, Rows, Cols, Attempts) ->
  Position = {random:uniform(Rows), random:uniform(Cols)},
  case is_proper_starting_point(Game, Position) of
    true -> Position;
    _ -> try_random_fep(Game, Rows, Cols, Attempts - 1)
  end.

walkthrough_fep(Game, Rows, Cols) ->
  walkthrough_fep(Game, Rows, Cols, {1, 1}).
walkthrough_fep(_Game, _Rows, _Cols, game_full) ->
  throw(game_full);
walkthrough_fep(Game, Rows, Cols, Position = {Rows, Cols}) ->
  try_walkthrough_fep(Game, Rows, Cols, Position, game_full);
walkthrough_fep(Game, Rows, Cols, Position = {Row, Cols}) ->
  try_walkthrough_fep(Game, Rows, Cols, Position, {Row + 1, 1});
walkthrough_fep(Game, Rows, Cols, Position = {Row, Col}) ->
  try_walkthrough_fep(Game, Rows, Cols, Position, {Row, Col + 1}).

try_walkthrough_fep(Game, Rows, Cols, Position, NextPosition) ->
  case is_proper_starting_point(Game, Position) of
    true -> Position;
    _ -> walkthrough_fep(Game, Rows, Cols, NextPosition)
  end.

%% @todo wait for ktn_random:uniform/1 and replace random:uniform here
random_direction(Game, {Row, Col}) ->
  Candidates =
    surrounding_positions(
      Row, Col, serpents_games:rows(Game), serpents_games:cols(Game)),
  {_, Direction} =
    lists:nth(random:uniform(length(Candidates)), Candidates),
  Direction.

validate(Rows, _Cols, _TickTime) when Rows < 5 -> throw(invalid_rows);
validate(_Rows, Cols, _TickTime) when Cols < 5 -> throw(invalid_cols);
validate(_Rows, _Cols, TickTime) when TickTime < 100 -> throw(invalid_ticktime);
validate(_Rows, _Cols, _TickTime) -> ok.

is_proper_starting_point(Game, {Row, Col}) ->
  SurroundingPositions =
    surrounding_positions(
      Row, Col, serpents_games:rows(Game), serpents_games:cols(Game)),
  lists:all(
    fun({Pos, _}) -> air == serpents_games:content(Game, Pos) end,
    [{{Row, Col}, none} | SurroundingPositions]).

surrounding_positions(Row, Col, Rows, Cols) ->
  Candidates =
    [ {Row-1, Col, up}
    , {Row+1, Col, down}
    , {Row, Col-1, left}
    , {Row, Col+1, right}
    ],
  [{{R, C}, D} || {R, C, D} <- Candidates, R > 0, C > 0, R =< Rows, C =< Cols].

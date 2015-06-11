%%% @doc Games repository
-module(spts_games_repo).
-author('elbrujohalcon@inaka.net').

-export([ create/1
        , add_serpent/2
        , countdown_or_start/1
        , turn/3
        , advance/1
        , can_add_serpent/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Creates a new game
-spec create(spts_core:options()) -> spts_games:game().
create(Options) ->
  {NumericId, Name} = random_name(),
  Rows = maps:get(rows, Options, 20),
  Cols = maps:get(cols, Options, 20),
  TickTime = maps:get(ticktime, Options, 250),
  Countdown = maps:get(countdown, Options, 10),
  Rounds = maps:get(rounds, Options, infinity),
  InitialFood = maps:get(initial_food, Options, 1),
  MaxSerpents = maps:get(max_serpents, Options, infinity),
  Flags = maps:get(flags, Options, []),
  check(
    Rows, Cols, TickTime, Countdown, Rounds, InitialFood, MaxSerpents, Flags),
  Game0 =
    spts_games:new(
      Name, NumericId, Rows, Cols, TickTime, Countdown, Rounds, InitialFood,
      MaxSerpents, Flags),
  add_initial_cells(Game0).

%% @doc Adds a serpent to a game
-spec add_serpent(spts_games:game(), spts_serpents:name()) -> spts_games:game().
add_serpent(Game, SerpentName) ->
  case spts_games:serpent(Game, SerpentName) of
    notfound ->
      Position = find_empty_position(Game, fun is_proper_starting_point/2),
      Direction = random_direction(Game, Position),
      InitialFood = spts_games:initial_food(Game),
      NumericId = length(spts_games:serpents(Game)) + 1,
      Serpent =
        spts_serpents:new(
          SerpentName, NumericId, Position, Direction, InitialFood),
      spts_games:add_serpent(Game, Serpent);
    _ -> throw(already_in)
  end.

%% @doc Do the game allow adding another serpent?
-spec can_add_serpent(spts_games:game()) -> boolean().
can_add_serpent(Game) ->
  case { spts_games:state(Game)
       , spts_games:max_serpents(Game)
       , spts_games:serpents(Game)
       } of
    {created, infinity, _} -> true;
    {created, MaxS, Ss} when MaxS > length(Ss) -> true;
    {_, _, _} -> false
  end.

%% @doc Starts a game
-spec countdown_or_start(spts_games:game()) -> spts_games:game().
countdown_or_start(Game) ->
  case spts_games:countdown(Game) of
    0 -> spts_games:state(Game, started);
    C -> spts_games:state(spts_games:countdown(Game, C - 1), countdown)
  end.

%% @doc Registers a change in direction for a serpent
-spec turn(spts_games:game(), spts_serpents:name(), spts_games:direction()) ->
  spts_games:game().
turn(Game, SerpentName, Direction) ->
  case spts_games:serpent(Game, SerpentName) of
    notfound -> throw(invalid_serpent);
    _Serpent -> spts_games:turn(Game, SerpentName, Direction)
  end.

%% @doc moves the game
-spec advance(spts_games:game()) -> spts_games:game().
advance(Game) ->
  NewGame = spts_games:advance_serpents(Game),
  LiveSerpents = [Serpent || Serpent <- spts_games:serpents(NewGame)
                           , alive == spts_serpents:status(Serpent)],
  NewRounds =
    case spts_games:rounds(Game) of
      infinity -> infinity;
      Rounds -> Rounds - 1
    end,
  NewerGame = spts_games:rounds(NewGame, NewRounds),
  case {NewRounds, LiveSerpents} of
    {0, _} -> spts_games:state(NewerGame, finished);
    {_, []} -> spts_games:state(NewerGame, finished);
    {_, [_|_]} -> ensure_fruit(Game, NewerGame)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ensure_fruit(OldGame, Game) ->
  case spts_games:fruit(Game) of
    notfound ->
      OldFruitValue =
        case spts_games:fruit(OldGame) of
          notfound -> 0;
          {_, V} -> V
        end,
      Position = find_empty_position(Game, fun spts_games:is_empty/2),
      Food =
        case { spts_games:is_flag_on(Game, random_food)
             , spts_games:is_flag_on(Game, increasing_food)
             } of
          {false, false} -> 1;
          {false, true} -> OldFruitValue + 1;
          {true, false} -> ktn_random:uniform(10);
          {true, true} -> ktn_random:uniform(OldFruitValue, OldFruitValue + 5)
        end,
      spts_games:content(Game, Position, {fruit, Food});
    _ ->
      Game
  end.

find_empty_position(Game, Validator) ->
  Rows = spts_games:rows(Game),
  Cols = spts_games:cols(Game),
  case try_random_fep(Game, Rows, Cols, Validator, 10) of
    notfound -> walkthrough_fep(Game, Rows, Cols, Validator);
    Position -> Position
  end.

try_random_fep(_Game, _Rows, _Cols, _Validator, 0) ->
  notfound;
try_random_fep(Game, Rows, Cols, Validator, Attempts) ->
  Position = {ktn_random:uniform(Rows), ktn_random:uniform(Cols)},
  case Validator(Game, Position) of
    true -> Position;
    _ -> try_random_fep(Game, Rows, Cols, Validator, Attempts - 1)
  end.

walkthrough_fep(Game, Rows, Cols, Validator) ->
  walkthrough_fep(Game, Rows, Cols, Validator, {1, 1}).
walkthrough_fep(_Game, _Rows, _Cols, _Validator, game_full) ->
  throw(game_full);
walkthrough_fep(Game, Rows, Cols, Validator, Position = {Rows, Cols}) ->
  try_walkthrough_fep(Game, Rows, Cols, Validator, Position, game_full);
walkthrough_fep(Game, Rows, Cols, Validator, Position = {Row, Cols}) ->
  try_walkthrough_fep(Game, Rows, Cols, Validator, Position, {Row + 1, 1});
walkthrough_fep(Game, Rows, Cols, Validator, Position = {Row, Col}) ->
  try_walkthrough_fep(Game, Rows, Cols, Validator, Position, {Row, Col + 1}).

try_walkthrough_fep(Game, Rows, Cols, Validator, Position, NextPosition) ->
  case Validator(Game, Position) of
    true -> Position;
    _ -> walkthrough_fep(Game, Rows, Cols, Validator, NextPosition)
  end.

random_name() ->
  {ok, Names} =
    file:consult(filename:join(code:priv_dir(serpents), "game-names")),
  try_random_name(Names).

try_random_name(Names) ->
  NumericId = ktn_random:uniform(length(Names)),
  Name = lists:nth(NumericId, Names),
  case spts_core:is_game(Name) of
    false -> {NumericId, Name};
    true -> try_random_name(Names)
  end.

random_direction(Game, {Row, Col}) ->
  Candidates =
    surrounding_positions(
      Row, Col, spts_games:rows(Game), spts_games:cols(Game)),
  {_, Direction} =
    lists:nth(ktn_random:uniform(length(Candidates)), Candidates),
  Direction.

check(Rows, _, _, _, _, _, _, _) when Rows < 5 -> throw(invalid_rows);
check(_, Cols, _, _, _, _, _, _) when Cols < 5 -> throw(invalid_cols);
check(_, _, Tick, _, _, _, _, _) when Tick < 100 -> throw(invalid_ticktime);
check(_, _, _, Count, _, _, _, _) when Count < 0 -> throw(invalid_countdown);
check(_, _, _, _, Rounds, _, _, _) when Rounds < 100 -> throw(invalid_rounds);
check(_, _, _, _, _, Food, _, _) when Food < 0 -> throw(invalid_food);
check(_, _, _, _, _, _, MaxSpts, _) when MaxSpts < 1 -> throw(invalid_serpents);
check(_, _, _, _, _, _, _, Flags) -> lists:foreach(fun check_flag/1, Flags).

check_flag(walls) -> ok;
check_flag(random_food) -> ok;
check_flag(increasing_food) -> ok;
check_flag(_) -> throw(invalid_flag).

is_proper_starting_point(Game, {Row, Col}) ->
  SurroundingPositions =
    surrounding_positions(
      Row, Col, spts_games:rows(Game), spts_games:cols(Game)),
  lists:all(
    fun({Pos, _}) -> spts_games:is_empty(Game, Pos) end,
    [{{Row, Col}, none} | SurroundingPositions]).

surrounding_positions(Row, Col, Rows, Cols) ->
  Candidates =
    [ {Row-1, Col, up}
    , {Row+1, Col, down}
    , {Row, Col-1, left}
    , {Row, Col+1, right}
    ],
  [{{R, C}, D} || {R, C, D} <- Candidates, R > 0, C > 0, R =< Rows, C =< Cols].

add_initial_cells(Game) ->
  case spts_games:is_flag_on(Game, walls) of
    false -> Game;
    true ->
      CellCount = spts_games:rows(Game) * spts_games:cols(Game),
      WallCount = 1 + ktn_random:uniform(trunc(CellCount / 10)),
      add_initial_cells(Game, WallCount)
  end.

add_initial_cells(Game, WallCount) ->
  lists:foldl(
    fun(_, AccGame) ->
      Position = find_empty_position(AccGame, fun spts_games:is_empty/2),
      spts_games:content(AccGame, Position, wall)
    end, Game, lists:seq(1, WallCount)).

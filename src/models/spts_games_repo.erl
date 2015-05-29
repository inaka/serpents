%%% @doc Games repository
-module(spts_games_repo).
-author('elbrujohalcon@inaka.net').

-export([ create/1
        , add_serpent/2
        , countdown_or_start/1
        , turn/3
        , advance/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Creates a new game
-spec create(spts_core:options()) -> spts_games:game().
create(Options) ->
  Name = random_name(),
  Rows = maps:get(rows, Options, 20),
  Cols = maps:get(cols, Options, 20),
  TickTime = maps:get(ticktime, Options, 250),
  Countdown = maps:get(countdown, Options, 10),
  Rounds = maps:get(rounds, Options, infinity),
  InitialFood = maps:get(initial_food, Options, 0),
  validate(Rows, Cols, TickTime, Countdown, Rounds, InitialFood),
  spts_games:new(Name, Rows, Cols, TickTime, Countdown, Rounds, InitialFood).

%% @doc Adds a serpent to a game
-spec add_serpent(spts_games:game(), spts_serpents:name()) -> spts_games:game().
add_serpent(Game, SerpentName) ->
  case spts_games:serpent(Game, SerpentName) of
    notfound ->
      Position = find_empty_position(Game, fun is_proper_starting_point/2),
      Direction = random_direction(Game, Position),
      InitialFood = spts_games:initial_food(Game),
      Serpent =
        spts_serpents:new(SerpentName, Position, Direction, InitialFood),
      spts_games:add_serpent(Game, Serpent);
    _ -> throw(already_in)
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
  TickTime = spts_games:ticktime(Game),
  NewRounds =
    case spts_games:rounds(Game) of
      infinity -> infinity;
      Rounds -> Rounds - 1
    end,
  NewerGame = spts_games:rounds(NewGame, NewRounds),
  case {NewRounds, LiveSerpents} of
    {0, _} -> spts_games:state(NewerGame, finished);
    {_, []} -> spts_games:state(NewerGame, finished);
    {_, [_|_]} -> ensure_fruit(NewerGame)
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ensure_fruit(Game) ->
  case spts_games:find(Game, fruit) of
    [] ->
      Position = find_empty_position(Game, fun spts_games:is_empty/2),
      spts_games:content(Game, Position, fruit);
    [_|_] ->
      Game
  end.

%% @todo wait for ktn_random:uniform/1 and remove the seeding
find_empty_position(Game, Validator) ->
  random:seed(erlang:now()),
  Rows = spts_games:rows(Game),
  Cols = spts_games:cols(Game),
  case try_random_fep(Game, Rows, Cols, Validator, 10) of
    notfound -> walkthrough_fep(Game, Rows, Cols, Validator);
    Position -> Position
  end.

%% @todo wait for ktn_random:uniform/1 and replace random:uniform here
try_random_fep(_Game, _Rows, _Cols, _Validator, 0) ->
  notfound;
try_random_fep(Game, Rows, Cols, Validator, Attempts) ->
  Position = {random:uniform(Rows), random:uniform(Cols)},
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

%% @todo wait for ktn_random:uniform/1 and replace random:uniform here
random_name() ->
  random:seed(erlang:now()),
  {ok, Names} =
    file:consult(filename:join(code:priv_dir(serpents), "game-names")),
  try_random_name(Names).

try_random_name(Names) ->
  Name = lists:nth(random:uniform(length(Names)), Names),
  case spts_core:is_game(Name) of
    false -> Name;
    true -> try_random_name(Names -- [Name])
  end.

%% @todo wait for ktn_random:uniform/1 and replace random:uniform here
random_direction(Game, {Row, Col}) ->
  random:seed(erlang:now()),
  Candidates =
    surrounding_positions(
      Row, Col, spts_games:rows(Game), spts_games:cols(Game)),
  {_, Direction} =
    lists:nth(random:uniform(length(Candidates)), Candidates),
  Direction.

validate(Rows, _, _, _, _, _) when Rows < 5 -> throw(invalid_rows);
validate(_, Cols, _, _, _, _) when Cols < 5 -> throw(invalid_cols);
validate(_, _, Tick, _, _, _) when Tick < 100 -> throw(invalid_ticktime);
validate(_, _, _, Count, _, _) when Count < 0 -> throw(invalid_countdown);
validate(_, _, _, _, Rounds, _) when Rounds /= undefined
                                   , Rounds < 100 -> throw(invalid_rounds);
validate(_, _, _, _, _, Food) when Food < 0 -> throw(invalid_food);
validate(_, _, _, _, _, _) -> ok.

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

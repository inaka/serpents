%%% @doc Games repository
-module(serpents_games_repo).
-author('elbrujohalcon@inaka.net').

-export([ create/1
        , join/2
        , start/1
        ]).

%% @doc Creates a new game
-spec create(serpents_core:options()) -> serpents_games:game().
create(Options) ->
  Rows = maps:get(rows, Options, 20),
  Cols = maps:get(cols, Options, 20),
  TickTime = maps:get(ticktime, Options, 250),
  serpents_games:new(Rows, Cols, TickTime).

%% @doc Adds a player to a game
-spec join(serpents_games:game(), serpents_players:id()) ->
  serpents_games:game().
join(Game, PlayerId) ->
  Players = serpents_games:players(Game),
  case lists:member(PlayerId, Players) of
    true -> throw(already_joined);
    false ->
      Position = find_empty_position(Game),
      serpents_games:add_player(Game, PlayerId, Position)
  end.

%% @doc Starts a game
-spec start(serpents_games:game()) -> serpents_games:game().
start(Game) -> serpents_games:state(Game, started).

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
  case serpents_games:content(Game, Position) of
    air -> Position;
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
  case serpents_games:content(Game, Position) of
    air -> Position;
    _ -> walkthrough_fep(Game, Rows, Cols, NextPosition)
  end.

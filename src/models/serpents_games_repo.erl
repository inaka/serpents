%%% @doc Games repository
-module(serpents_games_repo).
-author('elbrujohalcon@inaka.net').

-export([ create/1
        , join/2
        ]).

%% @doc Creates a new game
-spec create(serpents_core:options()) -> serpents_games:game().
create(Options) ->
  Rows = maps:get(rows, Options, 20),
  Cols = maps:get(cols, Options, 20),
  serpents_games:new(Rows, Cols).

%% @doc Adds a player to a game
-spec join(serpents_games:game(), serpents_players:id()) ->
  serpents_games:game().
join(Game, PlayerId) ->
  Players = serpents_games:players(Game),
  case lists:member(PlayerId, Players) of
    true -> throw(already_joined);
    false ->
      Position = find_empty_position(Game),
      serpents_games:fill_cell(
        serpents_games:players(Game, [PlayerId|Players]),
        Position, player_head, PlayerId)
  end.

find_empty_position(Game) ->
  Rows = serpents_games:rows(Game),
  Cols = serpents_games:cols(Game),
  case try_random_fep(Game, Rows, Cols, 10) of
    undefined -> walkthrough_fep(Game, Rows, Cols);
    Position -> Position
  end.

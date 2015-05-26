%% @doc Games model
-module(spts_games).
-author('elbrujohalcon@inaka.net').

-type row() :: pos_integer().
-type col() :: pos_integer().
-type position() :: {row(), col()}.
-type direction() :: left | right | up | down.
-type content() :: special_content() | {serpent, spts_players:id()}.
-type special_content() :: wall | fruit.
-type cell() ::
  #{ position => position()
   , content => special_content()
   }.
-type state() :: created | countdown | started | finished.
-opaque id() :: binary().
-opaque game() ::
  #{
    id => binary(),
    serpents => [spts_serpents:serpent()],
    state => state(),
    rows => pos_integer(),
    cols => pos_integer(),
    ticktime => Millis :: pos_integer(),
    countdown => Rounds :: non_neg_integer(),
    cells => [cell()],
    created_at => dcn_datetime:datetime(),
    updated_at => dcn_datetime:datetime()
  }.
-export_type([game/0, state/0, id/0, content/0, position/0, direction/0]).

-export(
  [ new/5
  , id/1
  , rows/1
  , cols/1
  , ticktime/1
  , countdown/1
  , countdown/2
  , millis_to_start/1
  , state/1
  , serpents/1
  , serpent/2
  , is_empty/2
  , content/3
  , find/2
  , add_serpent/2
  , state/2
  , turn/3
  , advance_serpents/1
  , to_json/1
  ]).
-export([process_name/1]).

-spec new(
  binary(), pos_integer(), pos_integer(), pos_integer(), pos_integer()) ->
  game().
new(Id, Rows, Cols, TickTime, Countdown) ->
  Now = ktn_date:now_human_readable(),
  #{ id => Id
   , serpents => []
   , state => created
   , rows => Rows
   , cols => Cols
   , ticktime => TickTime
   , countdown => Countdown
   , cells => []
   , created_at => Now
   , updated_at => Now
   }.

-spec id(game()) -> id().
id(#{id := Id}) -> Id.

-spec rows(game()) -> pos_integer().
rows(#{rows := Rows}) -> Rows.

-spec cols(game()) -> pos_integer().
cols(#{cols := Cols}) -> Cols.

-spec ticktime(game()) -> pos_integer().
ticktime(#{ticktime := TickTime}) -> TickTime.

-spec countdown(game()) -> pos_integer().
countdown(#{countdown := Countdown}) -> Countdown.

-spec countdown(game(), non_neg_integer()) -> game().
countdown(Game, Countdown) -> Game#{countdown := Countdown}.

-spec millis_to_start(game()) -> pos_integer().
millis_to_start(#{ticktime := TickTime, countdown := Countdown}) ->
  TickTime * Countdown.

-spec state(game()) -> state().
state(#{state := State}) -> State.

-spec serpents(game()) -> [spts_serpents:serpent()].
serpents(#{serpents := Serpents}) -> Serpents.

-spec serpent(game(), spts_players:id()) -> spts_serpents:serpent() | notfound.
serpent(#{serpents := Serpents}, PlayerId) ->
  case [Serpent || Serpent <- Serpents
                 , spts_serpents:is_owner(Serpent, PlayerId)] of
    [] -> notfound;
    [Serpent|_] -> Serpent
  end.

%% @doc returns the content of the cell at that position
-spec is_empty(game(), position()) -> boolean().
is_empty(Game, Position) -> [] == contents(Game, Position).

%% @doc adds a content in a position
-spec content(game(), position(), special_content()) -> game().
content(Game, Position, Content) ->
  #{cells := Cells} = Game,
  Cell = #{position => Position, content => Content},
  Game#{cells := [Cell | Cells]}.

%% @doc finds the position for a content
-spec find(game(), special_content()) -> [position()].
find(Game, Content) ->
  #{cells := Cells} = Game,
  [P || #{position := P, content := C} <- Cells, C == Content].

%% @doc adds a new player to the game
-spec add_serpent(game(), spts_serpents:serpent()) -> game().
add_serpent(Game, Serpent) ->
  #{serpents := Serpents} = Game,
  Game#{serpents := [Serpent | Serpents]}.

-spec state(game(), state()) -> game().
state(Game, State) -> Game#{state => State}.

-spec turn(game(), spts_players:id(), direction()) -> game().
turn(Game, PlayerId, Direction) ->
  #{serpents := Serpents} = Game,
  {[PlayerSerpent], OtherSerpents} =
    lists:partition(
      fun(Serpent) -> spts_serpents:is_owner(Serpent, PlayerId) end,
      Serpents),
  NewSerpent = spts_serpents:direction(PlayerSerpent, Direction),
  Game#{serpents := [NewSerpent|OtherSerpents]}.

-spec process_name(id()) -> atom().
process_name(GameId) ->
  binary_to_atom(<<?MODULE_STRING, $:, GameId/binary>>, utf8).

-spec advance_serpents(game()) -> game().
advance_serpents(Game = #{state := finished}) -> Game;
advance_serpents(Game) ->
  #{serpents := Serpents} = Game,
  NewSerpents = [spts_serpents:advance(Serpent) || Serpent <- Serpents],
  FinalSerpents = kill_or_feed(Game, NewSerpents),
  FinalGame = maybe_remove_fruit(Game#{serpents := FinalSerpents}),
  case [S || S <- FinalSerpents, spts_serpents:status(S) == alive] of
    [] -> FinalGame#{state := finished};
    [_|_] -> FinalGame
  end.

-spec to_json(game()) -> map().
to_json(Game) ->
  #{cells := Cells} = Game,
  #{ id => id(Game)
   , rows => rows(Game)
   , cols => cols(Game)
   , ticktime => ticktime(Game)
   , countdown => countdown(Game)
   , serpents =>
      maps:from_list(
        [ {spts_serpents:owner(Serpent), spts_serpents:to_json(Serpent)}
        || Serpent <- serpents(Game)])
   , state => state(Game)
   , cells => [cell_to_json(Cell) || Cell <- Cells]
   }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cell_to_json(#{position := {Row, Col}, content := Content}) ->
  #{row => Row, col => Col, content => Content}.

maybe_remove_fruit(Game) ->
  #{cells := Cells} = Game,
  case [Cell || Cell = #{content := fruit} <- Cells] of
    [] -> Game;
    [FruitCell = #{position := Position}] ->
      case contents(Game, Position) of
        [fruit] -> Game;
        _ASerpentEatIt ->
          Game#{cells := Cells -- [FruitCell]}
      end
  end.

kill_or_feed(Game, Serpents) ->
  NewGame = Game#{serpents := Serpents},
  [do_kill_or_feed(NewGame, Serpent) || Serpent <- Serpents].

do_kill_or_feed(Game, Serpent) ->
  [Head | _] = spts_serpents:body(Serpent),
  Owner = spts_serpents:owner(Serpent),
  case lists:sort(contents(Game, Head)) of
    [{serpent, Owner}] -> Serpent;
    [fruit, {serpent, Owner}] -> spts_serpents:feed(Serpent);
    [_, _| _] -> spts_serpents:status(Serpent, dead)
  end.

contents(Game, Position) ->
  #{serpents := Serpents} = Game,
  SerpentsInPos =
    [ {serpent, spts_serpents:owner(Serpent)}
    || Serpent <- Serpents
     , P <- spts_serpents:body(Serpent)
     , P == Position],
  SerpentsInPos ++ contents_in_cells(Game, Position).

contents_in_cells(_Game, {0, _}) -> [wall];
contents_in_cells(_Game, {_, 0}) -> [wall];
contents_in_cells(#{rows := Rows}, {Row, _}) when Row > Rows -> [wall];
contents_in_cells(#{cols := Cols}, {_, Col}) when Col > Cols -> [wall];
contents_in_cells(#{cells := Cells}, Position) ->
  [C || #{position := P, content := C} <- Cells, P == Position].

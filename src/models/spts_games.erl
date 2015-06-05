%% @doc Games model
-module(spts_games).
-author('elbrujohalcon@inaka.net').

-type row() :: pos_integer().
-type col() :: pos_integer().
-type position() :: {row(), col()}.
-type direction() :: left | right | up | down.
-type content() :: wall | {fruit, pos_integer()}.
-type cell() ::
  #{ position => position()
   , content => content()
   }.
-type state() :: created | countdown | started | finished.
-type flag() :: walls | random_food | increasing_food.
-type id() :: binary().
-opaque game() ::
  #{ id => id()
   , numeric_id => pos_integer()
   , serpents => [spts_serpents:serpent()]
   , state => state()
   , rows => pos_integer()
   , cols => pos_integer()
   , ticktime => Millis :: pos_integer()
   , countdown => CountdownRounds :: non_neg_integer()
   , rounds => GameRounds :: infinity | non_neg_integer()
   , initial_food => non_neg_integer()
   , max_serpents => infinity | pos_integer()
   , flags => [flag()]
   , cells => [cell()]
  }.
-export_type([
  game/0, state/0, id/0, content/0, position/0, direction/0, flag/0]).

-export(
  [ new/10
  , id/1
  , numeric_id/1
  , rows/1
  , cols/1
  , ticktime/1
  , countdown/1
  , countdown/2
  , rounds/1
  , rounds/2
  , initial_food/1
  , max_serpents/1
  , flags/1
  , is_flag_on/2
  , state/1
  , serpents/1
  , serpent/2
  , serpent_by_token/2
  , is_empty/2
  , content/3
  , fruit/1
  , walls/1
  , add_serpent/2
  , state/2
  , turn/3
  , advance_serpents/1
  , to_json/1
  ]).
-export([process_name/1]).

-spec new(
  id(), pos_integer(), pos_integer(), pos_integer(), pos_integer(),
  infinity | pos_integer(), infinity | pos_integer(), non_neg_integer(),
  infinity | pos_integer(), [flag()]) -> game().
new(
  Id, NumericId, Rows, Cols, TickTime, Countdown, Rounds, InitialFood,
  MaxSerpents, Flags) ->
  #{ id => Id
   , numeric_id => NumericId
   , serpents => []
   , state => created
   , rows => Rows
   , cols => Cols
   , ticktime => TickTime
   , countdown => Countdown
   , rounds => Rounds
   , initial_food => InitialFood
   , max_serpents => MaxSerpents
   , flags => Flags
   , cells => []
   }.

-spec id(game()) -> id().
id(#{id := Id}) -> Id.

-spec numeric_id(game()) -> pos_integer().
numeric_id(#{numeric_id := NumericId}) -> NumericId.

-spec rows(game()) -> pos_integer().
rows(#{rows := Rows}) -> Rows.

-spec cols(game()) -> pos_integer().
cols(#{cols := Cols}) -> Cols.

-spec ticktime(game()) -> pos_integer().
ticktime(#{ticktime := TickTime}) -> TickTime.

-spec countdown(game()) -> non_neg_integer().
countdown(#{countdown := Countdown}) -> Countdown.

-spec countdown(game(), non_neg_integer()) -> game().
countdown(Game, Countdown) -> Game#{countdown := Countdown}.

-spec rounds(game()) -> infinity | non_neg_integer().
rounds(#{rounds := Rounds}) -> Rounds.

-spec rounds(game(), infinity | non_neg_integer()) -> game().
rounds(Game, Rounds) -> Game#{rounds := Rounds}.

-spec initial_food(game()) -> non_neg_integer().
initial_food(#{initial_food := InitialFood}) -> InitialFood.

-spec max_serpents(game()) -> infinity | pos_integer().
max_serpents(#{max_serpents := MaxSerpents}) -> MaxSerpents.

-spec flags(game()) -> [flag()].
flags(#{flags := Flags}) -> Flags.

-spec is_flag_on(game(), flag()) -> boolean().
is_flag_on(#{flags := Flags}, Flag) -> lists:member(Flag, Flags).

-spec state(game()) -> state().
state(#{state := State}) -> State.

-spec serpents(game()) -> [spts_serpents:serpent()].
serpents(#{serpents := Serpents}) -> Serpents.

-spec serpent(game(), spts_serpents:name()) ->
  spts_serpents:serpent() | notfound.
serpent(#{serpents := Serpents}, SerpentName) ->
  case [Serpent || Serpent <- Serpents
                 , SerpentName == spts_serpents:name(Serpent)] of
    [] -> notfound;
    [Serpent|_] -> Serpent
  end.

-spec serpent_by_token(game(), binary()) ->
  spts_serpents:serpent() | notfound.
serpent_by_token(#{serpents := Serpents}, SerpentToken) ->
  case [Serpent || Serpent <- Serpents
                 , SerpentToken == spts_serpents:token(Serpent)] of
    [] -> notfound;
    [Serpent|_] -> Serpent
  end.

%% @doc returns the content of the cell at that position
-spec is_empty(game(), position()) -> boolean().
is_empty(Game, Position) -> [] == contents(Game, Position).

%% @doc adds a content in a position
-spec content(game(), position(), content()) -> game().
content(Game, Position, Content) ->
  #{cells := Cells} = Game,
  Cell = #{position => Position, content => Content},
  Game#{cells := [Cell | Cells]}.

%% @doc finds the position and value for the fruit, if any
-spec fruit(game()) -> notfound | {position(), pos_integer()}.
fruit(Game) ->
  #{cells := Cells} = Game,
  case [{P, V} || #{position := P, content := {fruit, V}} <- Cells] of
    [] -> notfound;
    [{Position, Value}] -> {Position, Value}
  end.

%% @doc finds the position of the walls within then game
-spec walls(game()) -> [position()].
walls(Game) ->
  #{cells := Cells} = Game,
  [P || #{position := P, content := wall} <- Cells].

%% @doc adds a new serpent to the game
-spec add_serpent(game(), spts_serpents:serpent()) -> game().
add_serpent(Game, Serpent) ->
  #{serpents := Serpents} = Game,
  Game#{serpents := [Serpent | Serpents]}.

-spec state(game(), state()) -> game().
state(Game, State) -> Game#{state => State}.

-spec turn(game(), spts_serpents:name(), direction()) -> game().
turn(Game, SerpentName, Direction) ->
  #{serpents := Serpents} = Game,
  {[ThisSerpent], OtherSerpents} =
    lists:partition(
      fun(Serpent) -> SerpentName == spts_serpents:name(Serpent) end,
      Serpents),
  NewSerpent = spts_serpents:direction(ThisSerpent, Direction),
  Game#{serpents := [NewSerpent|OtherSerpents]}.

-spec process_name(id()) -> atom().
process_name(GameId) ->
  binary_to_atom(<<?MODULE_STRING, $:, GameId/binary>>, utf8).

-spec advance_serpents(game()) -> game().
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
   , rounds => case rounds(Game) of
                  infinity -> null;
                  Rounds -> Rounds
                end
   , initial_food => initial_food(Game)
   , max_serpents => case max_serpents(Game) of
                       infinity -> null;
                       MaxSerpents -> MaxSerpents
                     end
   , flags => flags(Game)
   , serpents => [spts_serpents:to_json(Serpent) || Serpent <- serpents(Game)]
   , state => state(Game)
   , cells => [cell_to_json(Cell) || Cell <- Cells]
   }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cell_to_json(#{position := {Row, Col}, content := {fruit, Food}}) ->
  #{row => Row, col => Col, content => fruit, value => Food};
cell_to_json(#{position := {Row, Col}, content := wall}) ->
  #{row => Row, col => Col, content => wall}.

maybe_remove_fruit(Game) ->
  #{cells := Cells} = Game,
  case [Cell || Cell = #{content := {fruit, _}} <- Cells] of
    [] -> Game;
    [FruitCell = #{position := Position}] ->
      case contents(Game, Position) of
        [{fruit, _}] -> Game;
        _ASerpentEatIt ->
          Game#{cells := Cells -- [FruitCell]}
      end
  end.

kill_or_feed(Game, Serpents) ->
  NewGame = Game#{serpents := Serpents},
  [do_kill_or_feed(NewGame, Serpent) || Serpent <- Serpents].

do_kill_or_feed(Game, Serpent) ->
  case spts_serpents:status(Serpent) of
    dead -> Serpent;
    alive ->
      [Head | _] = spts_serpents:body(Serpent),
      Name = spts_serpents:name(Serpent),
      case lists:sort(contents(Game, Head)) of
        [{serpent, Name}] -> Serpent;
        [{fruit, Food}, {serpent, Name}] -> spts_serpents:feed(Serpent, Food);
        [_, _| _] -> spts_serpents:status(Serpent, dead)
      end
  end.

contents(Game, Position) ->
  #{serpents := Serpents} = Game,
  SerpentsInPos =
    [ {serpent, spts_serpents:name(Serpent)}
    || Serpent <- Serpents
     , alive == spts_serpents:status(Serpent)
     , P <- spts_serpents:body(Serpent)
     , P == Position],
  SerpentsInPos ++ contents_in_cells(Game, Position).

contents_in_cells(_Game, {0, _}) -> [wall];
contents_in_cells(_Game, {_, 0}) -> [wall];
contents_in_cells(#{rows := Rows}, {Row, _}) when Row > Rows -> [wall];
contents_in_cells(#{cols := Cols}, {_, Col}) when Col > Cols -> [wall];
contents_in_cells(#{cells := Cells}, Position) ->
  [C || #{position := P, content := C} <- Cells, P == Position].

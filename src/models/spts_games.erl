%% @doc Games model
-module(spts_games).
-author('elbrujohalcon@inaka.net').

-type row() :: non_neg_integer().
-type col() :: non_neg_integer().
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

-type diff_type() :: state | countdown | rounds | serpents | fruit.
-opaque diff() ::
    #{ type => state
     , data => state()
     }
  | #{ type => countdown
     , data => non_neg_integer()
     }
  | #{ type => rounds
     , data => non_neg_integer()
     }
  | #{ type => serpents
     , data => [spts_serpents:serpent()]
     }
  | #{ type => fruit
     , data => {position(), pos_integer()}
     }
  .
-export_type([diff_type/0, diff/0]).

-include("binary-sizes.hrl").

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
  , diffs/2
  , to_json/1
  , to_binary/2
  , diff_to_binary/1
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

-spec diffs(game(), game()) -> [diff()].
diffs(OldGame, NewGame) ->
  lists:flatmap(
    fun(DiffType) -> diffs(OldGame, NewGame, DiffType) end,
    [state, countdown, rounds, serpents, fruit]).

diffs(OldGame, NewGame, fruit) ->
  OldFruit = fruit(OldGame),
  case fruit(NewGame) of
    OldFruit -> [];
    NewFruit -> [#{type => fruit, data => NewFruit}]
  end;
diffs(OldGame, NewGame, serpents) ->
  Sort =
    fun(SerpentA, SerpentB) ->
      spts_serpents:name(SerpentA) =< spts_serpents:name(SerpentB)
    end,
  OldSerpents = lists:sort(Sort, serpents(OldGame)),
  case lists:sort(Sort, serpents(NewGame)) of
    OldSerpents -> [];
    NewSerpents -> [#{type => serpents, data => NewSerpents}]
  end;
diffs(OldGame, NewGame, Type) ->
  OldData = maps:get(Type, OldGame),
  case maps:get(Type, NewGame) of
    OldData -> [];
    NewData -> [#{type => Type, data => NewData}]
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

-spec to_binary(game(), complete | reduced) -> iodata().
to_binary(Game, complete) ->
  Id = numeric_id(Game),
  Name = spts_binary:pascal_string(id(Game)),
  State = state_to_binary(state(Game)),
  Flags = flags_to_binary(flags(Game)),
  Cols = cols(Game),
  Rows = rows(Game),
  TickRate = application:get_env(serpents, hdp_updates_per_second, 50),
  Countdown = countdown(Game),
  Rounds = case rounds(Game) of
             infinity -> 0;
             Rs -> Rs
           end,
  InitialFood = initial_food(Game),
  MaxSerpents = get_max_serpents(Game),
  Serpents = serpents(Game),
  NumSerpents = length(Serpents),
  Walls = walls(Game),
  NumWalls = length(Walls),
  WallsBin = << <<Row:?UCHAR, Col:?UCHAR>> || {Row, Col} <- Walls >>,

  [ << Id:?USHORT
     , Name/binary
     , State:?UCHAR
     , Flags:?UCHAR
     , Cols:?UCHAR
     , Rows:?UCHAR
     , TickRate:?UCHAR
     , Countdown:?UCHAR
     , Rounds:?UINT
     , InitialFood:?UCHAR
     , MaxSerpents:?UCHAR
     , NumWalls:?USHORT
     , WallsBin/binary
     , NumSerpents:?UCHAR
     >>
  | [spts_serpents:to_binary(S, reduced) || S <- Serpents]
  ];
to_binary(Game, reduced) ->
  Id = numeric_id(Game),
  Name = spts_binary:pascal_string(id(Game)),
  State = case state(Game) of
            created -> 0;
            countdown -> 1;
            started -> 2;
            finished -> 4
          end,
  MaxSerpents = get_max_serpents(Game),
  NumSerpents = length(serpents(Game)),
  <<Id:?USHORT, Name/binary, State:?UCHAR,
    NumSerpents:?UCHAR, MaxSerpents:?UCHAR>>.

-spec diff_to_binary(diff()) -> iodata().
diff_to_binary(Diff) ->
  #{type := Type} = Diff,
  TypeBin =
    case Type of
      state     -> 0;
      countdown -> 1;
      rounds    -> 2;
      serpents  -> 3;
      fruit     -> 4
    end,
  [<<TypeBin:?UCHAR>> | diff_data_to_binary(Diff)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_max_serpents(Game) ->
  case max_serpents(Game) of
    infinity -> 255;
    Value -> Value
  end.

diff_data_to_binary(#{type := state, data := Data}) ->
  State = state_to_binary(Data),
  [<<State:?UCHAR>>];
diff_data_to_binary(#{type := countdown, data := Countdown}) ->
  [<<Countdown:?USHORT>>];
diff_data_to_binary(#{type := rounds, data := Rounds}) ->
  [<<Rounds:?UINT>>];
diff_data_to_binary(#{type := serpents, data := Serpents}) ->
  NumSerpents = length(Serpents),
  [<<NumSerpents:?UCHAR>> |
    [spts_serpents:to_binary(S, complete) || S <- Serpents]];
diff_data_to_binary(#{type := fruit, data := {{Row, Col}, Food}}) ->
  [<<Food:?UCHAR, Row:?UCHAR, Col:?UCHAR>>].

state_to_binary(created) -> 0;
state_to_binary(countdown) -> 1;
state_to_binary(started) -> 2;
state_to_binary(finished) -> 4.

flags_to_binary(Flags) ->
  lists:sum([flag_to_binary(Flag) || Flag <- Flags]).

flag_to_binary(walls) -> 1;
flag_to_binary(random_food) -> 2;
flag_to_binary(increasing_food) -> 4.

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

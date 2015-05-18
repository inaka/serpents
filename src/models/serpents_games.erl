%% @doc Games model
-module(serpents_games).
-author('elbrujohalcon@inaka.net').

-type row() :: pos_integer().
-type col() :: pos_integer().
-type position() :: {row(), col()}.
-type direction() :: left | right | up | down.
-type content() :: air | special_content() | {serpent, serpents_players:id()}.
-type special_content() :: wall | fruit.
-type cell() ::
  #{ position => position()
   , content => special_content()
   }.
-type state() :: created | started | finished.
-opaque id() :: binary().
-opaque game() ::
  #{
    id => binary(),
    serpents => [serpents_serpents:serpent()],
    state => state(),
    rows => pos_integer(),
    cols => pos_integer(),
    ticktime => pos_integer(),
    cells => [cell()],
    created_at => dcn_datetime:datetime(),
    updated_at => dcn_datetime:datetime()
  }.
-export_type([game/0, state/0, id/0, content/0, position/0, direction/0]).

-export(
  [ new/3
  , id/1
  , rows/1
  , cols/1
  , ticktime/1
  , state/1
  , serpents/1
  , serpent/2
  , content/2
  , content/3
  , find/2
  , add_serpent/2
  , state/2
  , turn/3
  , advance_serpents/1
  ]).
-export([process_name/1]).

-spec new(pos_integer(), pos_integer(), pos_integer()) -> game().
new(Rows, Cols, TickTime) ->
  Now = ktn_date:now_human_readable(),
  #{ id => uuid:uuid_to_string(uuid:get_v4(), binary_standard)
   , serpents => []
   , state => created
   , rows => Rows
   , cols => Cols
   , ticktime => TickTime
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

-spec state(game()) -> state().
state(#{state := State}) -> State.

-spec serpents(game()) -> [serpents_serpents:serpent()].
serpents(#{serpents := Serpents}) -> Serpents.

-spec serpent(game(), serpents_players:id()) ->
  serpents_serpents:serpent() | notfound.
serpent(#{serpents := Serpents}, PlayerId) ->
  case [Serpent || Serpent <- Serpents
                 , serpents_serpents:is_owner(Serpent, PlayerId)] of
    [] -> notfound;
    [Serpent|_] -> Serpent
  end.

%% @doc returns the content of the cell at that position
-spec content(game(), position()) -> content().
content(Game, Position) ->
  #{cells := Cells, serpents := Serpents} = Game,
  case [Cell || Cell = #{position := P} <- Cells, P == Position] of
    [] ->
      case [ Serpent
           || Serpent <- Serpents
            , P <- serpents_serpents:body(Serpent)
            , P == Position] of
        [] -> air;
        [S|_] -> {serpent, serpents_serpents:owner(S)}
      end;
    [#{content := Content}] -> Content
  end.

%% @doc adds a content in a position
-spec content(game(), position(), special_content()) -> game().
content(Game, Position, Content) ->
  #{cells := Cells} = Game,
  Cell = #{position => Position, content => Content},
  Game#{cells := [Cell | Cells]}.

%% @doc finds the position for a content
-spec find(game(), special_content()) -> notfound | position().
find(Game, Content) ->
  #{cells := Cells} = Game,
  case [P || #{position := P, content := C} <- Cells, C == Content] of
    [] -> notfound;
    [Position|_] -> Position
  end.

%% @doc adds a new player to the game
-spec add_serpent(game(), serpents_serpents:serpent()) -> game().
add_serpent(Game, Serpent) ->
  #{serpents := Serpents} = Game,
  Game#{serpents := [Serpent | Serpents]}.

-spec state(game(), state()) -> game().
state(Game, State) -> Game#{state => State}.

-spec turn(game(), serpents_players:id(), direction()) -> game().
turn(Game, PlayerId, Direction) ->
  #{serpents := Serpents} = Game,
  {[PlayerSerpent], OtherSerpents} =
    lists:partition(
      fun(Serpent) -> serpents_serpents:is_owner(Serpent, PlayerId) end,
      Serpents),
  NewSerpent = serpents_serpents:direction(PlayerSerpent, Direction),
  Game#{serpents := [NewSerpent|OtherSerpents]}.

-spec process_name(id()) -> atom().
process_name(GameId) ->
  binary_to_atom(<<?MODULE_STRING, $:, GameId/binary>>, utf8).

-spec advance_serpents(game()) -> game().
advance_serpents(Game) ->
  #{serpents := Serpents} = Game,
  NewSerpents = [serpents_serpents:advance(Serpent) || Serpent <- Serpents],
  FinalSerpents = kill_colliding_serpents(Game, NewSerpents),
  case [S || S <- FinalSerpents, serpents_serpents:status(S) == alive] of
    [] -> Game#{serpents := [], state := finished};
    [_|_] -> Game#{serpents := FinalSerpents}
  end.

%% @todo actually kill the colliding ones
kill_colliding_serpents(_Game, NewSerpents) -> NewSerpents.

%% @doc Games model
-module(serpents_games).
-author('elbrujohalcon@inaka.net').

-type row() :: pos_integer().
-type col() :: pos_integer().
-type position() :: {row(), col()}.
-type direction() :: serpents_serpents:direction().
-type content() :: air
                 | wall
                 | fruit
                 | {serpent, head | body, serpents_players:id()}.
-type cell() ::
  #{ position => position()
   , content => content()
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
    process => undefined | pid(),
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
  , players/1
  , process/2
  , head/2
  , serpent/2
  , content/2
  , add_player/4
  , state/2
  ]).

-spec new(pos_integer(), pos_integer(), pos_integer()) -> game().
new(Rows, _Cols, _TickTime) when Rows < 5 -> throw(invalid_rows);
new(_Rows, Cols, _TickTime) when Cols < 5 -> throw(invalid_cols);
new(_Rows, _Cols, TickTime) when TickTime < 100 -> throw(invalid_ticktime);
new(Rows, Cols, TickTime) ->
  Now = ktn_date:now_human_readable(),
  #{ id => uuid:uuid_to_string(uuid:get_v4(), binary_standard)
   , serpents => []
   , state => created
   , rows => Rows
   , cols => Cols
   , ticktime => TickTime
   , cells => []
   , process => undefined
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

-spec players(game()) -> [serpents_players:id()].
players(#{serpents := Serpents}) ->
  [serpents_serpents:owner(Serpent) || Serpent <- Serpents].

-spec serpent(game(), serpents_players:id()) ->
  serpents_serpents:serpent() | notfound.
serpent(#{serpents := Serpents}, PlayerId) ->
  case [Serpent || Serpent <- Serpents
                 , serpents_serpents:is_owner(Serpent, PlayerId)] of
    [] -> notfound;
    [Serpent|_] -> Serpent
  end.

-spec process(game(), pid()) -> game().
process(Game, Process) -> Game#{process => Process}.

%% @doc where is the head of this player's serpent
-spec head(game(), serpents_players:id()) ->
  position() | notfound.
head(#{cells := Cells}, PlayerId) ->
  Heads =
    [ Position
    || #{position := Position, content := {serpent, head, P}} <- Cells
     , P == PlayerId],
  case Heads of
    [] -> notfound;
    [H|_] -> H
  end.

%% @doc returns the content of the cell at that position
-spec content(game(), position()) -> content().
content(#{cells := Cells}, Position) ->
  case [Cell || Cell = #{position := P} <- Cells, P == Position] of
    [] -> air;
    [#{content := Content}] -> Content
  end.

%% @doc adds a new player to the game
-spec add_player(game(), serpents_players:id(), position(), direction()) ->
  game().
add_player(Game, PlayerId, Position, Direction) ->
  #{ serpents := Serpents
   , cells := Cells
   } = Game,
  Cell = #{position => Position, content => {serpent, head, PlayerId}},
  Serpent = serpents_serpents:new(PlayerId, Direction),
  Game#{serpents := [Serpent | Serpents], cells := [Cell |Cells]}.

-spec state(game(), state()) -> game().
state(Game, State) -> Game#{state => State}.

%% @doc Games model
-module(serpents_games).
-author('elbrujohalcon@inaka.net').

-type cell() ::
  #{ position => serpents_core:position()
   , content => empty | wall | price | {player, serpents_players:id()}
   }.
-type state() :: created | started | finished.
-opaque id() :: binary().
-opaque game() ::
  #{
    id => binary(),
    players => [serpents_players:id()],
    state => state(),
    rows => pos_integer(),
    cols => pos_integer(),
    full_cells => [cell()],
    process => undefined | pid(),
    created_at => dcn_datetime:datetime(),
    updated_at => dcn_datetime:datetime()
  }.
-export_type([game/0, state/0, id/0]).

-export(
  [ new/2
  , id/1
  , rows/1
  , cols/1
  , state/1
  , players/1
  , process/2
  ]).

-spec new(pos_integer(), pos_integer()) -> game().
new(Rows, _Cols) when Rows < 5 -> throw(invalid_rows);
new(_Rows, Cols) when Cols < 5 -> throw(invalid_cols);
new(Rows, Cols) ->
  Now = ktn_date:now_human_readable(),
  #{ id => uuid:uuid_to_string(uuid:get_v4(), binary_standard)
   , players => []
   , state => created
   , rows => Rows
   , cols => Cols
   , full_cells => []
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

-spec state(game()) -> state().
state(#{state := State}) -> State.

-spec players(game()) -> [serpents_players:id()].
players(#{players := Players}) -> Players.

-spec process(game(), pid()) -> game().
process(Game, Process) -> Game#{process => Process}.

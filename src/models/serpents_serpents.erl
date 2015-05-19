%% @doc Serpent model
-module(serpents_serpents).
-author('elbrujohalcon@inaka.net').

-type status() :: alive | dead.
-opaque serpent() :: #{ owner     => serpents_players:id()
                      , body      => [serpents_games:position()]
                      , direction => serpents_games:direction()
                      , food      => pos_integer()
                      , status    => status()
                      }.
-export_type([serpent/0, status/0]).

-export([new/3]).
-export([ owner/1
        , is_owner/2
        , direction/1
        , direction/2
        , body/1
        , status/2
        , status/1
        , advance/1
        ]).

-spec new(
  serpents_players:id(), serpents_games:position(),
  serpents_games:direction()) -> serpent().
new(Owner, Position, Direction) ->
  #{ owner      => Owner
   , direction  => Direction
   , body       => [Position]
   , food       => 0
   , status     => alive
   }.

-spec owner(serpent()) -> serpents_players:id().
owner(#{owner := Owner}) -> Owner.

-spec direction(serpent()) -> serpents_games:direction().
direction(#{direction := Direction}) -> Direction.

-spec is_owner(serpent(), serpents_players:id()) -> boolean().
is_owner(#{owner := Owner}, Owner) -> true;
is_owner(_, _) -> false.

-spec direction(serpent(), serpents_games:direction()) -> serpent().
direction(Serpent, Direction) -> Serpent#{direction := Direction}.

-spec body(serpent()) -> [serpents_games:position()].
body(#{body := Body}) -> Body.

-spec status(serpent()) -> status().
status(#{status := Status}) -> Status.

-spec status(serpent(), status()) -> serpent().
status(Serpent, Status) -> Serpent#{status := Status}.

-spec advance(serpent()) -> serpent().
advance(Serpent = #{status := dead}) -> Serpent;
advance(Serpent) ->
  #{body := [Head|Tail], direction := Direction, food := Food} = Serpent,
  NewHead = advance(Head, Direction),
  {NewFood, NewTail} =
    case {Food, Tail} of
      {0, []} -> {0, []};
      {0, Tail} -> {0, [Head | lists:droplast(Tail)]};
      {Food, Tail} -> {Food - 1, Tail}
    end,
  Serpent#{body := [NewHead | NewTail], food := NewFood}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

advance({Row, Col}, up) -> {Row-1, Col};
advance({Row, Col}, down) -> {Row+1, Col};
advance({Row, Col}, left) -> {Row, Col-1};
advance({Row, Col}, right) -> {Row, Col+1}.

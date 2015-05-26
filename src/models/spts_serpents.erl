%% @doc Serpent model
-module(spts_serpents).
-author('elbrujohalcon@inaka.net').

-type status() :: alive | dead.
-opaque serpent() :: #{ owner     => spts_players:id()
                      , body      => [spts_games:position()]
                      , direction => spts_games:direction()
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
        , feed/1
        , to_json/1
        ]).

-spec new(spts_players:id(), spts_games:position(), spts_games:direction()) ->
  serpent().
new(Owner, Position, Direction) ->
  #{ owner      => Owner
   , direction  => Direction
   , body       => [Position]
   , food       => 0
   , status     => alive
   }.

-spec owner(serpent()) -> spts_players:id().
owner(#{owner := Owner}) -> Owner.

-spec direction(serpent()) -> spts_games:direction().
direction(#{direction := Direction}) -> Direction.

-spec is_owner(serpent(), spts_players:id()) -> boolean().
is_owner(#{owner := Owner}, Owner) -> true;
is_owner(_, _) -> false.

-spec direction(serpent(), spts_games:direction()) -> serpent().
direction(Serpent, Direction) -> Serpent#{direction := Direction}.

-spec body(serpent()) -> [spts_games:position()].
body(#{body := Body}) -> Body.

-spec status(serpent()) -> status().
status(#{status := Status}) -> Status.

-spec status(serpent(), status()) -> serpent().
status(Serpent, Status) -> Serpent#{status := Status}.

-spec advance(serpent()) -> serpent().
advance(Serpent = #{status := dead}) -> Serpent;
advance(Serpent) ->
  #{body := Body, direction := Direction, food := Food} = Serpent,
  [Head|_] = Body,
  NewHead = advance(Head, Direction),
  {NewFood, NewTail} =
    case Food of
      0 -> {0, lists:droplast(Body)};
      Food -> {Food - 1, Body}
    end,
  Serpent#{body := [NewHead | NewTail], food := NewFood}.

-spec feed(serpent()) -> serpent().
feed(Serpent = #{food := Food}) -> Serpent#{food := Food + 1}.

-spec to_json(serpent()) -> map().
to_json(Serpent) ->
  #{ owner => spts_players:to_json(spts_players_repo:fetch(owner(Serpent)))
   , body => [[Row, Col] || {Row, Col} <- body(Serpent)]
   , status => status(Serpent)
   }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

advance({Row, Col}, up) -> {Row-1, Col};
advance({Row, Col}, down) -> {Row+1, Col};
advance({Row, Col}, left) -> {Row, Col-1};
advance({Row, Col}, right) -> {Row, Col+1}.

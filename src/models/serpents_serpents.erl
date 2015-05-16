%% @doc Serpent model
-module(serpents_serpents).
-author('elbrujohalcon@inaka.net').

-type direction() :: left | right | up | down.

-opaque serpent() :: #{ owner     => serpents_players:id()
                      , length    => pos_integer()
                      , direction => direction()
                      , food      => pos_integer()
                      }.
-export_type([serpent/0]).

-export([new/2]).
-export([ owner/1
        , is_owner/2
        , direction/1
        , direction/2
        ]).

-spec new(serpents_players:id(), direction()) -> serpent().
new(Owner, Direction) ->
  #{ owner      => Owner
   , direction  => Direction
   , length     => 1
   , food       => 0
   }.

-spec owner(serpent()) -> serpents_players:id().
owner(#{owner := Owner}) -> Owner.

-spec direction(serpent()) -> direction().
direction(#{direction := Direction}) -> Direction.

-spec is_owner(serpent(), serpents_players:id()) -> boolean().
is_owner(#{owner := Owner}, Owner) -> true;
is_owner(_, _) -> false.

-spec direction(serpent(), direction()) -> serpent().
direction(Serpent, Direction) -> Serpent#{direction := Direction}.
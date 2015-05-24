%%% @doc Players document
-module(spts_players).
-author('elbrujohalcon@inaka.net').

-opaque id() :: binary().
-type name() :: binary().
-opaque player() ::
  #{
    id => id(),
    name => binary()
  }.
-export_type([player/0, id/0, name/0]).

-export([new/1]).
-export([id/1, name/1, to_json/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc creates a player
-spec new(binary()) -> player().
new(Name) ->
  #{ id   => uuid:uuid_to_string(uuid:get_v4(), binary_standard)
   , name => Name
   }.

%% @doc json representation
-spec to_json(player()) -> map().
to_json(Player) -> Player.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ACCESSORS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc id
-spec id(player()) -> binary().
id(#{id := Id}) -> Id.

%% @doc name
-spec name(player()) -> binary().
name(#{name := Name}) -> Name.

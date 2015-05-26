%%% @doc Players document
-module(spts_players).
-author('elbrujohalcon@inaka.net').

-behaviour(sumo_doc).

-opaque id() :: binary().
-type name() :: binary().
-opaque player() ::
  #{
    id => id(),
    name => binary(),
    created_at => dcn_datetime:datetime(),
    updated_at => dcn_datetime:datetime()
  }.
-export_type([player/0, id/0, name/0]).

-export([new/1]).
-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).
-export([id/1, name/1, to_json/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE,
    [ sumo:new_field(id,            binary,   [id, not_null])
    , sumo:new_field(name,          binary,   [not_null, index])
    , sumo:new_field(created_at,    datetime, [not_null])
    , sumo:new_field(updated_at,    datetime, [not_null])
    ]).

%% @private
-spec sumo_sleep(player()) -> sumo:doc().
sumo_sleep(Player) -> Player.

%% @private
-spec sumo_wakeup(sumo:doc()) -> player().
sumo_wakeup(Doc) -> Doc.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc creates a player
-spec new(binary()) -> player().
new(Name) ->
  Now = ktn_date:now_human_readable(),
  #{ id             => undefined
   , name           => Name
   , created_at     => Now
   , updated_at     => Now
   }.

%% @doc json representation
-spec to_json(player()) -> map().
to_json(Player) ->
  #{ id => id(Player)
   , name => name(Player)
   }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ACCESSORS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc id
-spec id(player()) -> binary().
id(#{id := Id}) -> Id.

%% @doc name
-spec name(player()) -> binary().
name(#{name := Name}) -> Name.

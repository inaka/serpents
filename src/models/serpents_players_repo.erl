%%% @doc Players repository
-module(serpents_players_repo).
-author('elbrujohalcon@inaka.net').

-export([ register/1
        , is_registered/1
        ]).

%% @doc Creates a new player
-spec register(binary()) -> serpents_players:player().
register(Name) ->
  Player = serpents_players:new(Name),
  sumo:persist(serpents_players, Player).

%% @doc Is the player registered?
-spec is_registered(serpents_players:id()) -> boolean().
is_registered(PlayerId) ->
  notfound =/= sumo:find(serpents_players, PlayerId).

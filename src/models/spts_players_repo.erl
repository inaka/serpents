%%% @doc Players repository
-module(spts_players_repo).
-author('elbrujohalcon@inaka.net').

-export([ register/1
        , is_registered/1
        , fetch/1
        ]).

%% @doc Creates a new player
-spec register(binary()) -> spts_players:player().
register(Name) -> sumo:persist(spts_players, spts_players:new(Name)).

%% @doc Is the player registered?
-spec is_registered(spts_players:id()) -> boolean().
is_registered(PlayerId) -> notfound =/= fetch(PlayerId).

%% @doc Retrieves the player
-spec fetch(spts_players:id()) -> notfound | spts_players:player().
fetch(PlayerId) -> sumo:find(spts_players, PlayerId).

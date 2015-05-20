%%% @doc Players repository
-module(spts_players_repo).
-author('elbrujohalcon@inaka.net').

-export([ register/1
        , is_registered/1
        ]).

%% @doc Creates a new player
-spec register(binary()) -> spts_players:player().
register(Name) ->
  Player = spts_players:new(Name),
  sumo:persist(spts_players, Player).

%% @doc Is the player registered?
-spec is_registered(spts_players:id()) -> boolean().
is_registered(PlayerId) ->
  notfound =/= sumo:find(spts_players, PlayerId).

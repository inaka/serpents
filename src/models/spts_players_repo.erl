%%% @doc Players repository
-module(spts_players_repo).
-author('elbrujohalcon@inaka.net').

-export([ create_schema/0
        , register/1
        , is_registered/1
        , fetch/1
        ]).

%% @doc Initializes the repo table
-spec create_schema() -> ok.
create_schema() ->
  ?MODULE = ets:new(?MODULE, [public, named_table, {read_concurrency, true}]),
  ok.

%% @doc Creates a new player
-spec register(binary()) -> spts_players:player().
register(Name) ->
  Player = spts_players:new(Name),
  true = ets:insert_new(?MODULE, {spts_players:id(Player), Player}),
  Player.

%% @doc Is the player registered?
-spec is_registered(spts_players:id()) -> boolean().
is_registered(PlayerId) -> ets:member(?MODULE, PlayerId).

%% @doc Retrieves the player
-spec fetch(spts_players:id()) -> notfound | spts_players:player().
fetch(PlayerId) ->
  case ets:lookup(?MODULE, PlayerId) of
    [] -> notfound;
    [{PlayerId, Player}] -> Player
  end.

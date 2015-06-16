-module(spts_hdp_game_handler).
-author('hernanrivasacosta@gmail.com').

-behavior(gen_server).

%% API
-export([user_connected/3, get_game_users/1, timestamp/0, user_update/3,
         get_games/0, get_game_name/1]).
% Supervisors
-export([start_link/0]).
% gen_server callbacks
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

-define(UCHAR,  8/unsigned-integer).
-define(USHORT, 16/unsigned-integer).
-define(UINT,   32/unsigned-integer).

-type id()          :: integer().
-type address()     :: {inet:ip_address(), integer()}.
-type user()        :: {id(), spts_serpents:name(), address()}.
-type event()       :: {integer(), iodata()}.
-type position()    :: {integer(), integer()}.
-type game_state()  :: {[position()], [position()]}.
-type game()    :: {id(), spts_games:id(), [{user(), [event()]}], game_state()}.

-record(state, {tick = 0           :: integer(),
                users = []         :: [user()],
                games = []         :: [game()],
                tref  = undefined  :: any()}).
-type state() :: #state{}.

-type start_link_response() :: {ok, atom(), pid()} | ignore | {error, any()}.

%%==============================================================================
%% API
%%==============================================================================
-spec start_link() -> start_link_response().
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec timestamp() -> integer().
timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega * 1000000 + Sec) * 1000 + round(Micro / 1000).

-spec user_connected(binary(), address(), id()) -> {ok, id()} | error.
user_connected(Name, Address, GameId) ->
  gen_server:call(?MODULE, {user_connected, Name, Address, GameId}).

-spec get_game_users(id()) -> [{id(), spts_serpents:name()}].
get_game_users(GameId) ->
  gen_server:call(?MODULE, {get_game_users, GameId}).

-spec get_game_name(id()) -> spts_games:id().
get_game_name(GameId) ->
  gen_server:call(?MODULE, {get_game_name, GameId}).

-spec user_update(address(), integer(), integer()) -> ok.
user_update(Address, KnownServerTick, Direction) ->
  gen_server:cast(?MODULE, {user_update, Address, KnownServerTick, Direction}).

-spec get_games() -> [{id(), integer(), integer(), integer()}].
get_games() ->
  gen_server:call(?MODULE, {get_games}).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================
-spec handle_call(any(), any(), state()) -> {reply, ok, state()}.
handle_call({user_connected, Name, Address, GameId}, _From,
            State = #state{users = Users,
                           games = Games,
                           tick  = CurrentTick}) ->
  case lists:keytake(GameId, 1, Games) of
    false ->
      lager:warning("Bad game id: ~p", [GameId]),
      {reply, error, State};
    {value, {GameId, GameName, _GameUsers, _GameState} = Game, OtherGames} ->
      case handle_user_connected(Name, Address, GameName) of
        ignored ->
          lager:warning("Ignored join: ~p / ~p", [GameId, Name]),
          {reply, error, State};
        {InGameId, Name, Address} ->
          Id = GameId * 10000 + InGameId,
          NewUser = {Id, Name, Address},
          NewUsers = [NewUser | Users],
          NewGame = add_user_to_game(NewUser, Game, CurrentTick),
          NewGames = [NewGame|OtherGames],
          {reply,
           {ok, Id, GameName},
           State#state{users = NewUsers, games = NewGames}}
      end
  end;
handle_call({get_games}, _From, State = #state{games = Games}) ->
  NewGames = handle_get_games(Games),
  Reply = [get_basic_info(Game) || Game <- NewGames],
  {reply, Reply, State#state{games = NewGames}};
handle_call({get_game_users, GameId}, _From, State = #state{games = Games}) ->
  {reply, handle_get_game_users(GameId, Games), State};
handle_call({get_game_name, GameId}, _From, State = #state{games = Games}) ->
  {reply, handle_get_game_name(GameId, Games), State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(update, State = #state{tick = Tick, games = Games}) ->
  CurrentTick = Tick + 1,
  _Pid = spawn(fun() -> update_all_users(CurrentTick, Games) end),
  {noreply, State#state{tick = CurrentTick}};
handle_info({event, {serpent_added, _Serpent}}, State) ->
  % Do nothing, we already know this (we joined the guy!)
  {noreply, State};
handle_info({event, {game_started, Game}}, State = #state{games = Games,
                                                          tick  = Tick}) ->
  NewGames = handle_game_started(Game, Tick, Games),
  {noreply, State#state{games = NewGames}};
handle_info({event, {game_finished, Game}}, State = #state{games = Games}) ->
  NewGames = handle_game_finished(Game, Games),
  {noreply, State#state{games = NewGames}};
handle_info({event, {game_updated, Game}}, State = #state{games = Games,
                                                          tick  = Tick}) ->
  NewGames = handle_game_updated(Game, Tick, Games),
  {noreply, State#state{games = NewGames}};
handle_info({event, {game_countdown, Game}}, State = #state{games = Games,
                                                            tick  = Tick}) ->
  NewGames = handle_countdown(Game, Tick, Games),
  {noreply, State#state{games = NewGames}};
handle_info(Msg, State) ->
  lager:notice("received unexpected info message: ~p", [Msg]),
  {noreply, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast({user_update, Address, KnownServerTick, Direction},
            State = #state{users = Users, games = Games, tick = CurrentTick}) ->
  User = get_user_by_address(Address, Users),
  NewGames = handle_user_update(User,
                                KnownServerTick, % Last tick the user received
                                CurrentTick, % The actual tick of the server
                                Direction, % The movement direction (as a uchar)
                                Games),
  {noreply, State#state{games = NewGames}}.

% Boilerplate
-spec terminate(atom(), state()) -> ok.
terminate(_Reason, _State) -> ok.
-spec code_change(string(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%==============================================================================
%% Handlers
%%==============================================================================
-spec init(any()) -> {ok, state()}.
init([]) ->
  TRef = timer:send_interval(get_ms_per_update(), ?MODULE, update),
  {ok, #state{tick = 0, tref = TRef}}.

handle_user_connected(Name, Address, GameName) ->
  try spts_core:add_serpent(GameName, Name) of
    Serpent -> {spts_serpents:numeric_id(Serpent), Name, Address}
  catch
    _:Reason ->
      lager:warning("Unable to join game, reason: ~p", [Reason]),
      ignored
  end.

handle_get_game_users(GameId, Games) ->
  case lists:keyfind(GameId, 1, Games) of
    false ->
      [];
    {GameId, _GameName, GameUsers, _GameState} ->
      [{UserId, UserName} ||
       {{UserId, UserName, _Address} = _User, _Events} <- GameUsers]
  end.

handle_get_game_name(GameId, Games) ->
  case lists:keyfind(GameId, 1, Games) of
    false ->
      [];
    {GameId, GameName, _GameUsers, _GameState} ->
      GameName
  end.

handle_user_update(User, KnownServerTick, CurrentTick, Direction, Games) ->
  {UserId, UserName, _Address} = User,
  GameId = get_game_id(UserId, Games),
  case lists:keytake(GameId, 1, Games) of
    false ->
      lager:error("invalid update of user ~p on game ~p", [UserId, GameId]),
      Games;
    {value, {GameId, GameName, GameUsers, GameState}, Tail} ->
      % Update the direction (if needed)
      NewEvents = case get_direction_atom(Direction) of
                    ignore ->
                      [];
                    DirectionAtom ->
                      ok = spts_core:turn(GameId, UserName, DirectionAtom),
                      [build_moved(UserId, CurrentTick, Direction)]
                  end,
      NewGameUsers = send_event(NewEvents, GameUsers),
      % TODO: Optimize calling this more than once
      NewGameUsers2 = [{TheUser,
                        clear_messages_older_than(UserId,
                                                  TheUser,
                                                  Events,
                                                  KnownServerTick)} ||
                        {TheUser, Events} <- NewGameUsers],
      [{GameId, GameName, NewGameUsers2, GameState} | Tail]
  end.

handle_get_games(KnownGames) ->
  AllGameIds =
    [ {spts_games:numeric_id(Game), spts_games:id(Game)}
    || Game <- spts_core:all_games()],
  lists:foldl(fun({GameId, GameName}, Acc) ->
                NewGame = case lists:keytake(GameName, 2, KnownGames) of
                            false ->
                              spts_hdp_event_handler:subscribe(GameName),
                              {GameId, GameName, [], {[], []}};
                            {value, Game, _Tail} ->
                              Game
                          end,
                [NewGame | Acc]
              end, [], AllGameIds).

handle_game_updated(Game, CurrentTick, Games) ->
  GameId = spts_games:numeric_id(Game),
  case lists:keytake(GameId, 1, Games) of
    false ->
      Games;
    {value, {GameId, GameName, GameUsers, {OldFruit, OldWalls}}, Tail} ->
      % Get the new cell data
      NewFruit = spts_games:fruit(Game),
      NewWalls = spts_games:walls(Game),
      NewSerpentCells = get_serpent_cells(spts_games:serpents(Game)),
      NewWalls = NewWalls + NewSerpentCells,
      % Find the diffs
      FruitDiff = get_fruit_diff(OldFruit, NewFruit),
      CellDiff = get_cell_diff(OldWalls, NewWalls),
      % Make the event
      NewEvents = [build_simulation_step(CurrentTick, FruitDiff, CellDiff)],
      NewGameUsers = [{User, Events ++ NewEvents} ||
                      {User, Events} <- GameUsers],
      % Build the new state
      NewGameState = {NewFruit, NewWalls},
      % Compose the new game
      NewGame = {GameId, GameName, NewGameUsers, NewGameState},
      % Return the new game list
      [NewGame | Tail]
  end.

handle_game_started(Game, CurrentTick, Games) ->
  GameId = spts_games:numeric_id(Game),
  case lists:keytake(GameId, 1, Games) of
    false ->
      Games;
    {value, {GameId, GameName, GameUsers, {_OldFruits, OldWalls}}, Tail} ->
      % Get the new cell data
      NewWalls = spts_games:walls(Game),
      NewSerpentCells = get_serpent_cells(spts_games:serpents(Game)),
      NewWalls = NewWalls + NewSerpentCells,
      % Find the diffs
      CellDiff = get_cell_diff(OldWalls, NewWalls),
      % Make the event
      NewEvents = [build_start(CurrentTick, CellDiff)],
      NewGameUsers = [{User, Events ++ NewEvents} ||
                      {User, Events} <- GameUsers],
      % Build the new state
      NewGameState = {[], NewWalls},
      % Compose the new game
      NewGame = {GameId, GameName, NewGameUsers, NewGameState},
      % Return the new game list
      [NewGame | Tail]
  end.

handle_countdown(Game, CurrentTick, Games) ->
   GameId = spts_games:numeric_id(Game),
  case lists:keytake(GameId, 1, Games) of
    false ->
      Games;
    {value, {GameId, GameName, GameUsers, GameState}, Tail} ->
      % Make the event
      Countdown = spts_games:countdown(Game),
      NewEvents = [build_countdown(CurrentTick, Countdown)],
      NewGameUsers = [{User, Events ++ NewEvents} ||
                      {User, Events} <- GameUsers],
      % Compose the new game
      NewGame = {GameId, GameName, NewGameUsers, GameState},
      % Return the new game list
      [NewGame | Tail]
  end. 

%%==============================================================================
%% Utils
%%==============================================================================
get_serpent_cells(Serpents) ->
  get_serpent_cells(Serpents, []).
get_serpent_cells([], Cells) ->
  Cells;
get_serpent_cells([Serpent | T], Cells) ->
  NewWalls = spts_serpents:body(Serpent),
  get_serpent_cells(T, Cells ++ NewWalls).

%% @todo USE the food, Luke!
get_fruit_diff(Fruit, Fruit) -> [];
get_fruit_diff({{OldX, OldY}, _OldF}, {{NewX, NewY}, _NewF}) ->
  [{removed, OldX, OldY}, {added, NewX, NewY}].

get_cell_diff(PreviousCells, CurrentCells) ->
  AddedCells = CurrentCells -- PreviousCells,
  RemovedCells = PreviousCells -- CurrentCells,
  [{removed, X, Y} || {X, Y} <- RemovedCells] ++
  [{added, X, Y} || {X, Y} <- AddedCells].

send_event([], GameUsers) ->
  GameUsers;
send_event([_|_] = NewEvents, GameUsers) ->
  [{TheUser, Events ++ NewEvents} ||
   {TheUser, Events} <- GameUsers];
send_event(Event, GameUsers) ->
  send_event([Event], GameUsers).

get_user_by_address(Address, Users) ->
  case lists:keyfind(Address, 3, Users) of
    false ->
      undefined;
    User ->
      User
  end.

update_all_users(CurrentTick, Users) ->
  lists:foreach(fun(User) -> update_user(CurrentTick, User) end, Users).

update_user(CurrentTick, {{_Id, _Name, {Ip, Port}}, Events}) ->
  Messages = [EventBinaries || {_Tick, EventBinaries} <- Events],
  NumMessages = length(Events),
  UpdateMessage = [<<NumMessages:?UCHAR>>, Messages],
  ok = spts_hdp_handler:send_update(CurrentTick, UpdateMessage, Ip, Port).

get_ms_per_update()      -> 1000 / get_updates_per_second().
get_updates_per_second() -> 50.

add_user_to_game(NewUser, Game, CurrentTick) ->
  {GameId, GameName, GameUsers, GameState} = Game,
  % Notify all players in the game that this user joined,
  % The events need to be in cronological order, hence the '++'
  NewEvents = [build_user_joined(NewUser, CurrentTick)],
  NewGameUsers = [{User, Events ++ NewEvents} ||
                  {User, Events} <- GameUsers],
  {GameId, GameName, [{NewUser, []} | NewGameUsers], GameState}.

clear_messages_older_than(UserId, {UserId, _Name, _Address}, Events, Tick) ->
  lists:filter(fun({EvtTick, _Data}) -> Tick < EvtTick end, Events);
clear_messages_older_than(_UserId, _User, Events, _Tick) ->
  Events.

% TODO: Optimize this by storing this info on the user tuple
get_game_id(_UserId, []) ->
  undefined;
get_game_id(UserId, [{GameId, _GameName, GameUsers} | T]) ->
  case lists:keyfind(UserId, 1, GameUsers) of
    false -> get_game_id(UserId, T);
    _     -> GameId
  end.

get_basic_info({GameId, GameName, Users, _Ignore}) ->
  Game = spts_core:fetch_game(GameName),
  MaxUsers = case spts_games:max_serpents(Game) of
               infinity -> 255;
               Value -> Value
             end,
  {GameId, spts_games:ticktime(Game), length(Users), MaxUsers}.

handle_game_finished(Game, Games) ->
  GameName = spts_games:id(Game),
  case lists:keytake(GameName, 2, Games) of
    false ->
      Games;
    {value, _DiscardedGame, NewGames} ->
      NewGames
  end.

get_direction_atom(1) -> left;
get_direction_atom(2) -> right;
get_direction_atom(4) -> up;
get_direction_atom(8) -> down;
get_direction_atom(_) -> ignore.

%%==============================================================================
%% Message building
%%==============================================================================
build_user_joined(User, Tick) ->
  {Id, Name, _Adress} = User,
  NameSize = size(Name),
  JoinCommand = get_command(join),
  {Tick, [<<Tick:?USHORT,
            JoinCommand:?UCHAR,
            Id:?UINT,
            NameSize:?UCHAR>>,
          Name]}.

build_start(Tick, WallDiff) ->
  StartCommand = get_command(start),
  {Tick, [<<Tick:?USHORT,
            StartCommand:?UCHAR>>,
          build_diff(WallDiff)]}.

build_moved(UserId, Tick, Direction) ->
  MovedCommand = get_command(moved),
  {Tick, <<Tick:?USHORT, MovedCommand:?UCHAR, UserId:?UINT, Direction:?UCHAR>>}.

build_simulation_step(Tick, FruitDiff, CellDiff) ->
  SimulationStepCommand = get_command(step),
  {Tick, [<<Tick:?USHORT,
            SimulationStepCommand:?UCHAR>>,
          build_diff(FruitDiff),
          build_diff(CellDiff)]}.

build_countdown(Tick, Countdown) ->
  CountdownCommand = get_command(countdown),
  {Tick, <<Tick:?USHORT,
           CountdownCommand:?UCHAR,
           Countdown:?UCHAR>>}.

%%==============================================================================
%% Message building Utils
%%==============================================================================
build_diff(Diff) ->
  [length(Diff),
   [<<(get_cell_status(Status)):?USHORT, X:?USHORT, Y:?USHORT>> ||
    {Status, X, Y} <- Diff]].

get_command(left)      -> 0;
get_command(join)      -> 1;
get_command(moved)     -> 2;
get_command(died)      -> 3;
get_command(start)     -> 4;
get_command(step)      -> 5; % Also called turn
get_command(countdown) -> 6.

get_cell_status(added) -> 1;
get_cell_status(removed) -> 2.

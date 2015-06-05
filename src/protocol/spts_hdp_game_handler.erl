-module(spts_hdp_game_handler).
-author('hernanrivasacosta@gmail.com').

-behavior(gen_server).

%% API
-export([user_connected/3, get_game_users/1, timestamp/0, user_update/3,
         get_games/0]).
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
-type game()        :: {id(), spts_games:id(), [{user(), [event()]}]}.

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
    {value, {GameId, GameName, GameUsers}, OtherGames} ->
      case handle_user_connected(Name, Address, GameName) of
        ignored ->
          lager:warning("Ignored join: ~p / ~p", [GameId, Name]),
          {reply, error, State};
        {InGameId, Name, Address} ->
          Id = GameId * 10000 + InGameId,
          NewUser = {Id, Name, Address},
          NewUsers = [NewUser | Users],
          NewGame =
            add_user_to_game(NewUser, GameId, GameName, GameUsers, CurrentTick),
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
  {reply, handle_get_game_users(GameId, Games), State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(update, State = #state{tick = Tick, games = Games}) ->
  CurrentTick = Tick + 1,
  _Pid = spawn(fun() -> update_all_users(CurrentTick, Games) end),
  {noreply, State#state{tick = CurrentTick}};
handle_info({event, {serpent_added, _Serpent}}, State) ->
  % Do nothing, we already know this (we joined the guy!)
  {noreply, State};
handle_info({event, {game_started, Game}}, State) ->
  lager:critical("game_started: ~p", [Game]),
  %NewGameUsers = send_event(NewEvents, GameUsers),
  {noreply, State};
handle_info({event, {game_finished, Game}}, State = #state{games = Games}) ->
  lager:critical("game_finished"),
  NewGames = handle_game_finished(Game, Games),
  {noreply, State#state{games = NewGames}};
handle_info({event, {game_updated, _Game}}, State) ->
  lager:critical("game_updated"),
  {noreply, State};
handle_info({event, {game_countdown, _Game}}, State) ->
  lager:critical("game_countdown"),
  {noreply, State};
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
    {GameId, _GameName, GameUsers} ->
      [{UserId, UserName} ||
       {{UserId, UserName, _Address} = _User, _Events} <- GameUsers]
  end.

handle_user_update(User, KnownServerTick, CurrentTick, Direction, Games) ->
  {UserId, UserName, _Address} = User,
  GameId = get_game_id(UserId, Games),
  case lists:keytake(GameId, 1, Games) of
    false ->
      lager:error("invalid update of user ~p on game ~p", [UserId, GameId]),
      Games;
    {value, {GameId, GameName, GameUsers}, Tail} ->
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
      [{GameId, GameName, NewGameUsers2} | Tail]
  end.

handle_get_games(KnownGames) ->
  AllGameIds =
    [ {spts_games:numeric_id(Game), spts_games:id(Game)}
    || Game <- spts_core:all_games()],
  lists:foldl(fun({GameId, GameName}, Acc) ->
                NewGame = case lists:keytake(GameName, 2, KnownGames) of
                            false ->
                              spts_hdp_event_handler:subscribe(GameName),
                              {GameId, GameName, []};
                            {value, Game, _Tail} ->
                              Game
                          end,
                [NewGame | Acc]
              end, [], AllGameIds).

%%==============================================================================
%% Utils
%%==============================================================================
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

add_user_to_game(NewUser, GameId, GameName, GameUsers, CurrentTick) ->
  % Notify all players in the game that this user joined,
  % The events need to be in cronological order, hence the '++'
  NewEvents = [build_user_joined(NewUser, CurrentTick)],
  NewGameUsers = [{User, Events ++ NewEvents} ||
                  {User, Events} <- GameUsers],
  {GameId, GameName, [{NewUser, []} | NewGameUsers]}.

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

get_basic_info({GameId, GameName, Users}) ->
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

get_user_name(UserId, []) ->
  undefined;
get_user_name(UserId, [{{UserId, UserName, _Address}, _Events} | _Users]) ->
  UserName;
get_user_name(UserId, [_User | Users]) ->
  get_user_name(UserId, Users).

%%==============================================================================
%% Message building
%%==============================================================================
get_command(left)  -> 0;
get_command(join)  -> 1;
get_command(moved) -> 2;
get_command(died)  -> 3;
get_command(start) -> 4;
get_command(step)  -> 5.

build_user_joined(User, Tick) ->
  {Id, Name, _Adress} = User,
  NameSize = size(Name),
  JoinCommand = get_command(join),
  {Tick, [<<Tick:?USHORT,
            JoinCommand:?UCHAR,
            Id:?UINT,
            NameSize:?UCHAR>>,
          Name]}.

build_simulation_step(Tick) ->
  SimulationStepCommand = get_command(step),
  {Tick, <<Tick:?USHORT,
           SimulationStepCommand:?UCHAR>>}.

build_moved(UserId, Tick, Direction) ->
  MovedCommand = get_command(moved),
  {Tick, <<Tick:?USHORT, MovedCommand:?UCHAR, UserId:?UINT, Direction:?UCHAR>>}.

-module(spts_hdp_game_handler).
-author('hernanrivasacosta@gmail.com').
-author('elbrujohalcon@inaka.net').

-behavior(gen_server).

-export([user_connected/3, user_update/4]).
-export([start_link/1, process_name/1]).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

-include("binary-sizes.hrl").

-type address() :: {inet:ip_address(), pos_integer()}.
-record(user, { serpent_id  :: pos_integer()
              , serpent_name:: spts_serpents:name()
              , tick        :: pos_integer()
              , address     :: address()
              }).
-type user() :: #user{}.
-type step() :: no_changes | spts_games:game().
-record(state, { game_id      :: spts_games:id()
               , tick = 0     :: pos_integer()
               , ticktime     :: pos_integer()
               , users = []   :: [user()]
               , tref         :: timer:tref()
               , history = [] :: [{pos_integer(), step()}]
               }).
-type state() :: #state{}.
-export_type([address/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External API functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec user_connected(
  binary(), address(), pos_integer()) -> pos_integer().
user_connected(Name, Address, GameId) ->
  Process =
    case spts_hdp_game_sup:start_child(GameId) of
      {ok, undefined} -> process_name(GameId); % already started
      {ok, Pid} -> Pid; % just started now
      {error, InitError} -> throw(InitError)
    end,
  case gen_server:call(Process, {user_connected, Name, Address}) of
    {ok, SerpentId} -> SerpentId;
    {error, Error} -> throw(Error)
  end.

-spec user_update(
  pos_integer(), address(), integer(), spts_games:direction()|undefined) -> ok.
user_update(SerpentId, Address, LastServerTick, Direction) ->
  GameId = spts_serpents:game_id(SerpentId),
  Process = process_name(GameId),
  gen_server:cast(
    Process, {user_update, SerpentId, Address, LastServerTick, Direction}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATELY EXPORTED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link(pos_integer()) -> {ok, pid()} | ignore | {error, any()}.
start_link(GameId) ->
  Process = process_name(GameId),
  case gen_server:start_link(
        {local, Process}, ?MODULE, GameId, [{debug, [trace, log]}]) of
    {ok, Pid} -> {ok, Pid};
    {error, {already_started, _Pid}} -> ignore;
    {error, Error} -> Error
  end.

-spec process_name(pos_integer()) -> atom().
process_name(GameId) ->
  binary_to_atom(
    iolist_to_binary([?MODULE_STRING, $-, integer_to_list(GameId)]), utf8).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callback implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init(pos_integer()) -> {ok, state()} | {stop, badgame}.
init(GameNumericId) ->
  try spts_core:fetch_game(GameNumericId) of
    Game ->
      GameId = spts_games:id(Game),
      UpdatesPerSecond =
        application:get_env(serpents, hdp_updates_per_second, 50),
      TickTime =
        erlang:min(1000 div UpdatesPerSecond, spts_games:ticktime(Game)),
      {ok, TRef} = timer:send_interval(TickTime, self(), tick),
      {ok, #state{ game_id = GameId
                 , ticktime = TickTime
                 , tick = 0
                 , users = []
                 , tref = TRef
                 , history = [{0, Game}]
                 }}
  catch
    _:{badgame, GameId} ->
      lager:warning("Not a game: ~p", [GameId]),
      {stop, {badgame, GameId}}
  end.

-spec handle_call(
  {user_connected, binary(), address()}, _, state()) ->
  {reply, {ok, pos_integer()} | {error, term()}, state()} |
  {stop, normal, {error, {badgame, pos_integer()}}, state()}.
handle_call({user_connected, Name, Address}, _From, State) ->
  #state{ users   = Users
        , game_id = GameId
        , tick    = Tick
        , history = History
        } = State,
  try spts_core:add_serpent(GameId, Name) of
    Serpent ->
      SerpentId = spts_serpents:numeric_id(Serpent),
      User = #user{ serpent_id   = SerpentId
                  , serpent_name = Name
                  , address      = Address
                  , tick         = undefined
                  },
      NewUsers = [User | Users],
      CurrentTick = Tick + 1,
      Game = spts_core:fetch_game(GameId),
      NewState =
        State#state{ users    = NewUsers
                   , tick     = CurrentTick
                   , history  = [{CurrentTick, Game} | History]
                   },
      {reply, {ok, SerpentId}, NewState}
  catch
    _:{badgame, _GameId} ->
      lager:warning("Not a game: ~p", [GameId]),
      {stop, normal, {error, {badgame, GameId}}, State};
    _:Reason ->
      lager:warning("Unable to join game, reason: ~p", [Reason]),
      {reply, {error, Reason}, State}
  end.

-spec handle_info(any(), state()) ->
  {noreply, state()} | {stop, normal, state()}.
handle_info(tick, State) ->
  #state{ tick    = Tick
        , history = History
        , game_id = GameId
        } = State,
  CurrentTick = Tick + 1,
  try spts_core:fetch_game(GameId) of
    Game ->
      Step =
        case latest_game(History) of
          Game -> no_changes;
          _OldGame -> Game
        end,
      NewState =
        State#state{ tick = CurrentTick
                   , history = [{CurrentTick, Step} | History]
                   },
      spawn(fun() -> update_all_users(NewState) end),
      {noreply, NewState}
  catch
    _:{badgame, _GameId} ->
      lager:info("Game Ended: ~p", [GameId]),
      {stop, normal, State}
  end;
handle_info(Msg, State) ->
  lager:notice("received unexpected info message: ~p", [Msg]),
  {noreply, State}.

-spec handle_cast(
  {user_update, pos_integer(), address(), pos_integer(),
   undefined | spts_games:direction()}, state()) -> {noreply, state()}.
handle_cast({user_update, SerpentId, Address, LastTick, Direction}, State) ->
  #state{users = Users, game_id = GameId} = State,
  NewState =
    case lists:keytake(SerpentId, #user.serpent_id, Users) of
      false ->
        lager:warning("Invalid user ~p / Users: ~p", [SerpentId, Users]),
        State;
      {value, User, OtherUsers} ->
        NewUsers = [User#user{address = Address, tick = LastTick} | OtherUsers],
        maybe_change_direction(GameId, User#user.serpent_name, Direction),
        clean_history(State#state{users = NewUsers})
    end,
  {noreply, NewState}.

-spec terminate(atom(), state()) -> ok.
terminate(Reason, #state{game_id = GameId}) ->
  lager:notice("HDP handler for ~p terminating: ~p", [GameId, Reason]),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Unused callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec code_change(string(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
latest_game([{_Tick, no_changes} | History]) -> latest_game(History);
latest_game([{_Tick, Game} | _History]) -> Game.

historic_game(Tick, [{Tick, no_changes} | History]) -> latest_game(History);
historic_game(Tick, [{Tick, Game} | _History]) -> Game;
historic_game(undefined, [{_FirstTick, Game}]) -> Game;
historic_game(Tick, [{_NewTick, _} | History]) -> historic_game(Tick, History).

update_all_users(State) ->
  #state{users = Users} = State,
  lists:foreach(
    fun(User) ->
      spawn(fun() -> update_user(User, State) end)
    end, Users).

update_user(User, State) ->
  #user{ tick    = UserTick
       , address = Address
       } = User,
  #state{ tick    = Tick
        , history = History
        } = State,
  UserGame = historic_game(UserTick, History),
  CurrentGame = latest_game(History),
  Diffs =
    [ spts_games:diff_to_binary(Diff)
    || Diff <- spts_games:diffs(UserGame, CurrentGame)],
  NumDiffs = length(Diffs),
  Message = [<<Tick:?USHORT, NumDiffs:?UCHAR>>, Diffs],
  spts_hdp_handler:send_update(Tick, Message, Address).

clean_history(State) ->
  #state{users = Users, history = History} = State,
  case lists:keyfind(undefined, #user.tick, Users) of
    #user{} ->
      %% NOTE: Can't clean, there is a user who needs the very first game, still
      State;
    false ->
      [#user{tick = MinTick}|_] = lists:keysort(#user.tick, Users),
      PurgedHistory = purge_history(MinTick, History, []),
      State#state{history = PurgedHistory}
  end.

purge_history(MinTick, History = [{MinTick, _Game} | _Rest], Acc) ->
  lists:reverse([{MinTick, latest_game(History)} | Acc]);
purge_history(MinTick, [Newer | History], Acc) ->
  purge_history(MinTick, History, [Newer | Acc]).

maybe_change_direction(_GameId, _SerpentName, undefined) -> ok;
maybe_change_direction(GameId, SerpentName, Direction) ->
  ok = spts_core:turn(GameId, SerpentName, Direction).

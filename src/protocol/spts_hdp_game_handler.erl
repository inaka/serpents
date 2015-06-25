-module(spts_hdp_game_handler).
-author('hernanrivasacosta@gmail.com').
-author('elbrujohalcon@inaka.net').

-behavior(gen_server).
-behaviour(spts_gen_event_handler).

-export([user_connected/3, user_update/4]).
-export([start_link/1]).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).
-export([notify/2]).

-define(UCHAR,  8/unsigned-integer).
-define(USHORT, 16/unsigned-integer).
-define(UINT,   32/unsigned-integer).

-type address() :: {inet:ip_address(), pos_integer()}.
-type user() :: {pos_integer(), address()}.
-record(state, { game_id    :: spts_games:id()
               , tick = 0   :: pos_integer()
               , ticktime   :: pos_integer()
               , users = [] :: [user()]
               , tref       :: timer:tref()
               }).
-type state() :: #state{}.
-export_type([address/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External API functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec user_connected(
  binary(), address(), pos_integer()) -> {ok, pos_integer()} | error.
user_connected(Name, Address, GameId) ->
  Process =
    case spts_hdp_game_sup:start_child(GameId) of
      {ok, undefined} -> process_name(GameId); % already started
      {ok, Pid} -> Pid % just started now
    end,
  try gen_server:call(Process, {user_connected, Name, Address}) of
    {ok, InGameId} -> join_user_id(GameId, InGameId);
    {error, Error} -> throw(Error)
  catch
    _:Exception ->
      lager:error(
        "Couldn't connect ~p (~p) to ~p (~p): ~p~nStack: ~p",
        [Name, Address, GameId, Process, Exception, erlang:get_stacktrace()]),
      throw(Exception)
  end.

-spec user_update(pos_integer(), address(), integer(), integer()) -> ok.
user_update(UserId, Address, LastServerTick, Direction) ->
  {GameId, InGameId} = split_user_id(UserId),
  Process = process_name(GameId),
  gen_server:cast(
    Process, {user_update, InGameId, Address, LastServerTick, Direction}).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callback implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc sends an event to a listener
-spec notify(pid(), spts_core:event()) -> ok.
notify(Pid, Event) -> Pid ! {event, Event}.

-spec init(pos_integer()) -> {ok, state()} | {error, badgame}.
init(GameNumericId) ->
  try spts_core:fetch_game(GameNumericId) of
    Game ->
      GameId = spts_games:id(Game),
      ok = spts_gen_event_handler:subscribe(GameId, ?MODULE, self()),
      UpdatesPerSecond =
        application:get_env(serpents, hdp_updates_per_second, 50),
      TickTime =
        erlang:min(1000 div UpdatesPerSecond, spts_games:ticktime(Game)),
      TRef = timer:send_interval(TickTime, ?MODULE, update),
      {ok, #state{ game_id = GameId
                 , ticktime = TickTime
                 , tick = 0
                 , users = []
                 , tref = TRef
                 }}
  catch
    _:{badgame, GameId} ->
      lager:warning("Not a game: ~p", [GameId]),
      {error, badgame}
  end.

-spec handle_call(
  {user_connected, binary(), address()}, _, state()) ->
  {reply, {ok, pos_integer()} | {error, term()}, state()}.
handle_call({user_connected, Name, Address}, _From, State) ->
  #state{ users   = Users
        , game_id = GameId
        } = State,
  try spts_core:add_serpent(GameId, Name) of
    Serpent ->
      SerpentId = spts_serpents:numeric_id(Serpent),
      NewUsers = [{SerpentId, Address} | Users],
      {reply, {ok, SerpentId}, State#state{users = NewUsers}}
  catch
    _:Reason ->
      lager:warning("Unable to join game, reason: ~p", [Reason]),
      {reply, {error, Reason}, State}
  end.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(update, State) ->
  #state{tick = Tick} = State,
  CurrentTick = Tick + 1,
  % @todo  _Pid = spawn(fun() -> update_all_users(CurrentTick, State) end),
  {noreply, State#state{tick = CurrentTick}};
handle_info({event, {serpent_added, _Serpent}}, State) ->
  % @todo
  {noreply, State};
handle_info({event, {game_started, _Game}}, State) ->
  % @todo
  {noreply, State};
handle_info({event, {game_finished, _Game}}, State) ->
  % @todo
  {noreply, State};
handle_info({event, {game_updated, _Game}}, State) ->
  % @todo
  {noreply, State};
handle_info({event, {game_countdown, _Game}}, State) ->
  % @todo
  {noreply, State};
handle_info(Msg, State) ->
  lager:notice("received unexpected info message: ~p", [Msg]),
  {noreply, State}.

-spec handle_cast(
  {user_update, address(), pos_integer(), spts_games:direction()}, state()) ->
  {noreply, state()}.
handle_cast({user_update, _Address, _LastServerTick, _Direction}, State) ->
  % @todo
  {noreply, State}.

-spec terminate(atom(), state()) -> ok.
terminate(Reason, #state{game_id = GameId}) ->
  lager:notice("HDP handler for ~p terminating: ~p", [GameId, Reason]),
  catch spts_gen_event_handler:unsubscribe(GameId, ?MODULE, self()),
  ok.

-spec code_change(string(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
process_name(GameId) ->
  binary_to_atom(
    iolist_to_binary([?MODULE_STRING, $-, integer_to_list(GameId)]), utf8).

join_user_id(GameId, InGameId) -> GameId * 10000 + InGameId.
split_user_id(UserId) -> {UserId div 10000, UserId rem 10000}.

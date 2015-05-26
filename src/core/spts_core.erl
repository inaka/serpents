%%% @doc The core of the game
-module(spts_core).
-author('elbrujohalcon@inaka.net').

-behavior(gen_fsm).

-record(state, {game :: spts_games:game(), dispatcher :: pid()}).

-type state() :: #state{}.

-export(
  [ register_player/1
  , create_game/0
  , create_game/1
  , join_game/2
  , start_game/1
  , stop_game/1
  , turn/3
  , is_game/1
  , fetch_game/1
  , can_start/1
  , all_games/0
  , subscribe/3
  , call_handler/3
  ]).

-export([start_link/1]).
-export(
  [ created/3
  , created/2
  , ready_to_start/3
  , ready_to_start/2
  , countdown/3
  , countdown/2
  , started/3
  , started/2
  , finished/3
  , finished/2
  , init/1
  , handle_event/3
  , handle_sync_event/4
  , handle_info/3
  , terminate/3
  , code_change/4
  ]).

-type options() :: #{ rows => pos_integer()
                    , cols => pos_integer()
                    , ticktime => Milliseconds :: pos_integer()
                    , countdown => Rounds :: pos_integer()
                    }.
-export_type([options/0]).

-type event() ::
    {player_joined, spts_players:id(), spts_game:position()}
  | {game_countdown, Number::pos_integer(), MillisToStart::pos_integer()}
  | {game_started, spts_games:game()}
  | {game_updated, spts_games:game()}
  | {collision_detected, spts_serpents:serpent()}
  | {game_finished, spts_games:game()}.
-export_type([event/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Registers a new player
-spec register_player(spts_players:name()) -> spts_players:player().
register_player(Name) ->
  spts_players_repo:register(Name).

%% @equiv create_game(#{}).
-spec create_game() -> spts_games:game().
create_game() -> create_game(#{}).

%% @doc Creates a new game
-spec create_game(options()) -> spts_games:game().
create_game(Options) ->
  Game = spts_games_repo:create(Options),
  {ok, _Pid} = spts_game_sup:start_child(Game),
  Game.

%% @doc PlayerId joins GameId
-spec join_game(spts_games:id(), spts_players:id()) ->
  {spts_games:position(), spts_games:direction()}.
join_game(GameId, PlayerId) ->
  call(GameId, {join, PlayerId}).

%% @doc Can we start the game?
-spec can_start(spts_games:id()) -> boolean().
can_start(GameId) ->
  call(GameId, can_start).

%% @doc Closes the joining period for the game and starts it
-spec start_game(spts_games:id()) -> ok.
start_game(GameId) ->
  cast(GameId, start).

%% @doc Stops the game
-spec stop_game(spts_games:id()) -> ok.
stop_game(GameId) ->
  cast(GameId, stop).

%% @doc a player changes direction
-spec turn(spts_games:id(), spts_players:id(), spts_games:direction()) -> ok.
turn(GameId, PlayerId, Direction) ->
  cast(GameId, {turn, PlayerId, Direction}).

%% @doc Retrieves the status of a game
-spec fetch_game(spts_games:id()) -> spts_games:game().
fetch_game(GameId) ->
  call(GameId, fetch).

%% @doc Is this game running?
-spec is_game(spts_games:id()) -> boolean().
is_game(GameId) ->
  undefined =/= erlang:whereis(spts_games:process_name(GameId)).

%% @doc Retrieves the list of all currently held games
-spec all_games() -> [spts_games:game()].
all_games() ->
  Children = supervisor:which_children(spts_game_sup),
  Processes = [Pid || {undefined, Pid, worker, [?MODULE]} <- Children],
  lists:map(
    fun(Process) ->
      {ok, Result} = do_call(Process, fetch),
      Result
    end, Processes).

%% @doc Subscribes to the game gen_event dispatcher.
-spec subscribe(spts_games:id(), module() | {module(), term()}, term()) ->
  ok.
subscribe(GameId, Handler, Args) ->
  gen_event:add_handler(call(GameId, dispatcher), Handler, Args).

%% @doc Calls the game gen_event dispatcher.
-spec call_handler(spts_games:id(), module() | {module(), term()}, term()) ->
  term().
call_handler(GameId, Handler, Request) ->
  gen_event:call(call(GameId, dispatcher), Handler, Request).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATELY EXPORTED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link(spts_games:game()) -> {ok, pid()} | {error, term()}.
start_link(Game) ->
  Process = spts_games:process_name(spts_games:id(Game)),
  gen_fsm:start_link({local, Process}, ?MODULE, Game, [{debug, [trace, log]}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FSM CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init(spts_games:game()) -> {ok, created, state()}.
init(Game) ->
  {ok, Dispatcher} = gen_event:start_link(),
  sys:trace(Dispatcher, true),
  {ok, created, #state{game = Game, dispatcher = Dispatcher}}.

-spec handle_event(stop, atom(), state()) -> {stop, normal, state()}.
handle_event(stop, _StateName, State) -> {stop, normal, State}.

-spec handle_sync_event
  (can_start, _From, atom(), state()) ->
    {reply, {ok, boolean()}, atom(), state()};
  (fetch, _From, atom(), state()) ->
    {reply, {ok, spts_games:game()}, atom(), state()};
  (dispatcher, _From, atom(), state()) ->
    {reply, {ok, pid()}, atom(), state()}.
handle_sync_event(can_start, _From, StateName, State) ->
  {reply, {ok, StateName =/= created}, StateName, State};
handle_sync_event(fetch, _From, StateName, State) ->
  {reply, {ok, State#state.game}, StateName, State};
handle_sync_event(dispatcher, _From, StateName, State) ->
  {reply, {ok, State#state.dispatcher}, StateName, State}.

-spec handle_info(tick|term(), atom(), state()) ->
  {next_state, atom(), state()}.
handle_info(tick, countdown, State) ->
  #state{game = Game} = State,
  NewGame = spts_games_repo:countdown_or_start(Game),
  case spts_games:state(NewGame) of
    started ->
      ok = notify({game_started, NewGame}, State),
      tick(Game),
      {next_state, started, State#state{game = NewGame}};
    countdown ->
      RoundsToGo = spts_games:countdown(Game),
      MillisToStart = spts_games:millis_to_start(Game),
      ok = notify({game_countdown, RoundsToGo, MillisToStart}, State),
      tick(NewGame),
      {next_state, countdown, State#state{game = NewGame}}
  end;
handle_info(tick, started, State) ->
  #state{game = Game} = State,
  NewGame = spts_games_repo:advance(Game),
  NewState = State#state{game = NewGame},
  OldDeadSerpents =
    [S || S <- spts_games:serpents(Game), spts_serpents:status(S) == dead],
  NewDeadSerpents =
    [S || S <- spts_games:serpents(NewGame), spts_serpents:status(S) == dead],
  lists:foreach(
    fun(DeadSerpent) ->
      notify({collision_detected, DeadSerpent}, NewState)
    end, NewDeadSerpents -- OldDeadSerpents),
  case spts_games:state(NewGame) of
    finished ->
      notify({game_finished, NewGame}, NewState),
      {next_state, finished, NewState};
    started ->
      notify({game_updated, NewGame}, NewState),
      tick(NewGame),
      {next_state, started, NewState}
  end;
handle_info(Info, StateName, State) ->
  lager:notice("~p received at ~p", [Info, StateName]),
  {next_state, StateName, State}.

-spec terminate(term(), atom(), state()) -> ok.
terminate(Reason, StateName, State) ->
  catch gen_event:stop(State#state.dispatcher),
  lager:notice("Terminating in ~p with reason ~p", [StateName, Reason]).

-spec code_change(term() | {down, term()}, atom(), state(), term()) ->
    {ok, atom(), state()}.
code_change(_, StateName, State, _) -> {ok, StateName, State}.

-type join_result() ::
    {ok, {spts_games:position(), spts_games:direction()}}
  | {error, term()}.
-spec created({join, spts_players:id()} | term(), _From, state()) ->
                {reply, join_result(), created, state()}.
created({join, PlayerId}, From, State) ->
  ready_to_start({join, PlayerId}, From, State);
created(Request, _From, State) ->
  lager:warning("Invalid Request: ~p", [Request]),
  {reply, {error, invalid_state}, created, State}.

-spec created(term(), state()) -> {next_state, created, state()}.
created(Request, State) ->
  lager:warning("Invalid Request: ~p", [Request]),
  {next_state, created, State}.

-spec ready_to_start({join, spts_players:id()} | term(), _From, state()) ->
    {reply, join_result(), ready_to_start, state()}.
ready_to_start({join, PlayerId}, _From, State) ->
  #state{game = Game} = State,
  case spts_players_repo:is_registered(PlayerId) of
    false -> {reply, {error, invalid_player}, ready_to_start, State};
    true ->
      try spts_games_repo:join(Game, PlayerId) of
        NewGame ->
          Serpent = spts_games:serpent(NewGame, PlayerId),
          [Position|_] = spts_serpents:body(Serpent),
          Direction = spts_serpents:direction(Serpent),
          Response = {ok, {Position, Direction}},
          ok = notify({player_joined, PlayerId, Position}, State),
          {reply, Response, ready_to_start, State#state{game = NewGame}}
      catch
        _:Error ->
          {reply, {error, Error}, ready_to_start, State}
      end
  end;
ready_to_start(Request, _From, State) ->
  lager:warning("Invalid Request: ~p", [Request]),
  {reply, {error, invalid_state}, ready_to_start, State}.

-spec ready_to_start(
  {turn, spts_players:id(), spts_games:direction()} | start | term(),
  state()) -> {next_state, started | ready_to_start, state()}.
ready_to_start({turn, PlayerId, Direction}, State) ->
  #state{game = Game} = State,
  try spts_games_repo:turn(Game, PlayerId, Direction) of
    NewGame ->
      {next_state, ready_to_start, State#state{game = NewGame}}
  catch
    throw:invalid_player ->
      lager:warning("Invalid Turn: ~p / ~p", [PlayerId, Direction]),
      {next_state, ready_to_start, State}
  end;
ready_to_start(start, State) ->
  handle_info(tick, countdown, State);
ready_to_start(Request, State) ->
  lager:warning("Invalid Request: ~p", [Request]),
  {next_state, ready_to_start, State}.

-spec countdown(term(), _From, state()) ->
    {reply, {error, invalid_state}, countdown, state()}.
countdown(Request, _From, State) ->
  lager:warning("Invalid Request: ~p", [Request]),
  {reply, {error, invalid_state}, countdown, State}.

-spec countdown(
  {turn, spts_players:id(), spts_games:direction()} | term(),
  state()) -> {next_state, countdown, state()}.
countdown({turn, PlayerId, Direction}, State) ->
  #state{game = Game} = State,
  try spts_games_repo:turn(Game, PlayerId, Direction) of
    NewGame ->
      {next_state, countdown, State#state{game = NewGame}}
  catch
    throw:invalid_player ->
      lager:warning("Invalid Turn: ~p / ~p", [PlayerId, Direction]),
      {next_state, countdown, State}
  end;
countdown(Request, State) ->
  lager:warning("Invalid Request: ~p", [Request]),
  {next_state, countdown, State}.

-spec started(
  {turn, spts_players:id(), spts_games:direction()} | term(),
  state()) -> {next_state, started | finished, state()}.
started({turn, PlayerId, Direction}, State) ->
  #state{game = Game} = State,
  try spts_games_repo:turn(Game, PlayerId, Direction) of
    NewGame ->
      {next_state, started, State#state{game = NewGame}}
  catch
    throw:invalid_player ->
      lager:warning("Invalid Turn: ~p / ~p", [PlayerId, Direction]),
      {next_state, started, State}
  end;
started(Request, State) ->
  lager:warning("Invalid Request: ~p", [Request]),
  {next_state, started, State}.

-spec started(term(), _From, state()) ->
                {reply, {error, invalid_state}, started, state()}.
started(Request, _From, State) ->
  lager:warning("Invalid Request: ~p", [Request]),
  {reply, {error, invalid_state}, started, State}.

-spec finished(term(), state()) -> {next_state, finished, state()}.
finished(Request, State) ->
  lager:warning("Invalid Request: ~p", [Request]),
  {next_state, finished, State}.

-spec finished(term(), _From, state()) ->
                {reply, {error, invalid_state}, finished, state()}.
finished(Request, _From, State) ->
  lager:warning("Invalid Request: ~p", [Request]),
  {reply, {error, invalid_state}, finished, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
call(GameId, Event) ->
  Process = spts_games:process_name(GameId),
  try do_call(Process, Event) of
    {ok, Result} -> Result;
    {error, Error} -> throw(Error)
  catch
    _:Exception ->
      lager:error(
        "Couldn't send ~p to ~p (~p): ~p~nStack: ~p",
        [Event, GameId, Process, Exception, erlang:get_stacktrace()]),
      throw(Exception)
  end.

do_call(Process, can_start) ->
  gen_fsm:sync_send_all_state_event(Process, can_start);
do_call(Process, fetch) ->
  gen_fsm:sync_send_all_state_event(Process, fetch);
do_call(Process, dispatcher) ->
  gen_fsm:sync_send_all_state_event(Process, dispatcher);
do_call(Process, Event) ->
  gen_fsm:sync_send_event(Process, Event).

cast(GameId, stop) ->
  gen_fsm:send_all_state_event(spts_games:process_name(GameId), stop);
cast(GameId, Event) ->
  gen_fsm:send_event(spts_games:process_name(GameId), Event).

tick(Game) -> erlang:send_after(spts_games:ticktime(Game), self(), tick).

%% @todo consider using sync_notify
notify(Event, State) ->
  ok = gen_event:notify(State#state.dispatcher, Event).

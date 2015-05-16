%%% @doc The core of the game
-module(serpents_core).
-author('elbrujohalcon@inaka.net').

-behavior(gen_fsm).

-record(state, {game :: serpents_games:game(), dispatcher :: pid()}).

-type state() :: #state{}.

-export(
  [ register_player/1
  , create_game/0
  , create_game/1
  , join_game/2
  , start_game/1
  , turn/3
  , fetch_game/1
  , game_dispatcher/1
  ]).

-export([start_link/1]).
-export(
  [ created/3
  , created/2
  , ready_to_start/3
  , ready_to_start/2
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
                    , ticktime => pos_integer()
                    }.
-export_type([options/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Registers a new player
-spec register_player(serpents_players:name()) -> serpents_players:player().
register_player(Name) ->
  serpents_players_repo:register(Name).

%% @equiv create_game(#{}).
-spec create_game() -> serpents_games:game().
create_game() -> create_game(#{}).

%% @doc Creates a new game
-spec create_game(options()) -> serpents_games:game().
create_game(Options) ->
  Game = serpents_games_repo:create(Options),
  {ok, Pid} = serpents_game_sup:start_child(Game),
  serpents_games:process(Game, Pid).

%% @doc PlayerId joins GameId
-spec join_game(serpents_games:id(), serpents_players:id()) ->
  {serpents_games:position(), serpents_games:direction()}.
join_game(GameId, PlayerId) ->
  call(GameId, {join, PlayerId}).

%% @doc Closes the joining period for the game and starts it
-spec start_game(serpents_games:id()) -> ok.
start_game(GameId) ->
  cast(GameId, start).

%% @doc a player changes direction
-spec turn(
  serpents_games:id(), serpents_players:id(), serpents_games:direction()) -> ok.
turn(GameId, PlayerId, Direction) ->
  cast(GameId, {turn, PlayerId, Direction}).

%% @doc Retrieves the status of a game
-spec fetch_game(serpents_games:id()) -> serpents_games:game().
fetch_game(GameId) ->
  call(GameId, fetch).

%% @doc Retrieves the pid for the event dispatcher associated with a game.
%%      It's a gen_event dispatcher.
-spec game_dispatcher(serpents_games:id()) -> pid().
game_dispatcher(GameId) ->
  call(GameId, dispatcher).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATELY EXPORTED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link(serpents_games:game()) -> {ok, pid()} | {error, term()}.
start_link(Game) ->
  Process = process_name(serpents_games:id(Game)),
  gen_fsm:start_link({local, Process}, ?MODULE, Game, [{debug, [trace, log]}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FSM CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init(serpents_games:game()) -> {ok, created, state()}.
init(Game) ->
  {ok, Dispatcher} = gen_event:start_link(),
  NewGame = serpents_games:process(Game, self()),
  {ok, created, #state{game = NewGame, dispatcher = Dispatcher}}.

-spec handle_event(Event, atom(), state()) ->
  {stop, {unexpected, Event}, state()}.
handle_event(Event, _StateName, State) -> {stop, {unexpected, Event}, State}.

-spec handle_sync_event
  (fetch, _From, atom(), state()) ->
    {reply, serpents_games:game(), atom(), state()};
  (dispatcher, _From, atom(), state()) ->
    {reply, pid(), atom(), state()}.
handle_sync_event(fetch, _From, StateName, State) ->
  {reply, {ok, State#state.game}, StateName, State};
handle_sync_event(dispatcher, _From, StateName, State) ->
  {reply, {ok, State#state.dispatcher}, StateName, State}.

-spec handle_info(term(), atom(), state()) -> {next_state, atom(), state()}.
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
    {ok, {serpents_games:position(), serpents_games:direction()}}
  | {error, term()}.
-spec created({join, serpents_players:id()}, _From, state()) ->
                {reply, join_result(), created, state()};
             (term(), _From, state()) ->
                {reply, {error, invalid_state}, created, state()}.
created({join, PlayerId}, From, State) ->
  ready_to_start({join, PlayerId}, From, State);
created(Request, _From, State) ->
  lager:warning("Invalid Request: ~p", [Request]),
  {reply, {error, invalid_state}, created, State}.

-spec created(term(), state()) -> {next_state, created, state()}.
created(Request, State) ->
  lager:warning("Invalid Request: ~p", [Request]),
  {next_state, created, State}.

-spec ready_to_start
  ({join, serpents_players:id()}, _From, state()) ->
    {reply, join_result(), ready_to_start, state()};
  (term(), _From, state()) ->
    {reply, {error, invalid_state}, ready_to_start, state()}.
ready_to_start({join, PlayerId}, _From, State) ->
  #state{game = Game} = State,
  case serpents_players_repo:is_registered(PlayerId) of
    false -> {reply, {error, invalid_player}, ready_to_start, State};
    true ->
      try serpents_games_repo:join(Game, PlayerId) of
        NewGame ->
          Position = serpents_games:head(NewGame, PlayerId),
          Serpent = serpents_games:serpent(NewGame, PlayerId),
          Direction = serpents_serpents:direction(Serpent),
          Response = {ok, {Position, Direction}},
          {reply, Response, ready_to_start, State#state{game = NewGame}}
      catch
        _:Error ->
          {reply, {error, Error}, ready_to_start, State}
      end
  end;
ready_to_start(Request, _From, State) ->
  lager:warning("Invalid Request: ~p", [Request]),
  {reply, {error, invalid_state}, ready_to_start, State}.

-spec ready_to_start
  ({turn, serpents_players:id(), serpents_games:direction()}, state()) ->
    {next_state, ready_to_start, state()};
  (start, state()) -> {next_state, started, state()};
  (term(), state()) -> {next_state, ready_to_start, state()}.
ready_to_start({turn, PlayerId, Direction}, State) ->
  #state{game = Game} = State,
  try serpents_games_repo:turn(Game, PlayerId, Direction) of
    NewGame ->
      {next_state, ready_to_start, State#state{game = NewGame}}
  catch
    throw:invalid_player ->
      lager:warning("Invalid Turn: ~p / ~p", [PlayerId, Direction]),
      {next_state, ready_to_start, State}
  end;
ready_to_start(start, State) ->
  #state{game = Game} = State,
  NewGame = serpents_games_repo:start(Game),
  {next_state, started, State#state{game = NewGame}};
ready_to_start(Request, State) ->
  lager:warning("Invalid Request: ~p", [Request]),
  {next_state, ready_to_start, State}.

-spec started
  ({turn, serpents_players:id(), serpents_games:direction()}, state()) ->
    {next_state, started | finished, state()};
 (term(), state()) -> {next_state, started, state()}.
started({turn, PlayerId, Direction}, State) ->
  #state{game = Game} = State,
  try serpents_games_repo:turn(Game, PlayerId, Direction) of
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
  Process = process_name(GameId),
  try do_call(Process, Event) of
    ok -> ok;
    {ok, Result} -> Result;
    {error, Error} -> throw(Error)
  catch
    _:Exception ->
      lager:error(
        "Couldn't send ~p to ~p (~p): ~p~nStack: ~p",
        [Event, GameId, Process, Exception, erlang:get_stacktrace()]),
      throw(Exception)
  end.

do_call(Process, fetch) ->
  gen_fsm:sync_send_all_state_event(Process, fetch);
do_call(Process, dispatcher) ->
  gen_fsm:sync_send_all_state_event(Process, dispatcher);
do_call(Process, Event) ->
  gen_fsm:sync_send_event(Process, Event).

cast(GameId, Event) ->
  gen_fsm:send_event(process_name(GameId), Event).

process_name(GameId) ->
  binary_to_atom(<<?MODULE_STRING, $:, GameId/binary>>, utf8).

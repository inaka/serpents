%% @doc AI behaviour for spts
-module(spts_cli).

-behaviour(gen_server).

%%% gen_server callbacks
-export([
         init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3
        ]).

%%% API
-export([start/4, stop/1]).

-record(state, { socket     :: port()
               , serpent_id :: pos_integer()
               , game       :: spts_hdp:game()
               , mod        :: module()
               , mod_state  :: term()
               }).
-type state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-callback init(pos_integer(), spts_hdp:game(), term()) -> term().
-callback handle_update(
  [spts_hdp:diff()], pos_integer(), spts_hdp:game(), term()) ->
  {spts_games:direction(), term()}.
-callback terminate(term(), pos_integer(), spts_hdp:game(), term()) -> _.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start(spts_games:id(), spts_serpents:name(), module(), term()) ->
  {ok, pid()}.
start(GameId, SerpentName, Module, InitArg) ->
  NumericId = spts_games:numeric_id(spts_core:fetch_game(GameId)),
  Params =
    #{ game_id      => NumericId
     , serpent_name => SerpentName
     , module       => Module
     , init_arg     => InitArg
     },
  gen_server:start(?MODULE, Params, []).

-spec stop(pid()) -> ok.
stop(Cli) -> gen_server:cast(Cli, stop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callback implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init(map()) -> {ok, state()}.
init(Params) ->
  #{ game_id      := NumericId
   , serpent_name := SerpentName
   , module       := Module
   , init_arg     := InitArg
   } = Params,
  {ok, UdpSocket} =
    gen_udp:open(0, [{mode, binary}, {reuseaddr, true}, {active, false}]),
  ok = spts_hdp:send(UdpSocket, spts_hdp:join(1, NumericId, SerpentName)),
  {join_response, 1, _, {SerpentId, GameDesc}} = spts_hdp:recv(UdpSocket),
  ok = inet:setopts(UdpSocket, [{active, true}]),
  ModState = Module:init(SerpentId, GameDesc, InitArg),
  {ok, #state{ socket     = UdpSocket
             , serpent_id = SerpentId
             , game       = GameDesc
             , mod        = Module
             , mod_state  = ModState
             }}.

-spec handle_cast(stop, state()) -> {stop, normal, state()}.
handle_cast(stop, State) -> {stop, normal, State}.

-spec handle_info(term(), state()) ->
  {noreply, state()} | {stop, normal, state()}.
handle_info(
  {udp, UdpSocket, _Ip, _Port, Packet}, State = #state{socket = UdpSocket}) ->
  #state{ serpent_id = SerpentId
        , game       = GameDesc
        , mod        = Module
        , mod_state  = ModState
        } = State,
  {server_update, Tick, Tick, {Tick, Diffs}} = spts_hdp:parse(Packet),

  case apply_diffs(Diffs, GameDesc) of
    #{state := finished} = NewGameDesc ->
      {stop, normal, State#state{game = NewGameDesc}};
    NewGameDesc ->
      {NewDirection, NewModState} =
        Module:handle_update(Diffs, SerpentId, NewGameDesc, ModState),

      ok = spts_hdp:send(
            UdpSocket, spts_hdp:update(Tick, SerpentId, Tick, NewDirection)),

      {noreply, State#state{mod_state = NewModState, game = NewGameDesc}}
  end;
handle_info(_Info, State) -> {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(Reason, State) ->
  #state{ socket     = UdpSocket
        , serpent_id = SerpentId
        , game       = GameDesc
        , mod        = Module
        , mod_state  = ModState
        } = State,
  catch gen_udp:close(UdpSocket),
  Module:terminate(Reason, SerpentId, GameDesc, ModState).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Unused Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle_call(X, term(), state()) -> {reply, {unknown, X}, state()}.
handle_call(X, _From, State) -> {reply, {unknown, X}, State}.
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
apply_diffs(Diffs, GameDesc) ->
  lists:foldl(fun apply_diff/2, GameDesc, Diffs).

apply_diff(#{type := state, data := State}, Game) ->
  Game#{state := State};
apply_diff(#{type := countdown, data := Countdown}, Game) ->
  Game#{countdown := Countdown};
apply_diff(#{type := rounds, data := Rounds}, Game) ->
  Game#{rounds := Rounds};
apply_diff(#{type := fruit, data := Fruit}, Game) ->
  Game#{fruit => Fruit};
apply_diff(#{type := serpents, data := Serpents}, Game) ->
  Game#{serpents := Serpents}.

-module(spts_wx).

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").

-record(state, { frame        :: wx:wx_object()
               , label        :: wx:wx_object()
               , socket       :: port()
               , serpent_id   :: pos_integer()
               , tick         :: non_neg_integer()
               }).
-type state() :: #state{}.

%%% gen_server callbacks
-export([
         init/1, handle_event/2, handle_cast/2, handle_call/3,
         handle_info/2, terminate/2, code_change/3
        ]).

%%% API
-export([start/2]).

-spec start(spts_games:id(), spts_serpents:name()) ->
  wxWindow:wxWindow() | {error, term()}.
start(GameId, SerpentName) ->
  NumericId = spts_games:numeric_id(spts_core:fetch_game(GameId)),
  Params =
    #{ game_id      => NumericId
     , serpent_name => SerpentName
     },
  wx_object:start(?MODULE, Params, []).

-spec init(map()) -> {wxFrame:wxFrame(), state()}.
init(Params) ->
  #{ game_id      := NumericId
   , serpent_name := SerpentName
   } = Params,
  {ok, UdpSocket} =
    gen_udp:open(0, [{mode, binary}, {reuseaddr, true}, {active, false}]),
  ok = spts_hdp:send(UdpSocket, spts_hdp:join(1, NumericId, SerpentName)),
  {join_response, 1, _, {SerpentId, _GameDesc}} = spts_hdp:recv(UdpSocket),
  ok = inet:setopts(UdpSocket, [{active, true}]),

  {Frame, Label} = create_screen(SerpentName),

  wxWindow:show(Frame),

  {Frame, #state{ socket = UdpSocket
                , frame = Frame
                , serpent_id = SerpentId
                , label = Label
                }}.

-spec handle_event(#wx{}, state()) -> {noreply, state()}.
handle_event(
  #wx{event = #wxKey{keyCode = KeyCode}}, State)
  when KeyCode =:= ?WXK_UP
     ; KeyCode =:= ?WXK_DOWN
     ; KeyCode =:= ?WXK_LEFT
     ; KeyCode =:= ?WXK_RIGHT ->
  Direction =
    case KeyCode of
      ?WXK_UP -> up;
      ?WXK_DOWN -> down;
      ?WXK_LEFT -> left;
      ?WXK_RIGHT -> right
    end,
  #state{ label   = Label
        , socket  = UdpSocket
        , serpent_id = SerpentId
        , tick    = Tick
        } = State,

  wxStaticText:setLabel(Label, string:to_upper(atom_to_list(Direction))),
  lager:notice("Key Press: ~p~n", [lager:pr(KeyCode, ?MODULE)]),

  ok = spts_hdp:send(
        UdpSocket, spts_hdp:update(Tick, SerpentId, Tick, Direction)),

  {noreply, State};
handle_event(#wx{event = #wxKey{uniChar = $W, controlDown = true}}, State) ->
  #state{frame = Frame} = State,
  wxWindow:close(Frame),
  wxWindow:destroy(Frame),
  {stop, normal, State};
handle_event(#wx{event = #wxKey{keyCode = ?WXK_ESCAPE}}, State) ->
  #state{frame = Frame} = State,
  wxWindow:close(Frame),
  wxWindow:destroy(Frame),
  {stop, normal, State};
handle_event(#wx{event = #wxClose{}}, State) ->
  #state{frame = Frame} = State,
  ok = wxFrame:setStatusText(Frame, "Closing...",[]),
  wxWindow:destroy(Frame),
  {stop, normal, State};
handle_event(#wx{} = Event, State) ->
  lager:notice("Something happened: ~p~n", [lager:pr(Event, ?MODULE)]),
  {noreply, State}.

-spec handle_info(term(), state()) ->
  {noreply, state()} | {stop, normal, state()}.
handle_info(
  {udp, UdpSocket, _Ip, _Port, Packet}, State = #state{socket = UdpSocket}) ->
  {server_update, Tick, Tick, {Tick, _Diffs}} = spts_hdp:parse(Packet),
  %% @todo do stuff with the Diffs like detecting the game ended
  {noreply, State#state{tick = Tick}};
handle_info(Info, State) ->
  lager:warning("Info: ~p", [lager:pr(Info, ?MODULE)]),
  {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
  #state{socket = UdpSocket, frame = Frame} = State,
  catch gen_udp:close(UdpSocket),
  wxFrame:destroy(Frame),
  wx:destroy().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Unused Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Cast, State) -> {noreply, State}.
-spec handle_call(X, term(), state()) -> {reply, {unknown, X}, state()}.
handle_call(X, _From, State) -> {reply, {unknown, X}, State}.
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_screen(SerpentName) ->
  wx:new(),
  Frame =
    wxFrame:new(
      wx:null(), ?wxID_ANY, binary_to_list(SerpentName),
      [ {size, {320, 150}}
      , {style, ?wxDEFAULT_FRAME_STYLE band (bnot ?wxRESIZE_BORDER)}]),

  IconFile = "www/assets/favicon.ico",
  Icon = wxIcon:new(IconFile, [{type, ?wxBITMAP_TYPE_ICO}]),
  wxFrame:setIcon(Frame, Icon),
  wxIcon:destroy(Icon),

  wxFrame:createStatusBar(Frame,[]),
  ok = wxFrame:setStatusText(Frame, "Use arrow keys to move", []),

  wxFrame:connect(Frame, close_window, [{skip, true}]),

  Panel = wxPanel:new(Frame),
  wxPanel:connect(Panel, key_up),

  Label = wxStaticText:new(
    Panel, ?wxID_ANY, "MOVE!",
    [ {style, ?wxALIGN_CENTER_HORIZONTAL bor ?wxALIGN_CENTER_VERTICAL}
    , {size, {320, 200}}
    , {pos, {0, 0}}
    ]),
  Font = wxFont:new(90, ?wxFONTFAMILY_SWISS, ?wxNORMAL, ?wxBOLD, []),
  wxStaticText:setFont(Label, Font),
  {Frame, Label}.

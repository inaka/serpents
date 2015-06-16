-module(spts_hdp_handler).
-author('hernanrivasacosta@gmail.com').

% API
-export([start_link/0, send_update/4]).
% For internal use only
-export([loop/1, handle_udp/4]).

-define(UCHAR,  8/unsigned-integer).
-define(USHORT, 16/unsigned-integer).
-define(UINT,   32/unsigned-integer).

-record(metadata, {messageId      = 0 :: integer(),
                   userTime       = 0 :: integer(),
                   userId         = 0 :: integer(),
                   socket = undefined :: port() | undefined,
                   ip     = undefined :: inet:ip_address() | undefined,
                   port   = undefined :: integer() | undefined}).

%%==============================================================================
%% API
%%==============================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
  Port = application:get_env(serpents, udp_port, 8584),
  {ok, UdpSocket} = gen_udp:open(Port, udp_opts()),
  UdpHandler = spawn_link(?MODULE, loop, [UdpSocket]),
  register(?MODULE, UdpHandler),
  gen_udp:controlling_process(UdpSocket, UdpHandler),
  {ok, UdpHandler}.

-spec send_update(integer(), iodata(), inet:ip_address(), integer()) -> ok.
send_update(Tick, Message, Ip, Port) ->
  Flags = set_flags([update, success]),
  ?MODULE ! {send_update, [<<Flags:?UCHAR, Tick:?UINT, Tick:?USHORT>>,
                           Message], Ip, Port}.

-spec loop(port()) -> _.
loop(UdpSocket) ->
  receive
    {udp, UdpSocket, Ip, Port, Bin} ->
      _Pid = spawn(?MODULE, handle_udp, [Bin, UdpSocket, Ip, Port]),
      loop(UdpSocket);
    {send_update, Message, Ip, Port} ->
      ok = send(Message, UdpSocket, Ip, Port),
      loop(UdpSocket);
    Other ->
      % Prevent the message queue from filling
      lager:warning("unexpected message received: ~p", [Other]),
      loop(UdpSocket)
  end.

%%==============================================================================
%% Message parsing/handling
%%==============================================================================
-spec handle_udp(binary(), port(), inet:ip_address(), integer()) -> any().
handle_udp(<<Flags:?UCHAR,
             MessageId:?USHORT,
             UserTime:?USHORT,
             UserId:?USHORT,
             Message/binary>> = Data,
           UdpSocket, Ip, Port) ->
  case get_message_type(Flags band 7) of
    undefined ->
      lager:info("received bad type from ~p: ~p", [Ip, Data]);
    MessageType ->
      Metadata = #metadata{messageId = MessageId,
                           userTime = UserTime,
                           userId = UserId,
                           socket = UdpSocket,
                           ip = Ip,
                           port = Port},
      case handle_message(MessageType, Message, Metadata) of
        undefined ->
          lager:info("received bad message ~p from ~p", [Message, Ip]);
        _Ok ->
          ok
      end
  end;
% Garbage and malformed messages are simply ignored
handle_udp(MalforedMessage, _UdpSocket, Ip, _Port) ->
  lager:info("received malformed message from ~p: ~p", [Ip, MalforedMessage]),
  ok.

%% PING
handle_message(ping, _Ignored, Metadata = #metadata{messageId = MessageId,
                                                    userTime  = UserTime}) ->
  Flags = set_flags([ping, success]),
  send(<<Flags:?UCHAR,
         MessageId:?UINT,
         UserTime:?USHORT>>,
       Metadata);
%% INFO REQUESTS
handle_message(info, <<>>, Metadata = #metadata{messageId = MessageId,
                                                userTime  = UserTime}) ->
  AllGames = spts_hdp_game_handler:get_games(),
  NumGames = length(AllGames),
  Flags = set_flags([info, success]),
  send([<<Flags:?UCHAR,
          MessageId:?UINT,
          UserTime:?USHORT,
          NumGames:?UCHAR>>,
        [<<Id:?USHORT, TickRate:?UCHAR, NumPlayers:?UCHAR, MaxPlayers:?UCHAR>>
         || {Id, TickRate, NumPlayers, MaxPlayers} <- AllGames]],
       Metadata);
handle_message(info,
               <<GameId:?USHORT>>,
               Metadata = #metadata{messageId = MessageId,
                                    userTime  = UserTime}) ->
  try
    % Retrieve the game data
    GameName = spts_hdp_game_handler:get_game_name(GameId),
    Game = spts_core:fetch_game(GameName),
    Rows = spts_games:rows(Game),
    Cols = spts_games:cols(Game),
    Tickrate = spts_games:ticktime(Game),
    MaxPlayers =
      case spts_games:max_serpents(Game) of
        infinity -> 255;
        MaxS -> MaxS
      end,

    % Retrieve the game data that's stored on the game handler
    Players = spts_hdp_game_handler:get_game_users(GameId),
    NumPlayers = length(Players),
    BinPlayersInfo = [[<<Id:?UINT, (size(PlayerName)):?UCHAR>>, PlayerName] ||
                      {Id, PlayerName} <- Players],

    SuccessFlags = set_flags([info, success]),
    send([<<SuccessFlags:?UCHAR,
            MessageId:?UINT,
            UserTime:?USHORT,
            GameId:?USHORT,
            Tickrate:?UCHAR,
            Cols:?UCHAR,
            Rows:?UCHAR,
            NumPlayers:?UCHAR,
            MaxPlayers:?UCHAR>>,
            BinPlayersInfo],
           Metadata)
  catch
    A:B -> lager:warning(
            "Unexpected error ~p:~p~n~p", [A, B, erlang:get_stacktrace()]),
           ErrorFlags = set_flags([info, error]),
           ErrorReason = "unspecified",
           ErrorReasonLength = length(ErrorReason),
           send([<<ErrorFlags:?UCHAR,
                   MessageId:?UINT,
                   UserTime:?USHORT,
                   GameId:?USHORT,
                   ErrorReasonLength:?UCHAR>>,
                 ErrorReason],
                Metadata)
  end;
  
%% JOIN COMMAND
handle_message(join,
               <<GameId:?USHORT,
                 NameSize:?UCHAR,
                 Name:NameSize/binary>>,
               Metadata = #metadata{messageId = MessageId,
                                    userTime  = UserTime}) ->
  try
    % Tell the game handler that the user connected
    Address = get_address_from_metadata(Metadata),
    {ok, PlayerId, GameName} =
      spts_hdp_game_handler:user_connected(Name, Address, GameId),

    % Retrieve the game data
    Game = spts_core:fetch_game(GameName),
    Rows = spts_games:rows(Game),
    Cols = spts_games:cols(Game),
    Tickrate = spts_games:ticktime(Game),
    MaxPlayers =
      case spts_games:max_serpents(Game) of
        infinity -> 255;
        MaxS -> MaxS
      end,

    % Retrieve the game data that's stored on the game handler
    Players = spts_hdp_game_handler:get_game_users(GameId),
    NumPlayers = length(Players),
    BinPlayersInfo = [[<<Id:?UINT, (size(PlayerName)):?UCHAR>>, PlayerName] ||
                      {Id, PlayerName} <- Players],

    % Build the response
    SuccessFlags = set_flags([join, success]),
    ct:pal("~p: ~w", [NumPlayers, BinPlayersInfo]),
    send([<<SuccessFlags:?UCHAR,
            MessageId:?UINT,
            UserTime:?USHORT,
            PlayerId:?UINT,
            GameId:?USHORT,
            Tickrate:?UCHAR,
            Cols:?UCHAR,
            Rows:?UCHAR,
            NumPlayers:?UCHAR,
            MaxPlayers:?UCHAR>>,
          BinPlayersInfo],
         Metadata)
  catch
    A:B -> lager:warning(
            "Unexpected error ~p:~p~n~p", [A, B, erlang:get_stacktrace()]),
           ErrorFlags = set_flags([join, error]),
           ErrorReason = "unspecified",
           ErrorReasonLength = length(ErrorReason),
           send([<<ErrorFlags:?UCHAR,
                   MessageId:?UINT,
                   UserTime:?USHORT,
                   GameId:?USHORT,
                   ErrorReasonLength:?UCHAR>>,
                 ErrorReason],
                Metadata)
  end;
handle_message(action, <<LastUpdate:?USHORT, Direction:?UCHAR>>, Metadata) ->
  Address = get_address_from_metadata(Metadata),
  spts_hdp_game_handler:user_update(Address, LastUpdate, Direction);
handle_message(_MessageType, _Garbage, _Metadata) ->
  undefined.

%%==============================================================================
%% Utils
%%==============================================================================
udp_opts() ->
  [{mode, binary}, {reuseaddr, true}].

get_message_type(1) -> ping;
get_message_type(2) -> info;
get_message_type(3) -> join;
get_message_type(4) -> action;
get_message_type(_Garbage) -> undefined.

set_flags(Flags) ->
  set_flags(Flags, 0).
set_flags(Flags, Value) ->
  lists:foldl(fun set_flag/2, Value, Flags).

set_flag(ping, Value) -> (Value band 248) bor 1;
set_flag(info, Value) -> (Value band 248) bor 2;
set_flag(join, Value) -> (Value band 248) bor 3;
set_flag(action, Value) -> (Value band 248) bor 4;
% Update is the server side equivalent of action
set_flag(update, Value) -> set_flag(action, Value);
set_flag(success, Value) -> Value bor 128;
set_flag(error, Value) -> Value band 127.

send(Message, #metadata{socket = UdpSocket, ip = Ip, port = Port}) ->
  send(Message, UdpSocket, Ip, Port).

send(Message, UdpSocket, Ip, Port) ->
  gen_udp:send(UdpSocket, Ip, Port, Message).

get_address_from_metadata(Metadata) ->
  {Metadata#metadata.socket, Metadata#metadata.port}.

%% @doc Utility functions to deal with HDP protocol messages.
-module(spts_hdp).

-include("binary-sizes.hrl").

-define(VERY_MUCH, 9999999).

-type parser() :: default | detail.
-type type() :: ping
              | ping_response
              | info
              | info_response
              | join
              | join_response
              | client_update
              | server_update
              | error_join_response
              | error_info_response
              .

-type cell() :: {pos_integer(), pos_integer()}.

-type serpent() ::
  #{ id => pos_integer()
   , name => binary()
   , body => [cell()]
   }.

-type fruit() :: {pos_integer(), pos_integer(), pos_integer()}.

-type game() ::
  #{ id => pos_integer()
   , name => binary()
   , state => spts_games:state()
   , flags => [spts_games:flag()]
   , cols => pos_integer()
   , rows => pos_integer()
   , tickrate => non_neg_integer()
   , countdown => non_neg_integer()
   , rounds => non_neg_integer()
   , initial_food => non_neg_integer()
   , max_serpents => pos_integer()
   , walls => [cell()]
   , current_serpents => non_neg_integer()
   , serpents => [serpent()]
   , fruit => fruit()
   }.

-type diff() ::
    #{type => state,      data => spts_games:state()}
  | #{type => countdown,  data => non_neg_integer()}
  | #{type => rounds,     data => non_neg_integer()}
  | #{type => serpents,   data => [serpent()]}
  | #{type => fruit,      data => fruit()}
  .

-type message() :: pong
                 | {pos_integer(), [game()]}
                 | game()
                 | {pos_integer(), binary()}
                 | {pos_integer(), game()}
                 | {pos_integer(), [diff()]}
                 .

-export_type([parser/0, type/0, game/0, diff/0, message/0]).
-export([recv/1, recv/2, parse/1, parse/2, send/2]).
-export([join/3, ping/1, games/1, game/2, head/2, update/4]).

-spec send(port(), iodata()) -> ok.
send(UdpSocket, Message) ->
  Port = application:get_env(serpents, udp_port, 8584),
  gen_udp:send(UdpSocket, localhost, Port, Message).

-spec recv(port()) -> {type(), non_neg_integer(), non_neg_integer(), message()}.
recv(UdpSocket) -> parse(do_recv(UdpSocket)).

-spec recv(port(), parser()) ->
  {type(), non_neg_integer(), non_neg_integer(), message()}.
recv(UdpSocket, Parser) -> parse(do_recv(UdpSocket), Parser).

-spec parse(binary()) ->
  {type(), non_neg_integer(), non_neg_integer(), message()}.
parse(Packet) -> parse(Packet, default).

-spec parse(binary(), parser()) ->
  {type(), non_neg_integer(), non_neg_integer(), message()}.
parse(Packet, Parser) ->
  <<Flags:?UCHAR, MsgId:?UINT, Time:?USHORT, Message/binary>> = Packet,
  Type =
    case Flags of
      129 -> ping_response;
      130 -> info_response;
      131 -> join_response;
      132 -> server_update;
      003 -> error_join_response;
      002 -> error_info_response
    end,
  {Type, MsgId, Time, parse(Type, Parser, Message)}.

-spec join(pos_integer(), pos_integer(), binary()) -> binary().
join(MsgId, GameId, Name) ->
  H = head(join, MsgId),
  S = size(Name),
  <<H/binary, GameId:?USHORT, S:?UCHAR, Name/binary>>.

-spec game(pos_integer(), pos_integer()) -> binary().
game(MsgId, GameId) ->
  H = head(info, MsgId),
  <<H/binary, GameId:?USHORT>>.

-spec games(pos_integer()) -> binary().
games(MsgId) -> head(info, MsgId).

-spec ping(pos_integer()) -> binary().
ping(MsgId) -> head(ping, MsgId).

-spec update(
  pos_integer(), pos_integer(), pos_integer(), spts_games:direction()) ->
  binary().
update(MsgId, UserId, LastTick, Direction) ->
  H = head(client_update, MsgId, UserId),
  DirData = direction(Direction),
  <<H/binary, LastTick:?USHORT, DirData:?UCHAR>>.

-spec head(byte() | type(), pos_integer()) -> binary().
head(Flags, MsgId) ->
  head(Flags, MsgId, 0).
head(Flags, MsgId, UserId) ->
  {_, _, Nanos} = os:timestamp(),
  head(Flags, MsgId, Nanos rem 65536, UserId).
head(Flags, MsgId, UserTime, UserId) when is_atom(Flags) ->
  head(msg_type(Flags), MsgId, UserTime, UserId);
head(Flags, MsgId, UserTime, UserId) ->
  <<Flags:?UCHAR, MsgId:?UINT, UserTime:?USHORT, UserId:?UINT>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
msg_type(ping) -> 1;
msg_type(info) -> 2;
msg_type(join) -> 3;
msg_type(client_update) -> 4.

parse(ping_response, _, <<>>) -> pong;
parse(info_response, default, <<GameCount:?UCHAR, Games/binary>>) ->
  { GameCount
  , [ #{ id => GameId
       , name => Name
       , state => parse_state(State)
       , current_serpents => CurrentSerpents
       , max_serpents => MaxSerpents
       }
    || << GameId:?USHORT
        , NameSize:?UCHAR, Name:NameSize/binary
        , State:?UCHAR
        , CurrentSerpents:?UCHAR
        , MaxSerpents:?UCHAR
        >> <= Games
    ]
  };
parse(info_response, detail, GameDesc) ->
  << GameId:?USHORT
   , NameSize:?UCHAR, Name:NameSize/binary
   , State:?UCHAR
   , Flags:?UCHAR
   , Cols:?UCHAR
   , Rows:?UCHAR
   , TickRate:?UCHAR
   , Countdown:?UCHAR
   , Rounds:?UINT
   , InitialFood:?UCHAR
   , MaxSerpents:?UCHAR
   , NumWalls:?USHORT
   , Walls1:NumWalls/binary
   , Walls2:NumWalls/binary
   , CurrentSerpents:?UCHAR
   , Serpents/binary
   >> = GameDesc,
  Walls =
    case NumWalls of
      0 -> <<>>;
      NumWalls -> <<Walls1/binary, Walls2/binary>>
    end,
  #{ id => GameId
   , name => Name
   , state => parse_state(State)
   , flags => parse_flags(Flags)
   , cols => Cols
   , rows => Rows
   , tickrate => TickRate
   , countdown => Countdown
   , rounds => Rounds
   , initial_food => InitialFood
   , max_serpents => MaxSerpents
   , walls => [{Row, Col} || <<Row:?UCHAR, Col:?UCHAR>> <= Walls]
   , current_serpents => CurrentSerpents
   , serpents => parse_serpents(Serpents)
   };
parse(error_join_response, _, Error) ->
  <<GameId:?USHORT, ReasonSize:?UCHAR, Reason:ReasonSize/binary>> = Error,
  {GameId, Reason};
parse(error_info_response, _, Error) ->
  <<GameId:?USHORT, ReasonSize:?UCHAR, Reason:ReasonSize/binary>> = Error,
  {GameId, Reason};
parse(join_response, _, Response) ->
  << SerpentId:?UINT
   , GameDesc/binary
   >> = Response,
  {SerpentId, parse(info_response, detail, GameDesc)};
parse(server_update, _, <<Tick:?USHORT, _NumDiffs:?UCHAR, Diffs/binary>>) ->
  {Tick, parse_diffs(Diffs)}.

parse_diffs(<<>>) -> [];
parse_diffs(<<DiffType:?UCHAR, Rest/binary>>) ->
  Type = parse_diff_type(DiffType),
  {Data, Next} = parse_diff_data(Type, Rest),
  [#{type => Type, data => Data} | parse_diffs(Next)].

parse_diff_type(0) -> state;
parse_diff_type(1) -> countdown;
parse_diff_type(2) -> rounds;
parse_diff_type(3) -> serpents;
parse_diff_type(4) -> fruit.

parse_diff_data(state, <<State:?UCHAR, Next/binary>>) ->
  {parse_state(State), Next};
parse_diff_data(countdown, <<Countdown:?USHORT, Next/binary>>) ->
  {Countdown, Next};
parse_diff_data(rounds, <<Rounds:?UINT, Next/binary>>) ->
  {Rounds, Next};
parse_diff_data(
  fruit, <<Food:?UCHAR, Row:?UCHAR, Col:?UCHAR, Next/binary>>) ->
  {{Food, Row, Col}, Next};
parse_diff_data(serpents, <<NumSerpents:?UCHAR, Next/binary>>) ->
  parse_diff_serpents(NumSerpents, Next, []).

parse_diff_serpents(0, Next, Acc) -> {lists:reverse(Acc), Next};
parse_diff_serpents(
  N,
  << SerpentId:?UINT
   , BodyLength:?USHORT
   , Body1:BodyLength/binary
   , Body2:BodyLength/binary
   , Next/binary
   >>,
  Acc) ->
  Body = <<Body1/binary, Body2/binary>>,
  Serpent =
    #{ id => SerpentId
     , body => [{Row, Col} || <<Row:?UCHAR, Col:?UCHAR>> <= Body]
     },
  parse_diff_serpents(N-1, Next, [Serpent|Acc]).

parse_state(0) -> created;
parse_state(1) -> countdown;
parse_state(2) -> started;
parse_state(4) -> finished.

parse_flags(Flags) when Flags rem 2 == 1 ->
  lists:sort([walls | parse_flags(Flags - 1)]);
parse_flags(Flags) when Flags >= 4 ->
  lists:sort([increasing_food | parse_flags(Flags - 4)]);
parse_flags(0) -> [];
parse_flags(2) -> [random_food].

parse_serpents(Serpents) ->
  [ #{id => Id, name => Name}
  || <<Id:?UINT, NameSize:?UCHAR, Name:NameSize/binary>> <= Serpents
  ].

direction(left)  -> 1;
direction(right) -> 2;
direction(up)    -> 4;
direction(down)  -> 8.

do_recv(UdpSocket) ->
  {ok, {{127, 0, 0, 1}, _, Packet}} = gen_udp:recv(UdpSocket, ?VERY_MUCH, 1000),
  Packet.

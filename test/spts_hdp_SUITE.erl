-module(spts_hdp_SUITE).
-author('elbrujohalcon@inaka.net').

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {spts_test_utils,
         [ init_per_suite/1
         , end_per_suite/1
         ]}
       ]).

-export([ all/0
        , init_per_testcase/2
        , end_per_testcase/2
        ]).
-export([ ping/1
        ]).

-define(UCHAR,  8/unsigned-integer).
-define(USHORT, 16/unsigned-integer).
-define(UINT,   32/unsigned-integer).
-define(HEADER_LENGTH, 56).

-spec all() -> [atom()].
all() -> spts_test_utils:all(?MODULE).

-spec init_per_testcase(atom(), spts_test_utils:config()) ->
  spts_test_utils:config().
init_per_testcase(_Test, Config) ->
  Port = application:get_env(serpents, udp_port, 8584) - 1,
  {ok, UdpSocket} =
    gen_udp:open(Port, [{mode, binary}, {reuseaddr, true}, {active, false}]),
  [{socket, UdpSocket} | Config].

-spec end_per_testcase(atom(), spts_test_utils:config()) ->
  spts_test_utils:config().
end_per_testcase(_Test, Config) ->
  {value, {socket, UdpSocket}, NewConfig} = lists:keytake(socket, 1, Config),
  catch gen_udp:close(UdpSocket),
  NewConfig.

-spec ping(spts_test_utils:config()) -> {comment, []}.
ping(Config) ->
  ct:comment("A ping is sent"),
  ok = hdp_send(hdp_ping(1), Config),

  ct:comment("A ping is received"),
  {ping_response, 1, _, <<>>} = hdp_recv(Config),
  {comment, ""}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Message parsing/handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hdp_recv(Config) ->
  {socket, UdpSocket} = lists:keyfind(socket, 1, Config),
  Port = application:get_env(serpents, udp_port, 8584),
  {ok, {{127,0,0,1}, Port, Packet}} =
    gen_udp:recv(UdpSocket, ?HEADER_LENGTH, 1000),
  <<Flags:?UCHAR, MsgId:?UINT, Time:?USHORT>> = Packet,
  ct:pal(
    "Flags: ~p / MsgId: ~p, Time: ~p, Pckt: ~p",
    [Flags, MsgId, Time, Packet]),
  Type =
    case Flags of
      0 -> throw(error);
      129 -> ping_response;
      130 -> info_response;
      131 -> join_response;
      132 -> game_update
    end,
  Message = hdp_recv_msg(Type, Config),
  {Type, MsgId, Time, Message}.

hdp_recv_msg(ping_response, _Config) -> <<>>.

hdp_send(Message, Config) ->
  {socket, UdpSocket} = lists:keyfind(socket, 1, Config),
  Port = application:get_env(serpents, udp_port, 8584),
  gen_udp:send(UdpSocket, localhost, Port, Message).

hdp_ping(MsgId) -> hdp_head(1, MsgId).

hdp_head(Flags, MsgId) ->
  hdp_head(Flags, MsgId, 0).
hdp_head(Flags, MsgId, UserId) ->
  {_, _, Nanos} = os:timestamp(),
  hdp_head(Flags, MsgId, Nanos rem 65536, UserId).
hdp_head(Flags, MsgId, UserTime, UserId) ->
  <<Flags:?UCHAR, MsgId:?USHORT, UserTime:?USHORT, UserId:?USHORT>>.

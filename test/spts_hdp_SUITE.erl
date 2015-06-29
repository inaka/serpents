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
        , games/1
        , single_game/1
        , join/1
        ]).

-define(UCHAR,  8/unsigned-integer).
-define(USHORT, 16/unsigned-integer).
-define(UINT,   32/unsigned-integer).
-define(VERY_MUCH, 9999999).

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
  {ping_response, 1, _, pong} = hdp_recv(Config),

  ct:comment("A new ping is sent"),
  ok = hdp_send(hdp_ping(2), Config),

  ct:comment("A new ping is received"),
  {ping_response, 2, _, pong} = hdp_recv(Config),
  {comment, ""}.

-spec games(spts_test_utils:config()) -> {comment, []}.
games(Config) ->
  ct:comment("A games request is sent"),
  ok = hdp_send(hdp_games(1), Config),

  ct:comment("A games list is received"),
  {info_response, 1, _, {0, []}} = hdp_recv(Config),

  ct:comment("A game is created"),
  Game = spts_core:create_game(),
  GameId = spts_games:numeric_id(Game),
  GameName = spts_games:id(Game),

  ct:comment("A games request is sent"),
  ok = hdp_send(hdp_games(2), Config),

  ct:comment("A games list is received"),
  {info_response, 2, _, {1, [GameInfo]}} = hdp_recv(Config),
  #{ id := GameId
   , name := GameName
   , state := created
   , current_serpents := 0
   , max_serpents := 255
   } = GameInfo,

  {comment, ""}.

-spec single_game(spts_test_utils:config()) -> {comment, []}.
single_game(Config) ->
  ct:comment("A game request is sent for an unexistent game"),
  ok = hdp_send(hdp_game(1, 10), Config),

  ct:comment("A game description is not received"),
  {error_info_response, 1, _, {10, <<"badgame">>}} = hdp_recv(Config),

  ct:comment("A defaul game is created"),
  Game = spts_core:create_game(),
  GameId = spts_games:numeric_id(Game),
  GameName = spts_games:id(Game),

  ct:comment("A game request is sent"),
  ok = hdp_send(hdp_game(2, GameId), Config),

  ct:comment("A game description is received"),
  {info_response, 2, _, GD1} = hdp_recv(Config, detail),
  #{ id := GameId
   , name := GameName
   , state := created
   , flags := []
   , cols := 20
   , rows := 20
   , tickrate := 50
   , countdown := 10
   , rounds := 0
   , initial_food := 1
   , current_serpents := 0
   , max_serpents := 255
   , serpents := []
   } = GD1,

  ct:comment("A player joins"),
  S1Id = spts_serpents:numeric_id(spts_core:add_serpent(GameName, <<"s1">>)),

  ct:comment("A game request is sent"),
  ok = hdp_send(hdp_game(3, GameId), Config),

  ct:comment("A game description is received"),
  {info_response, 3, _, GD2} = hdp_recv(Config, detail),
  #{ id := GameId
   , current_serpents := 1
   , serpents := [{S1Id, <<"s1">>}]
   } = GD2,

  ct:comment("A second player joins"),
  S2Id = spts_serpents:numeric_id(spts_core:add_serpent(GameName, <<"s2">>)),

  ct:comment("A game request is sent"),
  ok = hdp_send(hdp_game(4, GameId), Config),

  ct:comment("A game description is received"),
  {info_response, 4, _, GD3} = hdp_recv(Config, detail),
  #{ id := GameId
   , current_serpents := 2
   , serpents := [{S2Id, <<"s2">>}, {S1Id, <<"s1">>}]
   } = GD3,

  ct:comment("A game with different options is created"),
  Options =
    #{ rows => 100
     , cols => 200
     , ticktime => 345
     , countdown => 5
     , rounds => 654
     , initial_food => 3
     , max_serpents => 12
     , flags => [walls, random_food, increasing_food]
     },
  Game2Id = spts_games:numeric_id(spts_core:create_game(Options)),

  ct:comment("A game request is sent"),
  ok = hdp_send(hdp_game(5, Game2Id), Config),

  ct:comment("A game description is received"),
  {info_response, 5, _, GD5} = hdp_recv(Config, detail),
  #{ id := Game2Id
   , state := created
   , flags := [increasing_food, random_food, walls]
   , cols := 200
   , rows := 100
   , tickrate := 50
   , countdown := 5
   , rounds := 654
   , initial_food := 3
   , current_serpents := 0
   , max_serpents := 12
   , serpents := []
   } = GD5,

  {comment, ""}.

-spec join(spts_test_utils:config()) -> {comment, []}.
join(Config) ->
  ct:comment("A join request is sent for an unexistent game"),
  ok = hdp_send(hdp_join(1, 10, <<"s1">>), Config),

  ct:comment("A game description is not received"),
  {error_join_response, 1, _, {10, <<"badgame">>}} = hdp_recv(Config),

  ct:comment("A game is created"),
  Game = spts_core:create_game(),
  GameId = spts_games:numeric_id(Game),
  GameName = spts_games:id(Game),
  update_game_list(Config),

  ct:comment("A player joins"),
  ok = hdp_send(hdp_join(2, GameId, <<"s1">>), Config),
  {join_response, 2, _, {S1Id, GD1}} = hdp_recv(Config),
  #{ id := GameId
   , name := GameName
   , state := created
   , flags := []
   , cols := 20
   , rows := 20
   , tickrate := 50
   , countdown := 10
   , rounds := 0
   , initial_food := 1
   , current_serpents := 1
   , max_serpents := 255
   , serpents := [{S1Id, <<"s1">>}]
   } = GD1,
  [Serpent1] = spts_games:serpents(spts_core:fetch_game(GameName)),
  {S1Id, S1Id} = {S1Id, GameId * 10000 + spts_serpents:numeric_id(Serpent1)},

  ct:comment("A player tries to join again"),
  ok = hdp_send(hdp_join(3, GameId, <<"s1">>), Config),
  {error_join_response, 3, _, {GameId, <<"unspecified">>}} = hdp_recv(Config),
  [Serpent1] = spts_games:serpents(spts_core:fetch_game(GameName)),

  ct:comment("Another player joins"),
  ok = hdp_send(hdp_join(3, GameId, <<"s2">>), Config),
  {join_response, 3, _, GD2} = hdp_recv(Config),
  {S2Id, GameId, 250, 20, 20, 255, [{S2Id, <<"s2">>}, {S1Id, <<"s1">>}]} = GD2,
  Serpent2 = spts_games:serpent(spts_core:fetch_game(GameName), <<"s2">>),
  {S2Id, S2Id} = {S2Id, GameId * 10000 + spts_serpents:numeric_id(Serpent2)},

  ct:comment("The game starts"),
  spts_core:start_game(GameName),

  ct:comment("Nobody can join anymore"),
  ok = hdp_send(hdp_join(4, GameId, <<"s3">>), Config),
  {error_join_response, 4, _, {GameId, <<"unspecified">>}} = hdp_recv(Config),
  [_, _] = spts_games:serpents(spts_core:fetch_game(GameName)),

  {comment, ""}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Message parsing/handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hdp_recv(Config) ->
  hdp_recv(Config, default).
hdp_recv(Config, Parser) ->
  {socket, UdpSocket} = lists:keyfind(socket, 1, Config),
  Port = application:get_env(serpents, udp_port, 8584),
  {ok, {{127, 0, 0, 1}, Port, Packet}} =
    gen_udp:recv(UdpSocket, ?VERY_MUCH, 1000),
  <<Flags:?UCHAR, MsgId:?UINT, Time:?USHORT, Message/binary>> = Packet,
  ct:pal(
    "Flags: ~p / MsgId: ~p, Time: ~p, Pckt: ~p",
    [Flags, MsgId, Time, Packet]),
  Type =
    case Flags of
      129 -> ping_response;
      130 -> info_response;
      131 -> join_response;
      132 -> game_update;
      003 -> error_join_response;
      002 -> error_info_response;
      Other when Other band 128 == 0 -> throw({error, Other})
    end,
  {Type, MsgId, Time, hdp_parse(Type, Parser, Message)}.

hdp_parse(ping_response, _, <<>>) -> pong;
hdp_parse(info_response, default, <<GameCount:?UCHAR, Games/binary>>) ->
  { GameCount
  , [ #{ id => GameId
       , name => Name
       , state => hdp_parse_state(State)
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
hdp_parse(info_response, detail, GameDesc) ->
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
   , CurrentSerpents:?UCHAR
   , Serpents/binary
   >> = GameDesc,
  #{ id => GameId
   , name => Name
   , state => hdp_parse_state(State)
   , flags => hdp_parse_flags(Flags)
   , cols => Cols
   , rows => Rows
   , tickrate => TickRate
   , countdown => Countdown
   , rounds => Rounds
   , initial_food => InitialFood
   , max_serpents => MaxSerpents
   , current_serpents => CurrentSerpents
   , serpents => hdp_parse_serpents(Serpents)
   };
hdp_parse(error_join_response, _, Error) ->
  <<GameId:?USHORT, ReasonSize:?UCHAR, Reason:ReasonSize/binary>> = Error,
  {GameId, Reason};
hdp_parse(error_info_response, _, Error) ->
  <<GameId:?USHORT, ReasonSize:?UCHAR, Reason:ReasonSize/binary>> = Error,
  {GameId, Reason};
hdp_parse(join_response, _, Response) ->
  << SerpentId:?UINT
   , GameDesc/binary
   >> = Response,
  {SerpentId, hdp_parse(info_response, detail, GameDesc)}.

hdp_parse_state(0) -> created;
hdp_parse_state(1) -> countdown;
hdp_parse_state(2) -> started;
hdp_parse_state(4) -> finished.

hdp_parse_flags(Flags) when Flags rem 2 == 1 ->
  lists:sort([walls | hdp_parse_flags(Flags - 1)]);
hdp_parse_flags(0) -> [];
hdp_parse_flags(2) -> [random_food];
hdp_parse_flags(4) -> [increasing_food];
hdp_parse_flags(6) -> [increasing_food, random_food].

hdp_parse_serpents(Serpents) ->
  [ {Id, Name}
  || <<Id:?UINT, NameSize:?UCHAR, Name:NameSize/binary>> <= Serpents
  ].

hdp_send(Message, Config) ->
  {socket, UdpSocket} = lists:keyfind(socket, 1, Config),
  Port = application:get_env(serpents, udp_port, 8584),
  gen_udp:send(UdpSocket, localhost, Port, Message).

hdp_join(MsgId, GameId, Name) ->
  H = hdp_head(3, MsgId),
  S = size(Name),
  <<H/binary, GameId:?USHORT, S:?UCHAR, Name/binary>>.

hdp_game(MsgId, GameId) ->
  H = hdp_head(2, MsgId),
  <<H/binary, GameId:?USHORT>>.

hdp_games(MsgId) -> hdp_head(2, MsgId).

hdp_ping(MsgId) -> hdp_head(1, MsgId).

hdp_head(Flags, MsgId) ->
  hdp_head(Flags, MsgId, 0).
hdp_head(Flags, MsgId, UserId) ->
  {_, _, Nanos} = os:timestamp(),
  hdp_head(Flags, MsgId, Nanos rem 65536, UserId).
hdp_head(Flags, MsgId, UserTime, UserId) ->
  <<Flags:?UCHAR, MsgId:?USHORT, UserTime:?USHORT, UserId:?USHORT>>.

update_game_list(Config) ->
  ct:comment("A games request is sent"),
  ok = hdp_send(hdp_games(99), Config),

  ct:comment("A games list is received"),
  {info_response, 99, _, {_, _}} = hdp_recv(Config),
  ok.

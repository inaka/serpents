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
        , game_states/1
        , single_game/1
        , join/1
        , state_server_update/1
        , countdown_server_update/1
        , rounds_server_update/1
        , serpents_server_update/1
        , fruit_server_update/1
        , user_update_changes_direction/1
        , wrong_input/1
        ]).

-include("binary-sizes.hrl").

-define(VERY_MUCH, 9999999).

-spec all() -> [atom()].
all() -> spts_test_utils:all(?MODULE).

-spec init_per_testcase(atom(), spts_test_utils:config()) ->
  spts_test_utils:config().
init_per_testcase(serpents_server_update, Config) ->
  Port = application:get_env(serpents, udp_port, 8584) - 2,
  {ok, UdpSocket} = new_socket(Port),
  [{socket2, UdpSocket} | init_per_testcase(others, Config)];
init_per_testcase(_Test, Config) ->
  application:set_env(serpents, hdp_updates_per_second, 1),
  Port = application:get_env(serpents, udp_port, 8584) - 1,
  {ok, UdpSocket} = new_socket(Port),
  [{socket, UdpSocket} | Config].

-spec end_per_testcase(atom(), spts_test_utils:config()) ->
  spts_test_utils:config().
end_per_testcase(serpents_server_update, Config) ->
  {value, {socket2, UdpSocket}, NewConfig} = lists:keytake(socket2, 1, Config),
  catch gen_udp:close(UdpSocket),
  end_per_testcase(others, NewConfig);
end_per_testcase(_Test, Config) ->
  lists:foreach(
    fun(Game) -> spts_core:stop_game(spts_games:id(Game)) end,
    spts_core:all_games()),
  ktn_task:wait_for(fun spts_core:all_games/0, []),
  application:unset_env(serpents, hdp_updates_per_second),
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

-spec game_states(spts_test_utils:config()) -> {comment, []}.
game_states(Config) ->
  ct:comment("A game is created"),
  Game = spts_core:create_game(#{countdown => 1, rounds => 100}),
  GameName = spts_games:id(Game),

  CheckState =
    fun(I) ->
      ok = hdp_send(hdp_games(I), Config),
      {info_response, I, _, {1, [#{state := State}]}} = hdp_recv(Config),
      State
    end,

  ct:comment("Game create"),
  created = CheckState(1),

  ct:comment("Countdown"),
  spts_core:add_serpent(GameName, <<"s1">>),
  spts_core:start_game(GameName),
  countdown = CheckState(2),

  ct:comment("Started"),
  spts_games:process_name(GameName) ! tick,
  started = CheckState(3),

  ct:comment("Finished"),
  lists:foreach(
    fun(_) -> spts_games:process_name(GameName) ! tick end,
    lists:seq(1, 100)),
  finished = CheckState(4),

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

  ct:comment("Another game is created"),
  Game2 = spts_core:create_game(#{max_serpents => 2, countdown => 1}),
  Game2Id = spts_games:numeric_id(Game2),
  Game2Name = spts_games:id(Game2),

  ct:comment("A games request is sent"),
  ok = hdp_send(hdp_games(3), Config),

  ct:comment("A games list is received"),
  {info_response, 3, _, {2, GameInfos}} = hdp_recv(Config),
  [Game2Info] = GameInfos -- [GameInfo],
  #{ id := Game2Id
   , name := Game2Name
   , state := created
   , current_serpents := 0
   , max_serpents := 2
   } = Game2Info,

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
   , tickrate := 1
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
   , tickrate := 1
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

  ct:comment("A player joins"),
  ok = hdp_send(hdp_join(2, GameId, <<"s1">>), Config),
  {join_response, 2, _, {S1Id, GD1}} = hdp_recv(Config),
  #{ id := GameId
   , name := GameName
   , state := created
   , flags := []
   , cols := 20
   , rows := 20
   , tickrate := 1
   , countdown := 10
   , rounds := 0
   , initial_food := 1
   , current_serpents := 1
   , max_serpents := 255
   , serpents := [{S1Id, <<"s1">>}]
   } = GD1,
  [Serpent1] = spts_games:serpents(spts_core:fetch_game(GameName)),
  S1Id = spts_serpents:numeric_id(Serpent1),

  ct:comment("A player tries to join again"),
  ok = hdp_send(hdp_join(3, GameId, <<"s1">>), Config),
  {error_join_response, 3, _, {GameId, <<"unspecified">>}} = hdp_recv(Config),
  [Serpent1] = spts_games:serpents(spts_core:fetch_game(GameName)),

  ct:comment("Another player joins"),
  ok = hdp_send(hdp_join(3, GameId, <<"s2">>), Config),
  {join_response, 3, _, {S2Id, GD2}} = hdp_recv(Config),
  #{ current_serpents := 2
   , serpents := [{S2Id, <<"s2">>}, {S1Id, <<"s1">>}]
   } = GD2,
  [_, _] = spts_games:serpents(spts_core:fetch_game(GameName)),

  ct:comment("The game starts"),
  spts_core:start_game(GameName),

  ct:comment("Nobody can join anymore"),
  ok = hdp_send(hdp_join(4, GameId, <<"s3">>), Config),
  {error_join_response, 4, _, {GameId, <<"unspecified">>}} = hdp_recv(Config),
  [_, _] = spts_games:serpents(spts_core:fetch_game(GameName)),

  {comment, ""}.

-spec state_server_update(spts_test_utils:config()) -> {comment, []}.
state_server_update(Config) ->
  ct:comment("A game is created"),
  Game = spts_core:create_game(#{ticktime => 60000}),
  GameId = spts_games:numeric_id(Game),
  GameName = spts_games:id(Game),
  Process = spts_hdp_game_handler:process_name(GameId),

  {S1Id, GD1} = do_join(GameId, <<"s1">>, Config),
  #{tickrate := 1} = GD1,

  ct:comment("The game starts"),
  spts_core:start_game(GameName),

  ct:comment("After a tick, the server sends an update with a state diff"),
  Process ! tick,
  {Tick, Diffs} = diffs(hdp_recv(Config)),

  [countdown] = [Data || #{data := Data, type := state} <- Diffs],

  ct:comment("The client acks the update"),
  ok = hdp_direct_send(hdp_update(3, S1Id, Tick, none), Config),

  ct:comment("After a tick, the server sends an update with no diffs"),
  Process ! tick,
  no_diffs(hdp_recv(Config)),

  {comment , ""}.

-spec countdown_server_update(spts_test_utils:config()) -> {comment, []}.
countdown_server_update(Config) ->
  ct:comment("A game is created"),
  Game = spts_core:create_game(#{ticktime => 60000, countdown => 3}),
  GameId = spts_games:numeric_id(Game),
  GameName = spts_games:id(Game),
  Process = spts_hdp_game_handler:process_name(GameId),

  {S1Id, GD1} = do_join(GameId, <<"s1">>, Config),
  #{tickrate := 1} = GD1,

  ct:comment("The game starts"),
  spts_core:start_game(GameName),

  ct:comment("After every tick, the server sends an update with a c-down diff"),
  Counts =
    fun(I) ->
      Process ! tick,
      {_, Diffs} = diffs(hdp_recv(Config)),
      [I] = [Data || #{data := Data, type := countdown} <- Diffs],
      spts_games:process_name(GameName) ! tick
    end,

  lists:foreach(Counts, [2, 1, 0]),

  Process ! tick,
  {Tick, [_|_]} = diffs(hdp_recv(Config)),

  ct:comment("The client acks the updates"),
  ok = hdp_direct_send(hdp_update(3, S1Id, Tick, none), Config),

  ct:comment("After a tick, the server sends an update with no diffs"),
  Process ! tick,
  no_diffs(hdp_recv(Config)),

  {comment , ""}.

-spec rounds_server_update(spts_test_utils:config()) -> {comment, []}.
rounds_server_update(Config) ->
  ct:comment("A game is created"),
  Game =
    spts_core:create_game(
      #{ticktime => 60000, countdown => 0, rounds => 100}),
  GameId = spts_games:numeric_id(Game),
  GameName = spts_games:id(Game),
  Process = spts_hdp_game_handler:process_name(GameId),

  {S1Id, GD1} = do_join(GameId, <<"s1">>, Config),
  #{tickrate := 1} = GD1,

  ct:comment("The game starts"),
  spts_core:start_game(GameName),
  Process ! tick,
  clean_queue(Config),
  spts_games:process_name(GameName) ! tick,

  ct:comment("After every tick, the server sends an update with a rounds diff"),
  Counts =
    fun(I) ->
      Process ! tick,
      {_, Diffs} = diffs(hdp_recv(Config)),
      case [Data || #{data := Data, type := state} <- Diffs] of
        [finished] -> ok;
        _ -> [I] = [Data || #{data := Data, type := rounds} <- Diffs]
      end,
      spts_games:process_name(GameName) ! tick
    end,

  lists:foreach(Counts, lists:seq(99, 0, -1)),

  Process ! tick,
  {Tick, _} = diffs(hdp_recv(Config)),

  clean_queue(Config),

  ct:comment("The client acks the updates"),
  ok = hdp_direct_send(hdp_update(3, S1Id, Tick, none), Config),

  ct:comment("After a tick, the server sends an update with no diffs"),
  Process ! tick,
  {Tick2, []} = diffs(hdp_recv(Config)),

  ct:comment("The client acks the updates again"),
  ok = hdp_direct_send(hdp_update(3, S1Id, Tick2, none), Config),

  ct:comment("After a tick, the server sends an update with no diffs (Again)"),
  Process ! tick,
  no_diffs(hdp_recv(Config)),

  {comment , ""}.

-spec serpents_server_update(spts_test_utils:config()) -> {comment, []}.
serpents_server_update(Config) ->
  ct:comment("A game is created"),
  Game = spts_core:create_game(#{ticktime => 60000}),
  GameId = spts_games:numeric_id(Game),
  Process = spts_hdp_game_handler:process_name(GameId),

  %% Config for player 2
  {socket2, UdpSocket} = lists:keyfind(socket2, 1, Config),
  Config2 = [{socket, UdpSocket} | Config],

  {S1Id, GD1} = do_join(GameId, <<"s1">>, Config),
  #{tickrate := 1} = GD1,

  ct:comment(
    "After a tick, the server sends an update with the player serpent"),
  Process ! tick,
  {Tick1, Diffs} = diffs(hdp_recv(Config)),

  [[{Row, Col}]] =
    [Body || #{data := Data, type := serpents} <- Diffs
           , #{id := SId, body := Body} <- Data
           , SId == S1Id
           ],

  {S2Id, _GD2} = do_join(GameId, <<"s2">>, Config2),

  ct:comment(
    "After a tick, the server sends updates with both serpents to both users"),
  Process ! tick,
  {Tick2, Diffs2} = diffs(hdp_recv(Config)),
  {Tick2, Diffs2} = diffs(hdp_recv(Config2)),
  case Tick2 of
    Tick2 when Tick2 =< Tick1 -> ct:fail("Wrong ticks: ~p, ~p", [Tick1, Tick2]);
    Tick2 -> ok
  end,

  [SerpentsDiff] = [Data || #{data := Data, type := serpents} <- Diffs2],
  [_, _] = SerpentsDiff,

  [{Row, Col}] = serpent_body(S1Id, SerpentsDiff),
  [{_Row, _Col}] = serpent_body(S2Id, SerpentsDiff),

  ct:comment("The client 1 acks the update"),
  ok = hdp_direct_send(hdp_update(3, S1Id, Tick2, none), Config),

  ct:pal("After a tick, the server sends an update with no diffs to user 1"),
  Process ! tick,
  no_diffs(hdp_recv(Config)),

  ct:pal("After a tick, the server sends an update with diffs to user 2"),
  {_, Diffs2} = diffs(hdp_recv(Config2)),

  {comment , ""}.

-spec fruit_server_update(spts_test_utils:config()) -> {comment, []}.
fruit_server_update(Config) ->
  ct:comment("A game is created"),
  Game =
    spts_core:create_game(
      #{ticktime => 60000, countdown => 0, rows => 5, cols => 5}),
  GameId = spts_games:numeric_id(Game),
  GameName = spts_games:id(Game),
  Process = spts_hdp_game_handler:process_name(GameId),

  {S1Id, GD1} = do_join(GameId, <<"s1">>, Config),
  #{tickrate := 1} = GD1,

  ct:comment("The game starts"),
  spts_core:start_game(GameName),
  Process ! tick,
  clean_queue(Config),
  spts_games:process_name(GameName) ! tick,

  ct:comment("After a tick, the server sends an update with a fruit diff"),
  Process ! tick,
  {Tick, Diffs} = diffs(hdp_recv(Config)),
  [{1, Row, Col}] = [Data || #{data := Data, type := fruit} <- Diffs],
  case {Row, Col} of
    {Row, _} when Row > 5; Row < 1 -> ct:fail("Invalid row: ~p", [Row]);
    {_, Col} when Col > 5; Col < 1 -> ct:fail("Invalid col: ~p", [Col]);
    {_, _} -> ok
  end,

  ct:comment("The client acks the update"),
  ok = hdp_direct_send(hdp_update(3, S1Id, Tick, none), Config),

  ct:comment("After a tick, the server sends an update with no diffs"),
  Process ! tick,
  no_diffs(hdp_recv(Config)),

  {comment , ""}.

-spec wrong_input(spts_test_utils:config()) -> {comment, []}.
wrong_input(Config) ->
  ct:comment("Wrong info is ignored by spts_hdp_handler"),
  spts_hdp_handler ! wrong_info,
  ok = hdp_send(hdp_ping(1), Config),
  {ping_response, 1, _, pong} = hdp_recv(Config),

  ct:comment("Wrong calls generate static error"),
  {unknown, x} = gen_server:call(spts_hdp_handler, x),

  ct:comment("Malformed messages are ignored"),
  ok = hdp_send(<<0>>, Config),
  ok = hdp_send(hdp_head(0, 2), Config),
  ok = hdp_send(hdp_ping(3), Config),
  {ping_response, 3, _, pong} = hdp_recv(Config),

  ct:comment("A game is created, a player joins"),
  Game = spts_core:create_game(),
  GameId = spts_games:numeric_id(Game),
  GameName = spts_games:id(Game),

  {SId, _} = do_join(GameId, <<"s1">>, Config),

  ct:comment("Bad SerpentId is ignored"),
  ok = hdp_direct_send(hdp_update(3, SId + 1, 1, none), Config),

  ct:comment("The game is stopped"),
  ok = spts_core:stop_game(GameName),

  ct:comment("Another player tries to join, it should fail"),
  ok = hdp_send(hdp_join(3, GameId, <<"s2">>), Config),
  {error_join_response, 3, _, {_, <<"badgame">>}} = hdp_recv(Config),

  ct:comment("Another game is created, a player joins, then it's stopped"),
  Game2 = spts_core:create_game(),
  Game2Id = spts_games:numeric_id(Game2),
  Game2Name = spts_games:id(Game2),
  do_join(Game2Id, <<"s1">>, Config),
  ok = spts_core:stop_game(Game2Name),

  ct:comment("The handler is still alive"),
  Process = spts_hdp_game_handler:process_name(Game2Id),
  true = is_pid(whereis(Process)),

  ct:comment("Wrong input is ignored, but after a tick, it dies"),
  Process ! wrong_input,
  true = is_pid(whereis(Process)),

  Process ! tick,
  ktn_task:wait_for(fun() -> whereis(Process) end, undefined),

  {comment, ""}.

-spec user_update_changes_direction(spts_test_utils:config()) -> {comment, []}.
user_update_changes_direction(Config) ->
  ct:comment("A game is created, a player joins"),
  Game = spts_core:create_game(),
  GameId = spts_games:numeric_id(Game),
  GameName = spts_games:id(Game),

  {SId, _} = do_join(GameId, <<"s1">>, Config),
  CheckDir =
    fun() ->
      spts_serpents:direction(
        spts_games:serpent(spts_core:fetch_game(GameName), <<"s1">>))
    end,
  Turn =
    fun(Dir, Dir2) ->
      hdp_direct_send(hdp_update(30, SId, 1, Dir), Config),
      ktn_task:wait_for(CheckDir, Dir2),
      CheckDir()
    end,

  ct:comment("Player moves right"),
  right = Turn(right, right),
  right = Turn(none, right),

  ct:comment("Player moves left"),
  left = Turn(left, left),
  left = Turn(none, left),

  ct:comment("Player moves up"),
  up = Turn(up, up),
  up = Turn(none, up),

  ct:comment("Player moves down"),
  down = Turn(down, down),
  down = Turn(none, down),

  {comment, ""}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Message parsing/handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clean_queue(Config) ->
  ktn_task:wait_for(fun() -> hdp_recv(Config) end, {error, timeout}).

hdp_recv(Config) ->
  hdp_recv(Config, default).
hdp_recv(Config, Parser) ->
  {socket, UdpSocket} = lists:keyfind(socket, 1, Config),
  {ok, {{127, 0, 0, 1}, _, Packet}} = gen_udp:recv(UdpSocket, ?VERY_MUCH, 1000),
  <<Flags:?UCHAR, MsgId:?UINT, Time:?USHORT, Message/binary>> = Packet,
  ct:pal(
    "Flags: ~p / MsgId: ~p, Time: ~p, Pckt: ~p",
    [Flags, MsgId, Time, Packet]),
  Type =
    case Flags of
      129 -> ping_response;
      130 -> info_response;
      131 -> join_response;
      132 -> server_update;
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
  {SerpentId, hdp_parse(info_response, detail, GameDesc)};
hdp_parse(server_update, _, <<Tick:?USHORT, _NumDiffs:?UCHAR, Diffs/binary>>) ->
  {Tick, hdp_parse_diffs(Diffs)}.

hdp_parse_diffs(<<>>) -> [];
hdp_parse_diffs(<<DiffType:?UCHAR, Rest/binary>>) ->
  Type = hdp_parse_diff_type(DiffType),
  {Data, Next} = hdp_parse_diff_data(Type, Rest),
  [#{type => Type, data => Data} | hdp_parse_diffs(Next)].

hdp_parse_diff_type(0) -> state;
hdp_parse_diff_type(1) -> countdown;
hdp_parse_diff_type(2) -> rounds;
hdp_parse_diff_type(3) -> serpents;
hdp_parse_diff_type(4) -> fruit.

hdp_parse_diff_data(state, <<State:?UCHAR, Next/binary>>) ->
  {hdp_parse_state(State), Next};
hdp_parse_diff_data(countdown, <<Countdown:?USHORT, Next/binary>>) ->
  {Countdown, Next};
hdp_parse_diff_data(rounds, <<Rounds:?UINT, Next/binary>>) ->
  {Rounds, Next};
hdp_parse_diff_data(
  fruit, <<Food:?UCHAR, Row:?UCHAR, Col:?UCHAR, Next/binary>>) ->
  {{Food, Row, Col}, Next};
hdp_parse_diff_data(serpents, <<NumSerpents:?UCHAR, Next/binary>>) ->
  hdp_parse_diff_serpents(NumSerpents, Next, []).

hdp_parse_diff_serpents(0, Next, Acc) -> {lists:reverse(Acc), Next};
hdp_parse_diff_serpents(
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
  hdp_parse_diff_serpents(N-1, Next, [Serpent|Acc]).

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

hdp_direct_send(Message, Config) ->
  {socket, UdpSocket} = lists:keyfind(socket, 1, Config),
  Port = application:get_env(serpents, udp_port, 8584) - 1,
  Data = iolist_to_binary(Message),
  spts_hdp_handler:handle_udp(Data, UdpSocket, {127, 0, 0, 1}, Port).

hdp_send(Message, Config) ->
  {socket, UdpSocket} = lists:keyfind(socket, 1, Config),
  Port = application:get_env(serpents, udp_port, 8584),
  gen_udp:send(UdpSocket, localhost, Port, Message).

hdp_update(MsgId, UserId, LastTick, Direction) ->
  H = hdp_head(4, MsgId, UserId),
  Action = hdp_action(Direction),
  <<H/binary, LastTick:?USHORT, Action:?UCHAR>>.

hdp_action(left)  -> 1;
hdp_action(right) -> 2;
hdp_action(up)    -> 4;
hdp_action(down)  -> 8;
hdp_action(none)  -> 0.

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
  <<Flags:?UCHAR, MsgId:?UINT, UserTime:?USHORT, UserId:?UINT>>.

new_socket(Port) ->
  gen_udp:open(Port, [{mode, binary}, {reuseaddr, true}, {active, false}]).

serpent_body(SerpentId, SerpentsDiff) ->
  [SBody] =
    [Body || #{id := SId, body := Body} <- SerpentsDiff, SId == SerpentId],
  SBody.

no_diffs(Update) -> {_, []} = diffs(Update).

diffs({server_update, Tick, Tick, {Tick, Diffs}}) -> {Tick, Diffs}.

do_join(GameId, Name, Config) ->
  ct:comment("~p joins ~p", [Name, GameId]),
  ok = hdp_send(hdp_join(16, GameId, Name), Config),
  ct:comment("The response is received"),
  {join_response, 16, _, Response} = hdp_recv(Config),
  Response.

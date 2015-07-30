%% @doc General Test Utils
-module(spts_test_utils).
-author('elbrujohalcon@inaka.net').

-type config() :: proplists:proplist().
-export_type([config/0]).

-export([ all/1
        , init_per_suite/1
        , end_per_suite/1
        ]).
-export([ api_call/2
        , api_call/3
        , api_call/4
        ]).
-export([ get_events/2
        , get_events_after/3
        , no_events_after/2
        ]).
-export([ move/2
        , check_bounds/2
        ]).

-spec all(atom()) -> [atom()].
all(Module) ->
  ExcludedFuns = [module_info, init_per_suite, end_per_suite, group, all],
  Exports = Module:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  {ok, _} = serpents:start(),
  {ok, _} = shotgun:start(),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  serpents:stop(),
  shotgun:stop(),
  Config.

-spec api_call(atom(), string()) -> #{}.
api_call(Method, Uri) ->
  api_call(Method, Uri, #{}).

-spec api_call(atom(), string(), #{}) -> #{}.
api_call(Method, Uri, Headers) ->
  api_call(Method, Uri, Headers, []).

-spec api_call(atom(), string(), #{}, iodata()) -> #{}.
api_call(Method, Uri, Headers, Body) ->
  Port = application:get_env(serpents, http_port, 8585),
  {ok, Pid} = shotgun:open("localhost", Port),
  try
    {ok, Response} =
      shotgun:request(Pid, Method, "/api" ++ Uri, Headers, Body, #{}),
    Response
  after
    shotgun:close(Pid)
  end.

-spec get_events(iodata(), binary()) -> [{nofin | fin, reference(), binary()}].
get_events(Uri, EventType) ->
  {ok, Events} = get_events_after(Uri, EventType, fun() -> ok end),
  Events.

-spec get_events_after(iodata(), binary(), fun(() -> X)) -> {X, [map()]}.
get_events_after(Uri, EventType, Task) ->
  Port = application:get_env(serpents, http_port, 8585),
  {ok, Pid} = shotgun:open("localhost", Port),
  try
    ct:comment("Client connects"),
    {ok, Ref} = open_request(Pid, Uri),

    ct:comment("Task is performed: ~p", [Task]),
    TaskResult = Task(),

    ct:comment("Events are collected"),
    Events =
      ktn_task:wait_for_success(
        fun() ->
          Events =
            [shotgun:parse_event(Bin) || {_, _, Bin} <- shotgun:events(Pid)],
          ct:pal("Events: ~p / EventType: ~p", [Events, EventType]),
          [_|_] = [Event || Event = #{event := ET} <- Events, ET == EventType]
        end),

    {TaskResult, Events}
  after
    shotgun:close(Pid)
  end.

-spec no_events_after(iodata(), fun(() -> ok)) -> ok.
no_events_after(Uri, Task) ->
  Port = application:get_env(serpents, http_port, 8585),
  {ok, Pid} = shotgun:open("localhost", Port),
  try
    ct:comment("Client connects"),
    {ok, Ref} = open_request(Pid, Uri),

    ct:comment("Initial events are collected"),
    ktn_task:wait_for_success(fun() -> [_|_] = shotgun:events(Pid) end),

    ct:comment("Task is performed: ~p", [Task]),
    ok = Task(),

    ct:comment("No more events"),
    {error, {timeout, {badmatch, []}}} =
      ktn_task:wait_for_success(fun() -> [_|_] = shotgun:events(Pid) end),

    ok
  after
    shotgun:close(Pid)
  end.

-spec move(spts_games:position(), spts_games:direction()) ->
  spts_games:position().
move({Row, Col}, up) -> {Row-1, Col};
move({Row, Col}, down) -> {Row+1, Col};
move({Row, Col}, left) -> {Row, Col-1};
move({Row, Col}, right) -> {Row, Col+1}.

-spec check_bounds(spts_games:game(), spts_games:position()) -> in | out.
check_bounds(Game, Position) ->
  check_bounds(Position, spts_games:rows(Game), spts_games:cols(Game)).

check_bounds({0, _}, _, _) -> out;
check_bounds({_, 0}, _, _) -> out;
check_bounds({Row, _}, Rows, _) when Row > Rows -> out;
check_bounds({_, Col}, _, Cols) when Col > Cols -> out;
check_bounds(_, _, _) -> in.

open_request(Pid, Uri) ->
  shotgun:get(Pid, "/api" ++ Uri, #{}, #{async => true, async_mode => sse}).

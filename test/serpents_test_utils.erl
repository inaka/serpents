%% @doc General Test Utils
-module(serpents_test_utils).

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
    {ok, Response} = shotgun:request(Pid, Method, Uri, Headers, Body, #{}),
    Response
  after
    shotgun:close(Pid)
  end.

-spec move(serpents_games:position(), serpents_serpents:direction()) ->
  serpents_games:position().
move({Row, Col}, up) -> {Row-1, Col};
move({Row, Col}, down) -> {Row+1, Col};
move({Row, Col}, left) -> {Row, Col-1};
move({Row, Col}, right) -> {Row, Col+1}.

-spec check_bounds(serpents_games:game(), serpents_games:position()) -> in|out.
check_bounds(Game, Position) ->
  check_bounds(Position, serpents_games:rows(Game), serpents_games:cols(Game)).

check_bounds({0, _}, _, _) -> out;
check_bounds({_, 0}, _, _) -> out;
check_bounds({Row, _}, Rows, _) when Row > Rows -> out;
check_bounds({_, Col}, _, Cols) when Col > Cols -> out;
check_bounds(_, _, _) -> in.

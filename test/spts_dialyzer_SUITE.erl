-module(spts_dialyzer_SUITE).

-export([all/0]).
-export([dialyze/1]).

%%% @todo fix dialyzer warnings
-spec all() -> [dialyze].
all() -> [].

%% @todo actually build the plt if it's not there already
-spec dialyze(spts_test_utils:config()) -> {comment, []}.
dialyze(_Config) ->
  BaseDir = code:lib_dir(serpents),
  Plt = filename:join(BaseDir, ".serpents.plt"),
  Ebin = filename:join(BaseDir, "ebin"),
  Test = filename:join(BaseDir, "test"),

  ct:comment("`make plt-all` should've been executed"),
  true = filelib:is_file(Plt),

  ct:comment("Dialyzer must emit no warnings"),
  Opts =
    [ {analysis_type, succ_typings}
    , {plts, [Plt]}
    , {files_rec, [Ebin, Test]}
    , {check_plt, true}
    , {warnings, [error_handling, race_conditions, unmatched_returns]}
    , {get_warnings, true}
    ],
  [] = [dialyzer:format_warning(W, basename) || W <- dialyzer:run(Opts)],
  {comment, ""}.

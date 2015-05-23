%%% @doc utilities for web handlers
-module(spts_web_utils).
-author('elbrujohalcon@inaka.net').

-type authorization_mechanism() :: none | player | session.
-export_type([authorization_mechanism/0]).

-export([announce_req/2, handle_exception/3]).

-spec announce_req(cowboy_req:req(), iodata()) -> cowboy_req:req().
announce_req(Req, Suffix) ->
  {Method, Req1} = cowboy_req:method(Req),
  {Path,   Req2} = cowboy_req:path(Req1),
  lager:info("~s ~s ~s", [Method, Path, Suffix]),
  Req2.

-spec handle_exception(atom(), cowboy_req:req(), term()) ->
    {halt, cowboy_req:req(), term()}.
handle_exception({missing_field, Field}, Req, State) ->
  Response = spts_json:encode(#{error => <<"missing field: ", Field/binary>>}),
  {ok, Req1} = cowboy_req:reply(400, [], Response, Req),
  {halt, Req1, State};
handle_exception(Error, Req, State) when is_atom(Error) ->
  Response = spts_json:encode(#{error => Error}),
  {ok, Req1} = cowboy_req:reply(400, [], Response, Req),
  {halt, Req1, State};
handle_exception(not_found, Req, State) ->
  {ok, Req1} = cowboy_req:reply(404, Req),
  {halt, Req1, State};
handle_exception(Reason, Req, State) ->
  lager:error("~p. Stack Trace: ~p", [Reason, erlang:get_stacktrace()]),
  {ok, Req1} =
    try cowboy_req:reply(500, Req)
    catch
      _:Error ->
        lager:critical(
          "~p trying to report error through cowboy. Stack Trace: ~p",
          [Error, erlang:get_stacktrace()]),
        {ok, Req}
    end,
  {halt, Req1, State}.

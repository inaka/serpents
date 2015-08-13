%% @doc Binary abstraction library
-module(spts_binary).

-export([pascal_string/1]).

-define(UCHAR, 8/unsigned-integer).

-spec pascal_string(binary()) -> binary().
pascal_string(String) ->
  Length = erlang:size(String),
  <<Length:?UCHAR, String/binary>>.

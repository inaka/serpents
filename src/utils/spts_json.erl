%% @doc Json abstraction library
-module(spts_json).

-export([encode/1, decode/1]).

-type json() :: #{} | [#{}] | binary() | number() | boolean() |
                map()| [map()] | null.
-export_type([json/0]).

-spec encode(json()) -> iodata().
encode(Json) -> jiffy:encode(Json, [uescape]).

-spec decode(iodata()) -> json().
decode(Data) ->
  try jiffy:decode(Data, [return_maps])
  catch
    _:{error, _} ->
      _ = lager:warning("Bad Json: ~p", [Data]),
      throw(bad_json)
  end.

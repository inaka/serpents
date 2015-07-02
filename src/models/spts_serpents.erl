%% @doc Serpent model
-module(spts_serpents).
-author('elbrujohalcon@inaka.net').

-type status() :: alive | dead.
-type name() :: binary().
-opaque serpent() :: #{ name       => name()
                      , numeric_id => pos_integer()
                      , token      => binary()
                      , body       => [spts_games:position()]
                      , direction  => spts_games:direction()
                      , food       => pos_integer()
                      , status     => status()
                      }.
-export_type([serpent/0, status/0, name/0]).

-define(UINT, 32/unsigned-integer).

-export([new/6]).
-export([ name/1
        , numeric_id/1
        , game_id/1
        , token/1
        , direction/1
        , direction/2
        , body/1
        , status/2
        , status/1
        , advance/1
        , feed/2
        , to_json/1
        , to_json/2
        , to_binary/1
        ]).

-spec new(
  name(), pos_integer(), pos_integer(), spts_games:position(),
  spts_games:direction(), non_neg_integer()) -> serpent().
new(Name, GameNumericId, NumericId, Position, Direction, Food) ->
  #{ name       => Name
   , numeric_id => join_id(GameNumericId, NumericId)
   , token      => iolist_to_binary(ktn_random:generate())
   , direction  => Direction
   , body       => [Position]
   , food       => Food
   , status     => alive
   }.

-spec name(serpent()) -> name().
name(#{name := Name}) -> Name.

-spec numeric_id(serpent()) -> pos_integer().
numeric_id(#{numeric_id := NumericId}) -> NumericId.

-spec game_id(pos_integer()) -> pos_integer().
game_id(NumericId) -> NumericId div 10000.

-spec token(serpent()) -> binary().
token(#{token := Token}) -> Token.

-spec direction(serpent()) -> spts_games:direction().
direction(#{direction := Direction}) -> Direction.

-spec direction(serpent(), spts_games:direction()) -> serpent().
direction(Serpent, Direction) -> Serpent#{direction := Direction}.

-spec body(serpent()) -> [spts_games:position()].
body(#{body := Body}) -> Body.

-spec status(serpent()) -> status().
status(#{status := Status}) -> Status.

-spec status(serpent(), status()) -> serpent().
status(Serpent, Status) -> Serpent#{status := Status}.

-spec advance(serpent()) -> serpent().
advance(Serpent = #{status := dead}) -> Serpent;
advance(Serpent) ->
  #{body := Body, direction := Direction, food := Food} = Serpent,
  [Head|_] = Body,
  NewHead = advance(Head, Direction),
  {NewFood, NewTail} =
    case Food of
      0 -> {0, lists:droplast(Body)};
      Food -> {Food - 1, Body}
    end,
  Serpent#{body := [NewHead | NewTail], food := NewFood}.

-spec feed(serpent(), pos_integer()) -> serpent().
feed(Serpent = #{food := Food}, MoreFood) -> Serpent#{food := Food + MoreFood}.

-spec to_json(serpent()) -> map().
to_json(Serpent) -> to_json(Serpent, public).

-spec to_json(serpent(), public | private) -> map().
to_json(Serpent, public) ->
  #{ name => name(Serpent)
   , body => [[Row, Col] || {Row, Col} <- body(Serpent)]
   , status => status(Serpent)
   };
to_json(Serpent, private) ->
  #{ name => name(Serpent)
   , token => token(Serpent)
   , direction => direction(Serpent)
   , body => [[Row, Col] || {Row, Col} <- body(Serpent)]
   , status => status(Serpent)
   }.

-spec to_binary(serpent()) -> iodata().
to_binary(Serpent) ->
  #{numeric_id := Id, name := Name} = Serpent,
  [<<Id:?UINT>>, spts_binary:pascal_string(Name)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

advance({Row, Col}, up) -> {Row-1, Col};
advance({Row, Col}, down) -> {Row+1, Col};
advance({Row, Col}, left) -> {Row, Col-1};
advance({Row, Col}, right) -> {Row, Col+1}.

join_id(GameNumericId, NumericId) ->
  GameNumericId * 10000 + NumericId.

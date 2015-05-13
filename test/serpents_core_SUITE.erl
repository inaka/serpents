-module(serpents_core_SUITE).
-author('elbrujohalcon@inaka.net').

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {serpents_test_utils,
         [ init_per_suite/1
         , end_per_suite/1
         ]}
       ]).

-export([all/0]).
-export([player_registration/1]).

-spec all() -> [atom()].
all() -> serpents_test_utils:all(?MODULE).

-spec player_registration(serpents_test_utils:config()) -> {comment, string()}.
player_registration(_Config) ->
  ct:comment("A player is created"),
  Player1 = serpents_core:register_player(<<"name 1">>),
  <<"name 1">> = serpents_players:name(Player1),
  Id1 = serpents_players:id(Player1),

  ct:comment("Another player is created"),
  Player2 = serpents_core:register_player(<<"name 2">>),
  <<"name 2">> = serpents_players:name(Player2),
  case serpents_players:id(Player2) of
    Id1 -> ct:fail("Duplicated id: ~p", [Id1]);
    _ -> ok
  end,

  ct:comment("Even with the same name, no validations"),
  Player3 = serpents_core:register_player(<<"name 3">>),
  <<"name 3">> = serpents_players:name(Player3),
  case serpents_players:id(Player3) of
    Id1 -> ct:fail("Duplicated id: ~p", [Id1]);
    _ -> ok
  end,

  {comment, ""}.

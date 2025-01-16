-module(hello_world_tests).
-include_lib("eunit/include/eunit.hrl").

hello_world_test() ->
    ?assertEqual(2, 1 + 1).
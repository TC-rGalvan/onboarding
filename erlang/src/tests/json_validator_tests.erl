-module(json_validator_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../records/records.hrl").

validate_test() ->
    Record = #organization{id = "123", name = "Test Org"},
    Json = #{<<"id">> => <<"123">>, <<"name">> => <<"Test Org">>},
    ?assertEqual(true, json_validator:validate(Record, Json)).

validate_with_missing_fields_test() ->
    Record = #organization{id = "123", name = "Test Org"},
    Json = #{<<"id">> => <<"123">>}, %% Missing name
    ?assertEqual(false, json_validator:validate(Record, Json)).

validate_extra_fields_test() ->
    Record = #organization{id = "123", name = "Test Org"},
    Json = #{<<"id">> => <<"123">>, <<"name">> => <<"Test Org">>, <<"extra">> => <<"value">>},
    ?assertEqual(false, json_validator:validate(Record, Json)).

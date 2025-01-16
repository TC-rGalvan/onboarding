-module(redis_handler_tests).
-include_lib("eunit/include/eunit.hrl").

-export([setup/0, teardown/0, redis_handler_tests_/0]).

%% Setup and teardown for mocking
setup() ->
    meck:new(eredis, [unstick, passthrough]),
    ok.

teardown() ->
    meck:unload(eredis).

% Test for `create/3`
% create_test() ->
%     % Mock the Redis JSON.SET command
%     meck:expect(eredis, q, fun(_, ["JSON.SET", "type:id", "$", <<"{\"key\":\"value\"}">>]) ->
%         {ok, <<"OK">>}
%     end),

%     % Call the `create/3` function
%     Result = redis_handler:create("type", "id", #{<<"key">> => <<"value">>}),

%     % Assert the expected result
%     ?assertEqual({ok, <<"OK">>}, Result).

% Test for `read/2`
read_test() ->
    % Mock the Redis JSON.GET command
    meck:expect(eredis, q, fun(_, ["JSON.GET", "type:id"]) ->
        {ok, "{\"key\":\"value\"}"}
    end),

    % Call the `read/2` function
    Result = redis_handler:read("type", "id"),

    % Assert the expected result
    ?assertEqual("{\"key\":\"value\"}", Result).

% Test for `read_all/1`
% read_all_test() ->
%     % Mock the Redis KEYS and JSON.GET commands
%     meck:expect(eredis, q, fun(_, ["KEYS", "type:*"]) ->
%         {ok, [<<"type:1">>, <<"type:2">>]}
%     end),
%     meck:expect(eredis, q, fun(_, ["JSON.GET", <<"type:1">>]) ->
%         {ok, "{\"id\":\"1\",\"key\":\"value1\"}"}
%     end),
%     meck:expect(eredis, q, fun(_, ["JSON.GET", <<"type:2">>]) ->
%         {ok, "{\"id\":\"2\",\"key\":\"value2\"}"}
%     end),

%     % Call the `read_all/1` function
%     Result = redis_handler:read_all("type"),

%     % Assert the expected result
%     ?assertEqual(["{\"id\":\"1\",\"key\":\"value1\"}", "{\"id\":\"2\",\"key\":\"value2\"}"], Result).

% Test for `update/3`
% update_test() ->
%     % Mock the Redis JSON.SET command
%     meck:expect(eredis, q, fun(_, ["JSON.SET", "type:id", "$", "{\"key\":\"new-value\",\"id\":\"id\"}"]) ->
%         {ok, <<"OK">>}
%     end),

%     % Call the `update/3` function
%     Result = redis_handler:update("type", "id", #{<<"key">> => <<"new-value">>}),

%     % Assert the expected result
%     ?assertEqual({ok, "{\"key\":\"new-value\",\"id\":\"id\"}"}, Result).

%% Test for `delete/2`
delete_test() ->
    % Mock the Redis DEL command
    meck:expect(eredis, q, fun(_, ["DEL", "type:id"]) ->
        {ok, <<"1">>}
    end),

    % Call the `delete/2` function
    Result = redis_handler:delete("type", "id"),

    % Assert the expected result
    ?assertEqual({ok, <<"1">>}, Result).

% Test for `exists/2`
exists_test() ->
    % Mock the Redis EXISTS command
    meck:expect(eredis, q, fun(_, ["EXISTS", "type:id"]) ->
        {ok, <<"1">>}
    end),

    % Call the `exists/2` function
    Result = redis_handler:exists("type", "id"),

    % Assert the expected result
    ?assert(Result).

% Test for `put_id/1`
put_id_test() ->
    % Call the `put_id/1` function
    Result = redis_handler:put_id(#{<<"key">> => <<"value">>}),

    % Assert the result includes a UUID and the original data
    ?assert(maps:is_key(<<"id">>, Result)),
    ?assertEqual(<<"value">>, maps:get(<<"key">>, Result)).

% Test for `search_by_name/2`
search_by_name_test() ->
    % Mock the Redis FT.SEARCH command
    meck:expect(eredis, q, fun(_, ["FT.SEARCH", "type_index", "'@name:(test)'"]) ->
        {ok, [<<"1">>, <<"type:1">>, "{\"id\":\"123\", \"name\":\"test\"}"]}
    end),

    % Call the `search_by_name/2` function
    Result = redis_handler:search_by_name("type", "test"),

    % Assert the expected result
    ?assertEqual("{\"key\":\"value\"}", Result).

% Test for `search_by_type/2`
search_by_type_test() ->
    % Mock the Redis FT.SEARCH command
    meck:expect(eredis, q, fun(_, ["FT.SEARCH", "type_index", "'@type:(example)'"]) ->
        {ok, [<<"1">>, <<"type:1">>, "{\"id\":\"123\", \"type\":\"example\"}"]}
    end),

    % Call the `search_by_type/2` function
    Result = redis_handler:search_by_type("type", "example"),

    % Assert the expected result
    ?assertEqual("{\"key\":\"value\"}", Result).

%% Test for `search_by_field/3`
search_by_field_test() ->
    % Mock the Redis FT.SEARCH command
    meck:expect(eredis, q, fun(_, ["FT.SEARCH", "type_index", "'@field:(value)'"]) ->
        {ok, [<<"1">>, <<"type:1">>, "{\"id\":\"123\", \"field\":\"value\"}"]}
    end),

    % Call the `search_by_field/3` function
    Result = redis_handler:search_by_field("field", "type", "value"),

    % Assert the expected result
    ?assertEqual("{\"key\":\"value\"}", Result).

% Setup and teardown hooks for each test case
redis_handler_tests_() ->
    {setup,
     fun setup/0,
     fun teardown/0,
    [
        create_test,
        read_test,
        read_all_test,
        update_test,
        delete_test,
        exists_test,
        put_id_test,
        search_by_name_test,
        search_by_type_test,
        search_by_field_test
     ]}.


-module(organization_handler_SUITE).

%% Common Test exports
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([post_organization_test/1, put_organization_test/1, get_organization_test/1,
         get_all_organizations_test/1, delete_organization_test/1]).

%% Include Common Test macros
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Dependencies
-define(BASE_URL, "http://127.0.0.1:8080/organizations").

%% Test Cases
all() ->
    [post_organization_test, put_organization_test, get_organization_test,
     get_all_organizations_test, delete_organization_test].

%% Per-Suite Setup/Teardown
init_per_suite(Config) ->
    %% Start the Cowboy server with the `organization_handler`
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/organizations/[...]", organization_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),

    %% Mock dependencies
    meck:new(redis_handler),
    meck:new(json_validator),

    Config.

end_per_suite(_Config) ->
    %% Stop the Cowboy server
    cowboy:stop_listener(my_http_listener),
    %% Unload mocks
    meck:unload(redis_handler),
    meck:unload(json_validator),
    ok.

%% Per-Testcase Setup/Teardown
init_per_testcase(_TestCase, Config) ->
    %% Reset mocks for each test
    meck:validate(redis_handler),
    meck:validate(json_validator),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% Test Cases

%% Test POST /organizations
post_organization_test(_Config) ->
    %% Mock `redis_handler:put_id` and `redis_handler:create`
    meck:expect(redis_handler, put_id, fun(Json) -> maps:put(<<"id">>, <<"123">>, Json) end),
    meck:expect(redis_handler, create, fun(_, _, _) -> {ok, <<"OK">>} end),
    meck:expect(json_validator, validate, fun(_, _) -> true end),

    %% Perform HTTP POST
    Body = <<"{\"name\": \"New Org\"}">>,
    {ok, {{_, 201, _}, _, ResponseBody}} =
        httpc:request(post, {?BASE_URL, [], "application/json", Body}, [], []),

    %% Validate Response
    ExpectedResponse = <<"{\"name\":\"New Org\",\"id\":\"123\"}">>,
    ?assertEqual(ExpectedResponse, ResponseBody).

%% Test PUT /organizations/{id}
put_organization_test(_Config) ->
    %% Mock `redis_handler:update`
    meck:expect(redis_handler, update, fun(_, "123", _) -> {ok, <<"{\"name\":\"Updated Org\",\"id\":\"123\"}">>} end),
    meck:expect(json_validator, validate, fun(_, _) -> true end),

    %% Perform HTTP PUT
    Url = ?BASE_URL ++ "/123",
    Body = <<"{\"name\": \"Updated Org\", \"id\": \"123\"}">>,
    {ok, {{_, 200, _}, _, ResponseBody}} =
        httpc:request(put, {Url, [], "application/json", Body}, [], []),

    %% Validate Response
    ExpectedResponse = <<"{\"name\":\"Updated Org\",\"id\":\"123\"}">>,
    ?assertEqual(ExpectedResponse, ResponseBody).

%% Test GET /organizations/{id}
get_organization_test(_Config) ->
    %% Mock `redis_handler:read`
    meck:expect(redis_handler, read, fun(_, "123") -> <<"{\"name\":\"Test Org\",\"id\":\"123\"}">> end),

    %% Perform HTTP GET
    Url = ?BASE_URL ++ "/123",
    {ok, {{_, 200, _}, _, ResponseBody}} = httpc:request(get, {Url, []}, [], []),

    %% Validate Response
    ExpectedResponse = <<"{\"name\":\"Test Org\",\"id\":\"123\"}">>,
    ?assertEqual(ExpectedResponse, ResponseBody).

%% Test GET /organizations/all
get_all_organizations_test(_Config) ->
    %% Mock `redis_handler:read_all`
    meck:expect(redis_handler, read_all, fun(_) -> [
        <<"{\"name\":\"Org 1\",\"id\":\"1\"}">>,
        <<"{\"name\":\"Org 2\",\"id\":\"2\"}">>
    ] end),

    %% Perform HTTP GET
    Url = ?BASE_URL ++ "/all",
    {ok, {{_, 200, _}, _, ResponseBody}} = httpc:request(get, {Url, []}, [], []),

    %% Validate Response
    ExpectedResponse = <<"[{\"name\":\"Org 1\",\"id\":\"1\"},{\"name\":\"Org 2\",\"id\":\"2\"}]">>,
    ?assertEqual(ExpectedResponse, ResponseBody).

%% Test DELETE /organizations/{id}
delete_organization_test(_Config) ->
    %% Mock `redis_handler:delete`
    meck:expect(redis_handler, delete, fun(_, "123") -> ok end),

    %% Perform HTTP DELETE
    Url = ?BASE_URL ++ "/123",
    {ok, {{_, 204, _}, _, _}} = httpc:request(delete, {Url, []}, [], []),

    %% Validate Response (204 No Content, so no body expected)
    ok.

-module(organization_handler_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Common Test Callbacks
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test Cases
-export([test_get_all_organizations/1,
         test_get_single_organization/1,
         test_create_organization/1]).

%%% Setup and Teardown %%%

init_per_suite(Config) ->
    io:format("Starting test suite setup...~n"),
    %% Reset mock dependencies (e.g., Redis)
    redis_handler_mock:reset(),
    {ok, Config}.

end_per_suite(_Config) ->
    io:format("Cleaning up after test suite...~n"),
    ok.

init_per_testcase(TestCase, Config) ->
    io:format("Initializing test case: ~p~n", [TestCase]),
    redis_handler_mock:reset(),
    {ok, Config}.

end_per_testcase(TestCase, _Config) ->
    io:format("Cleaning up after test case: ~p~n", [TestCase]),
    ok.

%%% Test Cases %%%

%% Test retrieving all organizations
test_get_all_organizations(_Config) ->
    %% Arrange: Mock Redis to return a list of organizations
    redis_handler_mock:expect_read_all("org", [
        #{<<"id">> => <<"1">>, <<"name">> => <<"Org A">>},
        #{<<"id">> => <<"2">>, <<"name">> => <<"Org B">>}
    ]),

    %% Simulate the handler's behavior for GET /organizations/all
    Req = #{method => <<"GET">>, path => "/organizations/all"},
    {cowboy_rest, Req1, State} = organization_handler:init(Req, #{}),
    AllowedMethods = organization_handler:allowed_methods(Req1, State),

    %% Assert that GET is allowed for this operation
    ?assertMatch({[<<"GET">>], Req1, State}, AllowedMethods),

    %% Act: Call the handler to retrieve organizations
    Response = organization_handler:handle_rest(Req1, State),

    %% Assert: Validate the response from the mocked Redis call
    ?assertMatch({[
        #{<<"id">> := <<"1">>, <<"name">> := <<"Org A">>},
        #{<<"id">> := <<"2">>, <<"name">> := <<"Org B">>}
    ], Req1, State}, Response).

%% Test retrieving a single organization by ID
test_get_single_organization(_Config) ->
    %% Arrange: Mock Redis to return a specific organization
    redis_handler_mock:expect_read("org", "1", #{<<"id">> => <<"1">>, <<"name">> => <<"Org A">>}),

    %% Simulate the handler's behavior for GET /organizations/1
    Req = #{method => <<"GET">>, path => "/organizations/1"},
    {cowboy_rest, Req1, State} = organization_handler:init(Req, #{}),
    AllowedMethods = organization_handler:allowed_methods(Req1, State),

    %% Assert that GET is allowed for this operation
    ?assertMatch({[<<"GET">>], Req1, State}, AllowedMethods),

    %% Act: Call the handler to retrieve the organization
    Response = organization_handler:handle_rest(Req1, State),

    %% Assert: Validate the response from the mocked Redis call
    ?assertMatch({#{<<"id">> := <<"1">>, <<"name">> := <<"Org A">>}, Req1, State}, Response).

%% Test creating a new organization
test_create_organization(_Config) ->
    %% Arrange: Mock Redis to allow creating a new organization
    NewOrg = #{<<"name">> => <<"New Org">>},
    redis_handler_mock:expect_create("org", "Id123", NewOrg, {ok, <<"OK">>}),

    %% Simulate the handler's behavior for POST /organizations
    Req = #{method => <<"POST">>, path => "/organizations", body => jsx:encode(NewOrg)},
    {cowboy_rest, Req1, State} = organization_handler:init(Req, #{}),
    AllowedMethods = organization_handler:allowed_methods(Req1, State),

    %% Assert that POST is allowed for this operation
    ?assertMatch({[<<"POST">>, <<"GET">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>], Req1, State}, AllowedMethods),

    %% Act: Call the handler to create the organization
    Response = organization_handler:handle_rest(Req1, State),

    %% Assert: Validate the response from the mocked Redis call
    ?assertMatch({ok, #{<<"id">> := _Id, <<"name">> := <<"New Org">>}, _}, Response).

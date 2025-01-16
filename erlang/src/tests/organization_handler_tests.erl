-module(organization_handler_tests).

-include_lib("eunit/include/eunit.hrl").
% Include etest's assertion macros.
% -include_lib("etest/include/etest.hrl").

-export([teardown/0, setup/0, organization_handler_tests_/0]).

setup() ->
    %% Set up mock for redis_handler
    meck:new(redis_handler, [passthrough]),
    ok.

teardown() ->
    %% Unload and clean up the mock
    meck:unload(redis_handler).

%% Test suite
organization_handler_tests_() ->
    {setup,
     fun setup/0,
     fun teardown/0,
     [
         init_list_operation_test,
         init_single_operation_with_id_test,
         allowed_methods_list_operation_test,
         allowed_methods_single_operation_test,
         handle_get_all_test,
         handle_get_by_id_found_test,
         handle_get_by_id_not_found_test,
         delete_resource_test
     ]}.

%% --------------------------------------------------------------------------
%% Test cases
%% --------------------------------------------------------------------------

%% Test init/2 with list operation
init_list_operation_test() ->
    %% Arrange
    Req = #{path => <<"/organizations/all">>},
    ExpectedState = #{operation => list},
    meck:expect(cowboy_req, path, fun (_) -> <<"/organizations/all">> end),

    %% Act
    {cowboy_rest, ReqOut, State} = organization_handler:init(Req, #{}),

    %% Assert
    ?assertMatch(Req, ReqOut),
    ?assertEqual(ExpectedState, State).

%% Test init/2 with single operation (with ID)
init_single_operation_with_id_test() ->
    %% Arrange
    Req = #{path => <<"/organizations/123">>},
    ExpectedState = #{operation => single, id => "123"},
    meck:expect(cowboy_req, path, fun (_) -> <<"/organizations/123">> end),

    %% Act
    {cowboy_rest, ReqOut, State} = organization_handler:init(Req, #{}),

    %% Assert
    ?assertMatch(Req, ReqOut),
    ?assertEqual(ExpectedState, State).

%% Test allowed_methods/2 for list operation
allowed_methods_list_operation_test() ->
    %% Arrange
    Req = #{},
    State = #{operation => list},
    ExpectedMethods = <<"GET">>,

    %% Act
    {[Methods], ReqOut, StateOut} = organization_handler:allowed_methods(Req, State),

    %% Assert
    ?assertEqual(ExpectedMethods, Methods),
    ?assertMatch(Req, ReqOut),
    ?assertEqual(State, StateOut).

%% Test allowed_methods/2 for single operation
allowed_methods_single_operation_test() ->
    %% Arrange
    Req = #{},
    State = #{operation => single},
    ExpectedMethods = [<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>],

    %% Act
    {Methods, ReqOut, StateOut} = organization_handler:allowed_methods(Req, State),
    
    %% Assert
    ?assertEqual(ExpectedMethods, Methods),
    ?assertMatch(Req, ReqOut),
    ?assertEqual(State, StateOut).

% %% Test handle_get_all/2 (happy path)
handle_get_all_test() ->
    %% Arrange
    Req = #{},
    State = #{operation => list},
    ExpectedResponse = <<"">>,

    %% Mock redis_handler:read_all
    meck:expect(redis_handler, read_all, fun ("org") -> ExpectedResponse end),

    % io:format("Formatted response: ~p~n", [ExpectedResponse]),
    io:format("Actualresponse: ~p~n", [organization_handler:handle_get_all(Req, State)]),
    %% Act
    {Response, _ReqOut, _StateOut} = organization_handler:handle_get_all(Req, State),

    %% Assert
    ?assertEqual(ExpectedResponse, Response).
    % ?assertMatch(Req, ReqOut),
    % ?assertEqual(State, StateOut).

% Test handle_get_by_id/3 when ID is found
handle_get_by_id_found_test() ->
    % Arrange
    Req = #{},
    State = #{},
    Id = "123",
    ExpectedResponse = #{id => "123", name => "Org123"},

    % Mock redis_handler:read
    meck:expect(redis_handler, read, fun ("org", _Id) -> ExpectedResponse end),

    % Act
    {Response, _ReqOut, _StateOut} = organization_handler:handle_get_by_id(Id, Req, State),

    % Assert
    ?assertEqual(ExpectedResponse, Response).

% Test handle_get_by_id/3 when ID is not found
handle_get_by_id_not_found_test() ->
    % Arrange
    Req = #{},
    State = #{},
    Id = "999",

    % Mock redis_handler:read to return undefined
    meck:expect(redis_handler, read, fun ("org", _Id) -> undefined end),
    meck:expect(cowboy_req, reply, fun (404, _, _, ReqIn) -> ReqIn end),

    % Act
    {Result, ReqOut, StateOut} = organization_handler:handle_get_by_id(Id, Req, State),

    % Assert
    ?assert(Result),
    ?assertMatch(Req, ReqOut),
    ?assertEqual(State, StateOut).

% Test delete_resource/2
delete_resource_test() ->
    %% Arrange
    Req = #{},
    State = #{id => "123"},
    Id = id,

    %% Mock redis_handler:delete
    meck:expect(redis_handler, delete, fun ("org", _Id) -> io:format("Id deleted: ~p~n", [Id]), ok end),

    %% Act
    {Response, ReqOut, StateOut} = organization_handler:delete_resource(Req, State),

    %% Assert
    ?assertEqual(ok, Response),
    ?assertMatch(Req, ReqOut),
    ?assertEqual(State, StateOut).

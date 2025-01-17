-module(user_validator).
-export([validate/2]).
-include("../records/records.hrl").

%% @doc Validate the organization and role of a user exists before creating the user.
%% @spec validate(Organization, Role) -> {ok, OrganizationName, RoleName} | {error, Reason}
%% @param Organization The organization of the user.
%% @param Role The role of the user.
%% @return {ok, OrganizationName, RoleName} if the organization and role exist, {error, Reason} otherwise.
validate(Organization, Role) ->
    case validate_organization(Organization) of
        {ok, OrganizationName} ->
            case validate_role(Role) of
                {ok, RoleName} ->
                    {ok, OrganizationName, RoleName};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


validate_organization(OrganizationId) ->
    % case uuid:is_valid(<<Organization>>) of
        % true ->
            Valid = redis_handler:read("org", OrganizationId),
            case Valid =:= undefined of
                false ->
                    io:format("Valid: ~p~n", [Valid]),
                    DecodedJson = jsx:decode(Valid),
                    Name = binary_to_list(maps:get(<<"name">>, DecodedJson)),
                    {ok, Name};
                true ->
                    {error, "Organization does not exist."}
            end.
    %     false ->
    %         {error, <<"Invalid UUID for organization.">>}
    % end.

validate_role(RoleId) ->
    % case uuid:is_valid(<<Role>>) of
    %     true ->
            Valid = redis_handler:read("role", RoleId),
            case Valid =:= undefined of
                false ->
                    io:format("Valid: ~p~n", [Valid]),
                    DecodedJson = jsx:decode(Valid),
                    Name = binary_to_list(maps:get(<<"name">>, DecodedJson)),
                    {ok, Name};
                true  ->
                    {error, "Role does not exist."}
            end.
    %     false ->
    %         {error, <<"Invalid UUID for role.">>}
    % end.
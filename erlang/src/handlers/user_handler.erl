-module(user_handler).
-behavior(cowboy_rest).

-export([init/2]).
-export([allowed_methods/2, content_types_accepted/2, content_types_provided/2]).
-export([resource_exists/2, delete_resource/2, is_conflict/2]).
-export([handle_rest/2]).

-include("../records/records.hrl").

init(Req, _State) ->
    Path = binary_to_list(cowboy_req:path(Req)),
    case string:split(Path, "/", all) of
        ["", "users", "all"] ->
            {cowboy_rest, Req, #{operation => list}};
        ["", "users", Id] ->
            {cowboy_rest, Req, #{operation => single, id => Id}};
        ["", "users"] ->
            {cowboy_rest, Req, #{operation => single}};
        _ ->
            {cowboy_rest, Req, #{operation => unknown}}
    end.

allowed_methods(Req, State) ->
    case maps:get(operation, State) of
        list ->
            {[<<"GET">>], Req, State};
        single ->
            {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}
    end.

%% handle callbacks distinct by method (get-post)
content_types_accepted(Req, State) ->
    io:format("accepted~n"),
    {[{{<<"application">>, <<"json">>, '*'}, handle_rest}], Req, State}.

content_types_provided(Req, State) ->
    io:format("provided~n"),
    {[{{<<"application">>, <<"json">>, '*'}, handle_rest}], Req, State}.

handle_rest(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> ->
            case maps:get(operation, State) of
                single ->
                    Id = maps:get(id, State),
                    handle_get_by_id(Id, Req, State);
                list ->
                    handle_get_all(Req, State)
            end;
        <<"POST">> ->
            handle_create(Req, State);
        <<"PUT">> ->
            handle_update(Req, State);
        <<"DELETE">> ->
            delete_resource(Req, State)
    end.

handle_get_all(Req, State) ->
    {redis_handler:read_all("user"), Req, State}.

handle_get_by_id(Id, Req, State) ->
    case redis_handler:read("user", Id) of
        undefined ->
            Req1 =
                cowboy_req:reply(404,
                                 #{<<"content-type">> => <<"application/json">>},
                                 <<"{\"error\": \"User not found\"}">>,
                                 Req),
            {true, Req1, State};
        Value ->
            {Value, Req, State}
    end.

resource_exists(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> ->
            {true, Req, State};
        _ ->
            entity_exists(Req, State)
    end.

is_conflict(Req, State) ->
    {ok, Body, _} = cowboy_req:read_body(Req),

    StateWithBody = maps:put(body, Body, State),
    BinaryBody = jsx:decode(Body),
    Name = binary_to_list(maps:get(<<"first_name">>, BinaryBody)),

    case redis_handler:search_by_name("user", Name) of
        %% User not exists, proceed with update
        false ->
            {false, Req, StateWithBody};
        %% User exists, evaluates if it's the same object,
        %% Same object: proceed with update
        %% Not same object: return conflict
        JsonObject ->
            IdFromState = maps:get(id, State),
            BinaryObject = jsx:decode(JsonObject),
            IdFromDb = binary_to_list(maps:get(<<"id">>, BinaryObject)),

            case IdFromState =:= IdFromDb of
                true ->
                    % Req2 = cowboy_req:set_resp_body(Body, Req1),
                    {false, Req, StateWithBody};
                false ->
                    Req2 =
                        cowboy_req:reply(409,
                                         #{<<"content-type">> => <<"application/json">>},
                                         <<"{\"error\": \"User cannot be updated\"}">>,
                                         Req),
                    {true, Req2, StateWithBody}
            end
    end.

entity_exists(Req, State) ->
    case maps:is_key(id, State) of
        false ->
            {false, Req, State};
        true ->
            Id = maps:get(id, State),
            {redis_handler:exists("user", Id), Req, State}
    end.

handle_create(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    BinaryBody = jsx:decode(Body),
    BinaryBody1 = redis_handler:put_id(BinaryBody),

    Id = binary_to_list(maps:get(<<"id">>, BinaryBody1)),
    FirstName = binary_to_list(maps:get(<<"first_name">>, BinaryBody1)),
    Organization = binary_to_list(maps:get(<<"organization">>, BinaryBody1)),
    Role = binary_to_list(maps:get(<<"role">>, BinaryBody1)),
    User = #user{id = Id, first_name = FirstName},


    case user_validator:validate(Organization, Role) of
        {ok, OrganizationName, RoleName} ->
            case json_validator:validate(User, BinaryBody1) of
                true ->
                    case redis_handler:search_by_name("user", FirstName) of
                        false ->
                            case redis_handler:create("user", Id, BinaryBody1) of
                                {ok, <<"OK">>} ->
                                    BinaryBody2 = maps:remove(<<"organization">>, BinaryBody1),
                                    BinaryBody3 = maps:remove(<<"role">>, BinaryBody2),
                                    BinaryBody4 = maps:put(<<"organization">>, list_to_binary(OrganizationName), BinaryBody3),
                                    BinaryBody5 = maps:put(<<"role">>, list_to_binary(RoleName), BinaryBody4),
                                    cowboy_req:reply(201,
                                                     #{<<"content-type">> => <<"application/json">>},
                                                     jsx:encode(BinaryBody5),
                                                     Req1)
                            end;
                        JsonObject ->
                            MapObject = jsx:decode(JsonObject),
                            Name = binary_to_list(maps:get(<<"name">>, MapObject)),
                            Message =
                                string:join(["{\"error\": \"User ", " already exists\"}"], Name),
                            cowboy_req:reply(409,
                                             #{<<"content-type">> => <<"application/json">>},
                                             Message,
                                             Req)
                    end;
                false ->
                    Req2 =
                        cowboy_req:reply(400,
                                         #{<<"content-type">> => <<"application/json">>},
                                         <<"{\"error\": \"Bad Json Model\"}">>,
                                         Req),
                    {true, Req2, State}
            end;
        {error, Reason} ->
            io:format("Reason: ~p~n", [Reason]),
            Error = string:join(["{\"error\": \"", Reason, "\"}"],""),
            Req2 =
                cowboy_req:reply(400,
                                 #{<<"content-type">> => <<"application/json">>},
                                 Error,
                                 Req),
            {true, Req2, State}
    end.

   

handle_update(Req, State) ->
    Body = maps:get(body, State),
    BinaryBody = jsx:decode(Body),

    Id = maps:get(id, State),
    FirstName = maps:get(<<"first_name">>, BinaryBody),
    User = #user{id = Id, first_name = FirstName},

    Organization = binary_to_list(maps:get(<<"organization">>, BinaryBody)),
    Role = binary_to_list(maps:get(<<"role">>, BinaryBody)),

    case user_validator:validate(Organization, Role) of
        {ok, OrganizationName, RoleName} ->
            case json_validator:validate(User, BinaryBody) of
                true ->
                    case redis_handler:update("user", Id, BinaryBody) of
                        {ok, _UserUpdated} ->
                            BinaryBody1 = maps:remove(<<"organization">>, BinaryBody),
                            BinaryBody2 = maps:remove(<<"role">>, BinaryBody1),
                            BinaryBody3 = maps:put(<<"organization">>, list_to_binary(OrganizationName), BinaryBody2),
                            BinaryBody4 = maps:put(<<"role">>, list_to_binary(RoleName), BinaryBody3),
                            Req2 = cowboy_req:set_resp_body(jsx:encode(BinaryBody4), Req),
                            {true, Req2, State};
                        _ ->
                            Req2 =
                                cowboy_req:reply(500,
                                                 #{<<"content-type">> => <<"application/json">>},
                                                 <<"{\"error\": \"Failed to update user\"}">>,
                                                 Req),
                            {true, Req2, State}
                    end;
                false ->
                    Req2 =
                        cowboy_req:reply(400,
                                         #{<<"content-type">> => <<"application/json">>},
                                         <<"{\"error\": \"Bad Json Model\"}">>,
                                         Req),
                    {true, Req2, State}
            end;
        {error, Reason} ->
            io:format("Reason: ~p~n", [Reason]),
            Error = string:join(["{\"error\": \"", Reason, "\"}"],""),
            Req2 =
                cowboy_req:reply(400,
                                 #{<<"content-type">> => <<"application/json">>},
                                 Error,
                                 Req),
            {true, Req2, State}
    end.

delete_resource(Req, State) ->
    Id = maps:get(id, State),
    {redis_handler:delete("user", Id), Req, State}.

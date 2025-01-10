-module(organization_handler).

-behavior(cowboy_rest).

-export([init/2]).
-export([allowed_methods/2, content_types_accepted/2, content_types_provided/2]).
-export([resource_exists/2, delete_resource/2, is_conflict/2]).
-export([handle_json/2]).

-include("../records/records.hrl").

init(Req, _State) ->
    Path = binary_to_list(cowboy_req:path(Req)),
    case string:split(Path, "/", all) of
        ["", "organization", "all"] ->
            {cowboy_rest, Req, #{operation => list}};
        ["", "organization", Id] ->
            {cowboy_rest, Req, #{operation => single, id => Id}};
        ["", "organization"] ->
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
    {[{{<<"application">>, <<"json">>, '*'}, handle_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, handle_json}], Req, State}.

handle_json(Req, State) ->
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
    {redis_handler:read_all("org"), Req, State}.

handle_get_by_id(Id, Req, State) ->
    case redis_handler:read("org", Id) of
        Value ->
            Value;
        notfound ->
            Req1 =
                cowboy_req:reply(409,
                                 #{<<"content-type">> => <<"application/json">>},
                                 <<"{\"error\": \"Organization already exists\"}">>,
                                 Req),
            {true, Req1, State}
    end.

resource_exists(Req, State) ->
    io:format("resource_exists hit ~p~n", [State]),
    case cowboy_req:method(Req) of
        <<"GET">> ->
            io:format("GET"),
            {true, Req, State};
        _ ->
            entity_exists(Req, State)
    end.

is_conflict(Req, State) ->
    {ok, Body, _} = cowboy_req:read_body(Req),

    StateWithBody = maps:put(body, Body, State),
    BinaryBody = jsx:decode(Body),
    Name = binary_to_list(maps:get(<<"name">>, BinaryBody)),

    case redis_handler:search_by_name("org", Name) of
		%% Organization not exists, proceed with update
        false ->
            {false, Req, StateWithBody};
		%% Organization exists, evaluates if it's the same object,
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
                                         <<"{\"error\": \"Organization already exists\"}">>,
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
            {redis_handler:exists("org", Id), Req, State}
    end.

handle_create(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    BinaryBody = jsx:decode(Body),
    io:format("BinaryBody: ~p~n", [BinaryBody]),
    BinaryBody1 = redis_handler:put_id(BinaryBody),

    Id = binary_to_list(maps:get(<<"id">>, BinaryBody1)),
    Name = binary_to_list(maps:get(<<"name">>, BinaryBody1)),
    Organization = #organization{id = Id, name = Name},
    io:format("Id handle_create: ~p~n", [Id]),
    io:format("Body handle_create: ~p~n", [BinaryBody1]),
    case json_validator:validate(Organization, BinaryBody1) of
        true ->
            case redis_handler:create("org", Id, BinaryBody1) of
                {ok, <<"OK">>} ->
                    Req2 =
                        cowboy_req:reply(201,
                                         #{<<"content-type">> => <<"application/json">>},
                                         jsx:encode(BinaryBody1),
                                         Req1),
                    {true, Req2, State}
            end;
        false ->
            Req2 =
                cowboy_req:reply(400,
                                 #{<<"content-type">> => <<"application/json">>},
                                 "Incorrect Json Model",
                                 Req1),
            {true, Req2, State}
    end.

handle_update(Req, State) ->
    % {ok, Body, Req1} = cowboy_req:read_body(Req),
    io:format("State: ~p~n", [State]),
    Body = maps:get(body, State),
    io:format("BodyToUpdate: ~p~n", [Body]),
    BinaryBody = jsx:decode(Body),
    io:format("BinaryBody: ~p~n", [BinaryBody]),
    Id = maps:get(<<"id">>, BinaryBody),
    Name = maps:get(<<"name">>, BinaryBody),

    Organization = #organization{id = Id, name = Name},
    case json_validator:validate(Organization, BinaryBody) of
        true ->
            case redis_handler:update("org", Id, jsx:encode(BinaryBody)) of
                {ok, OrgUpdated} ->
                    Req2 = cowboy_req:set_resp_body(OrgUpdated, Req),
                    {true, Req2, State};
                _ ->
                    {<<"{\"error\": \"Failed to update organization\"}">>, Req, State}
            end;
        false ->
            Req2 =
                cowboy_req:reply(400,
                                 #{<<"content-type">> => <<"application/json">>},
                                 "Incorrect Json Model",
                                 Req),
            {true, Req2, State}
    end.

delete_resource(Req, State) ->
    Id = maps:get(id, State),
    io:format("delete organization: ~p~n", [Id]),
    {redis_handler:delete("org", Id), Req, State}.

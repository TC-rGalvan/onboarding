-module(redis_handler).

-export([start_link/0, create/3, read/2, read_all/1, update/3, delete/2, exists/2,
         put_id/1, search_by_name/2, search_by_field/3, search_by_type/2]).

-include("../records/records.hrl").

start_link() ->
    eredis:start_link("redis", 6379). %% just once per app

create(Type, Id, BinaryData) ->
    {ok, C} = start_link(),
    Data = jsx:encode(BinaryData),
    RedisArgs = string:join([Type, Id], ":"),
    RedisResponse = eredis:q(C, ["JSON.SET", RedisArgs, "$", Data]),
    io:format("RedisResponse: ~p~n", [RedisResponse]),
    case RedisResponse of
        {ok, <<"OK">>} ->
            {ok, <<"OK">>};
        {error, Reason} ->
            {error, string:join([Reason, RedisArgs], " ")}
    end.

read(Type, Id) ->
    {ok, C} = start_link(),
    RedisArgs = string:join([Type, Id], ":"),
    io:format("RedisArgs: ~p~n", [RedisArgs]),
    case eredis:q(C, ["JSON.GET", RedisArgs]) of
        {ok, Value} ->
            case Value =:= undefined of
                false ->
                    case Type of 
                        "user" ->
                            io:format("Value: ~p~n", [Value]),
                            BinaryBody = jsx:decode(Value),
                            {ok, OrganizationName} = get_organization_name(binary_to_list(maps:get(<<"organization">>, BinaryBody))),
                            {ok, RoleName} = get_role_name(binary_to_list(maps:get(<<"role">>, BinaryBody))),
                            BinaryBody1 = maps:remove(<<"organization">>, BinaryBody),
                            BinaryBody2 = maps:remove(<<"role">>, BinaryBody1),
                            BinaryBody3 = maps:put(<<"organization">>, list_to_binary(OrganizationName), BinaryBody2),
                            BinaryBody4 = maps:put(<<"role">>, list_to_binary(RoleName), BinaryBody3),
                            jsx:encode(BinaryBody4);
                        _ ->
                            Value
                    end;
                true ->
                    undefined
            end
    end.
get_organization_name(OrganizationId) ->
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

get_role_name(RoleId) -> 
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

read_all(Type) ->
    {ok, C} = start_link(),
    RedisArgs = string:join([Type, "*"], ":"),
    {ok, Keys} = eredis:q(C, ["KEYS", RedisArgs]),
    JsonValues = lists:map(fun(Key) ->
                               {ok, Value} = eredis:q(C, ["JSON.GET", Key]),
                               case Type of
                                   "user" ->
                                        BinaryBody = jsx:decode(Value),
                                        {ok, OrganizationName} = get_organization_name(binary_to_list(maps:get(<<"organization">>, BinaryBody))),
                                        {ok, RoleName} = get_role_name(binary_to_list(maps:get(<<"role">>, BinaryBody))),
                                        BinaryBody1 = maps:remove(<<"organization">>, BinaryBody),
                                        BinaryBody2 = maps:remove(<<"role">>, BinaryBody1),
                                        BinaryBody3 = maps:put(<<"organization">>, list_to_binary(OrganizationName), BinaryBody2),
                                        BinaryBody4 = maps:put(<<"role">>, list_to_binary(RoleName), BinaryBody3),
                                        jsx:encode(BinaryBody4);

                                    _ ->
                                        Value
                               end
                           end, Keys),
    JsonValues.

update(Type, Id, Data) ->
    {ok, C} = start_link(),
    RedisArgs = string:join([Type, Id], ":"),
    io:format("args: ~p~n", [RedisArgs]),
    io:format("Data: ~p~n", [Data]),

    DataWithoutId = maps:remove(<<"id">>, Data),
    io:format("DataWithoutId: ~p~n", [DataWithoutId]),
    
    RealData = jsx:encode(maps:put(<<"id">>, list_to_binary(Id), DataWithoutId)),
    io:format("RealData: ~p~n", [RealData]),
    % io:format("RealDataEncoded: ~p~n", [jsx:encode(RealData)]),

    case eredis:q(C, ["JSON.SET", RedisArgs, "$", RealData]) of
        {ok, <<"OK">>} ->
            {ok, RealData};
        {error, _} ->
            {error, "Failed to update record: " ++ RedisArgs}
    end.

delete(Type, Id) ->
    {ok, C} = start_link(),
    RedisArgs = string:join([Type, Id], ":"),
    eredis:q(C, ["DEL", RedisArgs]).

exists(Type, Id) ->
    {ok, C} = start_link(),
    RedisArgs = string:join([Type, Id], ":"),
    io:format("exists: ~p~n", [RedisArgs]),
    case eredis:q(C, ["EXISTS", RedisArgs]) of
        {ok, <<"1">>} ->
            true;
        {ok, <<"0">>} ->
            false
    end.

put_id(MapBody) ->
    case maps:is_key(id, MapBody) of
        false ->
            Id = list_to_binary(uuid:to_string(
                                    uuid:uuid4())),
            maps:put(<<"id">>, Id, MapBody)
    end.

search_by_name(Type, Name) ->
    {ok, C} = start_link(),
    IndexName = string:join([Type, "index"], "_"),
    RedisArgs = string:join(["'@name:(", Name, ")"], ""),
    RedisResponse = eredis:q(C, ["FT.SEARCH", IndexName, RedisArgs]),
    io:format("SearchByName: ~p~n", [RedisResponse]),

    case eredis:q(C, ["FT.SEARCH", IndexName, RedisArgs]) of
        {ok, [<<"0">>]} ->
            false;
        {ok, [<<"1">>, RedisKey, [_, JsonObject]]} ->
            io:format("RedisKey, ~p~n", [RedisKey]),
            JsonObject
    end.

search_by_type(Type, TypeName) ->
    {ok, C} = start_link(),
    IndexName = string:join([Type, "index"], "_"),
    RedisArgs = string:join(["'@type:(", TypeName, ")"], ""),
    RedisResponse = eredis:q(C, ["FT.SEARCH", IndexName, RedisArgs]),
    io:format("SearchByName: ~p~n", [RedisResponse]),

    case eredis:q(C, ["FT.SEARCH", IndexName, RedisArgs]) of
        {ok, [<<"0">>]} ->
            false;
        {ok, [<<"1">>, RedisKey, [_, JsonObject]]} ->
            io:format("RedisKey, ~p~n", [RedisKey]),
            JsonObject
    end.

search_by_field(Field, Type, Value) ->
    {ok, C} = start_link(),
    IndexName = string:join([Type, "index"], "_"),
    RedisArgs = string:join(["'@", Field, ":(", Value, ")"], ""),
    RedisResponse = eredis:q(C, ["FT.SEARCH", IndexName, RedisArgs]),
    io:format("SearchByField: ~p~n", [RedisResponse]),

    case eredis:q(C, ["FT.SEARCH", IndexName, RedisArgs]) of
        {ok, [<<"0">>]} ->
            false;
        {ok, [<<"1">>, RedisKey, [_, JsonObject]]} ->
            io:format("RedisKey, ~p~n", [RedisKey]),
            JsonObject
    end.

-module(redis_handler).
-export([start_link/0, create/3, read/2, read_all/1, update/3, delete/2, exists/2, put_id/1, search_by_name/2]).

-include("../records/records.hrl").

start_link() ->
    eredis:start_link(). %% just once per app

create(Type, Id, BinaryData) ->
    {ok, C} = start_link(),

    Data = jsx:encode(BinaryData),
    io:format("Id: ~p~n", [Id]),
    RedisArgs = string:join([Type, Id], ":"),
    io:format("BeforeSend: ~p~n", [Data]),
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
    {ok, Value} = eredis:q(C, ["JSON.GET", RedisArgs]),
    Value.

read_all(Type) ->
    {ok, C} = start_link(),
    RedisArgs = string:join([Type, "*"], ":"),
    {ok, Keys} = eredis:q(C, ["KEYS", RedisArgs]),
    lists:map(fun(Key) ->
        {ok, Value} = eredis:q(C, ["JSON.GET", Key]),
        Value
    end, Keys).

update(Type, Id, Data) ->
    {ok, C} = start_link(),
    RedisArgs = string:join([Type, integer_to_list(Id)], ":"),
    io:format("args: ~p~n", [RedisArgs]),
    case eredis:q(C, ["SET", RedisArgs, Data]) of
        {ok, <<"OK">>} ->
            {ok, Data};
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
            Id = list_to_binary(uuid:to_string(uuid:uuid4())),
            maps:put(<<"id">>, Id, MapBody)
    end.

search_by_name(Type, Name) ->
    {ok, C} = start_link(),
    IndexName = string:join([Type, "index"], "_"),
    RedisArgs = string:join(["'@name:(", Name, ")" ], ""),
    RedisResponse = eredis:q(C, ["FT.SEARCH", IndexName, RedisArgs]),
    io:format("SearchByName: ~p~n", [RedisResponse]),

    case eredis:q(C, ["FT.SEARCH", IndexName, RedisArgs]) of
        {ok, [<<"0">>]} ->
            false;
        {ok, [<<"1">>, RedisKey, [_, JsonObject]]} ->

            io:format("RedisKey, ~p~n", [RedisKey]),
            JsonObject
    end.


    
     

    
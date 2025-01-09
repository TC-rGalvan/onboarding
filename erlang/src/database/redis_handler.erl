-module(redis_handler).
-export([start_link/0, create/2, read/2, read_all/1, update/3, delete/2, exists/2]).

-include("../records/records.hrl").

start_link() ->
    eredis:start_link().

create(Type, Data) ->
    {ok, C} = start_link(),
    Key = get_key_id(Data),
    io:format("Key: ~p~n", [Key]),
    RedisArgs = string:join([Type, Key], ":"),
    io:format("RedisResponse: ~p~n", [RedisArgs]),
    RedisResponse = eredis:q(C, ["JSON.SET", RedisArgs, Data]),
    io:format("RedisArgs: ~p~n", [RedisResponse]),
    case eredis:q(C, ["SET", RedisArgs, Data]) of
        {ok, <<"OK">>} ->
            io:format("Data: ~p~n", [jsx:decode(Data)]),
            % MapData = jsx:decode(Data),
            %OrgRecord = #organization{id = maps:get(<<"id">>, MapData), name = maps:get(<<"name">>, MapData)},

            {ok, jsx:encode(Data)};
        {error, Reason} ->
            {error, string:join([Reason, RedisArgs], " ")}
    end.
        
read(Type, Id) ->
    {ok, C} = start_link(),
    RedisArgs = string:join([Type, Id], ":"),
    {ok, Value} = eredis:q(C, ["GET", RedisArgs]),
    Value.

read_all(Type) ->
    {ok, C} = start_link(),
    RedisArgs = string:join([Type, "*"], ":"),
    {ok, Keys} = eredis:q(C, ["KEYS", RedisArgs]),
    lists:map(fun(Key) ->
        {ok, Value} = eredis:q(C, ["GET", Key]),
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
    case eredis:q(C, ["EXISTS", RedisArgs]) of
        {ok, <<"1">>} ->
            true;
        {ok, <<"2">>} ->
            false
    end.

get_key_id(Data) ->
    MapData = jsx:decode(Data),
    case maps:is_key(id, MapData) of
        true ->
            maps:get(<<"id">>, MapData);
        false ->
            uuid:to_string(uuid:uuid4())
    end.
    
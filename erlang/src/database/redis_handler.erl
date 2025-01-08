-module(redis_handler).
-export([start_link/0, create/3, read/2, read_all/1, update/3, delete/2, exists/2]).


start_link() ->
    eredis:start_link().

create(Type, Id, Data) ->
    {ok, C} = start_link(),
    RedisArgs = string:join([Type, integer_to_list(Id)], ":"),
    RedisResponse = eredis:q(C, ["SET", RedisArgs, Data]),
    io:format("RedisResponse: ~p~n", [RedisResponse]),
    case eredis:q(C, ["SET", RedisArgs, Data]) of
        {ok, <<"OK">>} ->
            io:format("Data: ~p~n", [Data]),
            {ok, Data};
        {error, _} ->
            {error, "Failed to create record: " ++ RedisArgs}
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

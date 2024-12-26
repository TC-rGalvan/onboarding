-module(redis_handler).
-export([start_link/0, create/3, read/2, update/3, delete/2, list_all/1]).

start_link() ->
    eredis:start_link().

create(Type, Id, Data) ->
    {ok, C} = start_link(),
    eredis:q(C, ["SET", Type ++ ":" ++ Id, Data]).

read(Type, Id) ->
    {ok, C} = start_link(),
    eredis:q(C, ["GET", Type ++ ":" ++ Id]).

update(Type, Id, Data) ->
    {ok, C} = start_link(),
    eredis:q(C, ["SET", Type ++ ":" ++ Id, Data]).

delete(Type, Id) ->
    {ok, C} = start_link(),
    eredis:q(C, ["DEL", Type ++ ":" ++ Id]).

list_all(Type) ->
    {ok, C} = start_link(),
    {ok, Keys} = eredis:q(C, ["KEYS", Type ++ ":*"]),
    lists:map(fun(Key) ->
        {ok, Value} = eredis:q(C, ["GET", Key]),
        Value
    end, Keys).
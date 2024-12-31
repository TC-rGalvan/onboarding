-module(redis_handler).
-export([start_link/0, create/3, read/2, update/3, delete/2, list_all/1]).

start_link() ->
    eredis:start_link().

create(Type, Id, Data) ->
    {ok, C} = start_link(),
    SetArguments = string:join([Type, integer_to_list(Id)], ":"),

    case eredis:q(C, ["SET", SetArguments, Data]) of
        {ok, <<"OK">>} ->
            {ok, Data};
        {error, _} ->
            {error, "Failed to create record: " ++ SetArguments}
    end.
        

read(Type, Id) ->
    {ok, C} = start_link(),
    {ok, Value} = eredis:q(C, ["GET", Type ++ ":" ++ Id]),
    Value.

update(Type, Id, Data) ->
    {ok, C} = start_link(),
    SetArguments = string:join([Type, integer_to_list(Id)], ":"),
    io:format("args: ~p~n", [SetArguments]),
    case eredis:q(C, ["SET", SetArguments, Data]) of
        {ok, <<"OK">>} ->
            {ok, Data};
        {error, _} ->
            {error, "Failed to update record: " ++ SetArguments}
    end.

delete(Type, Id) ->
    {ok, C} = start_link(),
    SetArguments = string:join([Type, Id], ":"),
    eredis:q(C, ["DEL", SetArguments]).

list_all(Type) ->
    {ok, C} = start_link(),
    {ok, Keys} = eredis:q(C, ["KEYS", Type ++ ":*"]),
    lists:map(fun(Key) ->
        {ok, Value} = eredis:q(C, ["GET", Key]),
        Value
    end, Keys).
-module(functions).
-compile([debug_info, export_all]).

head([H | _]) -> H.
second([_, X | _]) -> X.

same(X,X) -> 
    true;
same(_,_) ->
    false.

valid_time({Date = {Y, M, D}, Time = {H, Min, S}}) -> 
    io:format("The date tuple ~p says today is: ~p/~p/~p, ~n", [Date, Y, M, D]),
    io:format("The time tuple ~p says the hour is: ~p:~p:~p, ~n", [Time, H, Min, S]);
valid_time(_) ->
    io:format("Invalid date or time ~n").

old_enough(X) when X >= 16, X =< 104 ->
    true;
old_enough(_) ->
    false.
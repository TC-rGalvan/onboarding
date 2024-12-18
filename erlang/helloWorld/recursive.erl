-module(recursive).
-compile([debug_info, export_all]).
% -export([fac/1, len/1, tail_fac/1, tail_len/1, duplicate/2, tail_duplicate/2, reverse/1, tail_reverse/1, sublist/2, tail_sublist/2]).


fac(0) -> 1;
fac(N) when N > 0 -> N * fac(N-1).

len([]) -> 0;
len([_|T]) -> 1 + len(T).

tail_fac(N) -> tail_fac(N,1).
tail_fac(0, Acc) -> Acc;
tail_fac(N, Acc) when N > 0 -> tail_fac(N - 1, N * Acc). 

tail_len(L) -> tail_len(L,0).
tail_len([], Acc) -> Acc;
tail_len([_|T], Acc) -> tail_len(T, Acc + 1).

duplicate(0, _) -> [];
duplicate(N, Term) when N > 0 ->
    [Term|duplicate(N-1, Term)].

tail_duplicate(N, Term) -> 
    tail_duplicate(N, Term, []).
tail_duplicate(0, _, List) ->
    List;
tail_duplicate(N, Term, List) when N > 0 ->
    tail_duplicate(N - 1, Term, [Term | List]).

reverse([]) -> 
    [];
reverse([H|T]) -> 
    reverse(T)++[H].

tail_reverse(L) -> tail_reverse(L,[]).
tail_reverse([], Acc) -> Acc;
tail_reverse([H | T], Acc) -> tail_reverse(T, [H | Acc]).

sublist(_, 0) -> [];
sublist([], _) -> [];
sublist([H|T], N) when N > 0 -> [H | sublist(T, N - 1)].

%% Always use lists:reverse to efficiently reverse lists
tail_sublist(L, N) -> lists:reverse(tail_sublist(L, N, [])).
tail_sublist(_, 0, Sublist) -> Sublist;
tail_sublist([], _, Sublist) -> Sublist;
tail_sublist([H|T], N, Sublist) when N > 0 -> 
    tail_sublist(T,  N - 1, [H | Sublist]).

zip([], []) -> [];
zip([X | Xs], [Y | Ys]) ->
    [{X, Y} | zip(Xs, Ys)].

tail_zip(List_one, List_two) ->
    lists:reverse(tail_zip(List_one, List_two, [])).
tail_zip([], [], Acc) -> Acc;
tail_zip([X | Xs], [Y | Ys], Acc) ->
    tail_zip(Xs, Ys, [{X, Y } | Acc]).

lenient_zip(_, []) -> [];
lenient_zip([], _) -> [];
lenient_zip([X | Xs], [Y | Ys]) ->
    [{X,  Y} | lenient_zip(Xs, Ys)].

tail_lenient_zip(X, Y) -> tail_lenient_zip(X, Y, []).
tail_lenient_zip(_, [], Acc) -> Acc;
tail_lenient_zip([], _, Acc) -> Acc;
tail_lenient_zip([X | Xs], [Y | Ys], Acc) ->
    [{X, Y} | tail_lenient_zip(Xs, Ys, Acc)].

quicksort([]) -> [];
quicksort([Pivot | Rest]) ->
    {Smaller, Larger} = partition(Pivot, Rest, [], []),
    quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).

partition(_, [], Smaller, Larger) -> {Smaller, Larger};
partition(Pivot, [H|T], Smaller, Larger) ->
    if 
        H =< Pivot -> partition(Pivot, T, [H|Smaller], Larger);
        H > Pivot -> partition(Pivot, T, Smaller, [H|Larger])
end.
    
printHello(0) -> ok;
printHello(N) when N >= 0 -> 
    io:format("Hello world ~n"),
    printHello(N - 1).
-module(hacker_rank).
-compile([debug_info, export_all]).


printHello(0) -> ok;
printHello(N) when N > 0 -> 
    io:format("Hello world ~n"),
    printHello(N - 1).

printListElements(_, []) -> ok;
printListElements(N, [H|T]) when N >= 0 ->
    printElement(N, H),
    printListElements(N, T).

printElement(0, _) -> ok;
printElement(N, Elem) when N > 0 ->
    io:format("~p~n", [Elem]),
    printElement(N - 1, Elem).
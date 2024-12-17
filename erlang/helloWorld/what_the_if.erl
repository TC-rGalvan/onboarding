-module(what_the_if).
-export([oh_god/1, insert/2, beach/1]).


% heh_fine() ->
%     if 1 =:= 1 ->
%         works
%     end,
%     if 1 =:= 2; 1 =:= 1 ->
%         works
%     end,
%     if 1 =:= 2, 1 =:= 1 ->
%         fails
%     end.

oh_god(N) ->
    if N =:= 2 -> might_succeed;
        true -> always_does  %% this is Erlang's if's 'else!'
    end.

insert(X,[]) ->
    [X];
insert(X,Set) ->
    case lists:member(X,Set) of
        true  -> Set;
        false -> [X|Set]
    end.

beach(Temperature) ->
    case Temperature of
    {celsius, N} when N >= 20, N =< 45 ->
        'favorable';
    {kelvin, N} when N >= 293, N =< 318 ->
        'scientifically favorable';
    {farenheit, N} when N >= 68, N =< 113 ->
        'favorable in US';
    _ -> 
        'avoid beach'
    end.
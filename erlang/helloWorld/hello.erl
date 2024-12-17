-module(hello).
-export([add/2, hello/0, greet_and_add_two/1, greet/2]).
%% imports more used are list, evr else is considered a good practice to leave the module nome for readability
%% -import(io:format/2)
%% Define compile options in file (export_all just for dev)
-compile([debug_info, export_all]).

add(A,B) ->
    A + B.

%% Shows greetings.
%% io:format/1 is the standard function used to output text.
hello() ->
    io:format("Hello, world!~n").

greet_and_add_two(X) ->
    hello(),
    add(X,2).

greet(male, Name) ->
    io:format("Hello, Mr. ~s!", [Name]);
greet(female, Name) ->
    io:format("Hello, Mrs. ~s!", [Name]);
greet(_, Name) ->
    io:format("Hello, ~s!", [Name]).

-module(json_handler).
-behavior(cowboy_rest).

-export([init/2, allowed_methods/2, content_types_provided/2, hello_from_json/2]).

init(Req, State) ->
	{cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
	{[<<"GET">>], Req, State}. 

content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, []}, hello_from_json}
	], Req, State}.

hello_from_json(Req, State) ->
	Message = #{hello => <<"Hello world from json!">>},
	{jsx:encode(Message), Req, State}.
	% {ok, Body, Req} = jsx:encode(Message),
	% {ok, Body, Req, State}.

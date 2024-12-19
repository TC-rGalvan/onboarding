-module(json_handler).

-behavior(cowboy_rest).

-export([init/2,
		content_types_provided/2,
		allowed_methods/2,
		to_json/2]).

init(Req, State) ->
	{cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>], Req, State}. 
content_types_provided(Req, State) ->
		{[{{<<"application">>, <<"json">>, []}, to_json}
	], Req, State}.
to_json(Req, State) ->
	Response = #{hello => <<"Hello world from json!">>},
	{jsx:encode(Response), Req, State}.
		% {jiffy:encode(Response), Req, State}.


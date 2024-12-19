-module(json_handler).
-behavior(cowboy_rest).

-export([init/2,
		 allowed_methods/2,
		 content_types_provided/2, content_types_accepted/2,
		 resource_exists/2,
		 to_html/2]).

init(Req, State) ->
	Response = jsx:encode([{<<"name">>, <<"Rodrigo">>}, {<<"age">>, 29}]),

	Req2 = cowboy_req:reply(200,
		#{<<"content-type">> => <<"application/json">>},
		Response,
		Req),
	{cowboy_rest, Req2, State}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>], Req, State}. 

content_types_provided(Req, State) ->
	{[
		{{<<"text">>, <<"html">>, '*'}, to_html}
	], Req, State}.

content_types_accepted(Req, State) ->
	{[
		{<<"application/json">>, register_from_json}
	], Req, State}.

%% validates that method requestes is allowed
resource_exists(Req, State) ->
	case cowboy_req:method(Req) of
		<<"GET">> -> {true, Req, State};
		<<"POST">> -> {true, Req, State}
	end.

to_html(Req, State) ->
	{<<"<html><body>This is REST!</body></html>">>, Req, State}.

% to_json(Req, State) ->
% 	Data = [{<<"message">>, <<"This is REST!">>}],
% 	{ok, Json} = jsx:encode(Data),
% 	{ok, Json, Req, State}.

% register_from_json(Req, State) ->
% 	{ok, Body, Req2} = cowboy_req:body(Req),
% 	{ok, Json} = jsx:decode(Body),
% 	Name = proplists:get_value(<<"name">>, Json),
% 	Age = proplists:get_value(<<"age">>, Json),
% 	Response = jsx:encode([{<<"name">>, Name}, {<<"age">>, Age}]),
% 	{ok, Response, Req2, State}.

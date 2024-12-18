-module(hello_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req, State) ->
	Req = cowboy_req:reply(200,
		#{<<"content-type">> => <<"text/plain">>}, 
		<<"Hello world! this is my first http erlang request">>,
		Req),
	{ok, Req, State}.

-module(erlang_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
				{"/", hello_handler, []},
				{"/json", json_handler, []},
				{"/organization", organization_handler, []}
			]
		}
	]),

	% Non-secure http listener
	{ok, _} = cowboy:start_clear(my_http_listener,
			[{port, 8080}],
			#{
				env => #{dispatch => Dispatch},
				middlewares => [cowboy_router, cowboy_handler]
			}
		),
	erlang_sup:start_link().

stop(_State) ->
	ok = cowboy:stop_listener(my_http_listener).

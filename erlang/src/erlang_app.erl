-module(erlang_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	{ok, _} = cowboy:start_clear(my_http_listener,
			[{port, 8080}],
			#{
				env => #{dispatch => dispatch()},
				middlewares => [cowboy_router, cowboy_handler]
			}
		),
	erlang_sup:start_link().

stop(_State) ->
	ok = cowboy:stop_listener(my_http_listener).

dispatch() -> 
	cowboy_router:compile([
		{'_', [
				{"/json", hello_world, []},
				{"/organization", organization_handler, []},
				{"/organization/all", organization_handler, []},
				{"/organization/:id", organization_handler, []},
				{"/role", role_handler, []},
				{"/role/all", role_handler, []},
				{"/role/:id", role_handler, []},
				{"/event", event_handler, []},
				{"/event/all", event_handler, []},
				{"/event/:id", event_handler, []}
			]
		}
	]).
-module(erlang_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	% io:format("BeforeSeedIndexHandler~n"),
	% index_seeder:create_all_indexes(),
	% io:format("Application started successfully with indexes."),
	% io:format("AfterSeedIndexHandler~n"),

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
				{"/organizations", organization_handler, []},
				{"/organizations/all", organization_handler, []},
				{"/organizations/:id", organization_handler, []},
				{"/roles", role_handler, []},
				{"/roles/all", role_handler, []},
				{"/roles/:id", role_handler, []},
				{"/events", event_handler, []},
				{"/events/all", event_handler, []},
				{"/events/:id", event_handler, []},
				{"/users", user_handler, []},
				{"/users/all", user_handler, []},
				{"/users/:id", user_handler, []},
				{"/users-events", user_event_handler, []}
			]
		}
	]).
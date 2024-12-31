-module(organization_handler).
-behavior(cowboy_rest).

-export([init/2]).
-export([allowed_methods/2, content_types_accepted/2, content_types_provided/2]).
-export([handle_json/2, delete_resource/2]).

% -record(organization, {
% 	id :: string(),
% 	name :: string()
% }).

init(Req, _State) ->
	Path = binary_to_list(cowboy_req:path(Req)),
	case string:split(Path, "/", all) of
        ["", "organization", "all"] -> {cowboy_rest, Req, #{operation => list}};
        ["", "organization", Id] -> {cowboy_rest, Req, #{operation => single, id => Id}};
        ["", "organization"] -> {cowboy_rest, Req, #{operation => single}};
        _ -> {cowboy_rest, Req, #{operation => unknown}}
    end.

allowed_methods(Req, State) ->
	io:format("allow_methods hit: ~p~n", [State]),
	case maps:get(operation, State) of
		list -> {[<<"GET">>], Req, State};
		single ->
			io:format("single method ~n"),
			{[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}
	end.

content_types_accepted(Req, State) ->
	io:format("accepted hit: ~n"),
	{[{{<<"application">>, <<"json">>, '*'}, handle_json}], Req, State}.

content_types_provided(Req, State) ->
	io:format("provided hit: ~n"),
	{[{{<<"application">>, <<"json">>, '*'}, handle_json}], Req, State}.

handle_json(Req, State) ->
	io:format("method: ~p~n", [cowboy_req:method(Req)]),
	case cowboy_req:method(Req) of
		<<"GET">> ->
			case maps:get(operation, State) of
				single -> 
					Id = maps:get(id, State),
					{handle_get_by_id(Id), Req, State};
				list ->
					{handle_get_all(), Req, State}
			end;
		<<"POST">> ->
			handle_create_user(Req, State);
		<<"PUT">> ->
			handle_update_user(Req, State);
		<<"DELETE">> ->
			delete_resource(Req, State)
	end.

handle_get_all() ->
	redis_handler:list_all("org").

handle_get_by_id(Id) ->
	redis_handler:read("org", Id).

handle_create_user(Req, State) ->
	{ok, Body, Req1} = cowboy_req:read_body(Req),
	BinaryBody = jsx:decode(Body),
	Id = maps:get(<<"id">>, BinaryBody),

	case redis_handler:create("org", Id, jsx:encode(BinaryBody)) of
	  {ok, User} ->
		io:format("created user: ~p~n", [User]),
		Req2 = cowboy_req:set_resp_body(User, Req1),
		{true, Req2, State};
	_ ->
		{<<"{\"error\": \"Failed to create user\"}">>, Req1, State}
	end.

handle_update_user(Req, State) ->
	{ok, Body, Req1} = cowboy_req:read_body(Req),
	BinaryBody = jsx:decode(Body),
	Id = maps:get(<<"id">>, BinaryBody),
	io:format("update user: ~p~n", [BinaryBody]),
	case redis_handler:update("org", Id, jsx:encode(BinaryBody)) of
		{ok, User} ->
			Req2 = cowboy_req:set_resp_body(User, Req1),
			{true, Req2, State};
		_ ->	
			{<<"{\"error\": \"Failed to update user\"}">>, Req1, State}
	end.

delete_resource(Req, State) ->
	Id = maps:get(id, State),
	io:format("delete user: ~p~n", [Id]),
	redis_handler:delete("org", Id),
	{<<"{\"status\": \"ok\"}">>, Req, State}.

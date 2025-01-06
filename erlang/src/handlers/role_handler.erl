-module(role_handler).
-behavior(cowboy_rest).

-export([init/2]).
-export([allowed_methods/2, content_types_accepted/2, content_types_provided/2]).
-export([handle_json/2, delete_resource/2]).


init(Req, _State) ->
	Path = binary_to_list(cowboy_req:path(Req)),
	case string:split(Path, "/", all) of
        ["", "role", "all"] -> {cowboy_rest, Req, #{operation => list}};
        ["", "role", Id] -> {cowboy_rest, Req, #{operation => single, id => Id}};
        ["", "role"] -> {cowboy_rest, Req, #{operation => single}};
        _ -> {cowboy_rest, Req, #{operation => unknown}}
    end.

allowed_methods(Req, State) ->
	case maps:get(operation, State) of
		list -> {[<<"GET">>], Req, State};
		single ->
			io:format("single method ~n"),
			{[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}
	end.
	
content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"json">>, '*'}, handle_json}], Req, State}.

content_types_provided(Req, State) ->
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
			handle_create_role(Req, State);
		<<"PUT">> ->
			handle_update_role(Req, State);
		<<"DELETE">> ->
			delete_resource(Req, State)
	end.

handle_get_all() ->
	redis_handler:list_all("role").

handle_get_by_id(Id) ->
	redis_handler:read("role", Id).

handle_create_role(Req, State) ->
	{ok, Body, Req1} = cowboy_req:read_body(Req),
	BinaryBody = jsx:decode(Body),
	Id = maps:get(<<"id">>, BinaryBody),

	case redis_handler:create("role", Id, jsx:encode(BinaryBody)) of
	  {ok, Role} ->
		io:format("created role: ~p~n", [Role]),
		Req2 = cowboy_req:set_resp_body(Role, Req1),
		{true, Req2, State};
	_ ->
		{<<"{\"error\": \"Failed to create role\"}">>, Req1, State}
	end.

handle_update_role(Req, State) ->
	{ok, Body, Req1} = cowboy_req:read_body(Req),
	BinaryBody = jsx:decode(Body),
	Id = maps:get(<<"id">>, BinaryBody),
	io:format("update role: ~p~n", [BinaryBody]),
	case redis_handler:update("role", Id, jsx:encode(BinaryBody)) of
		{ok, Role} ->
			Req2 = cowboy_req:set_resp_body(Role, Req1),
			{true, Req2, State};
		_ ->	
			{<<"{\"error\": \"Failed to update role\"}">>, Req1, State}
	end.

delete_resource(Req, State) ->
	Id = maps:get(id, State),
	io:format("delete role: ~p~n", [Id]),
	redis_handler:delete("role", Id),
	{<<"{\"status\": \"ok\"}">>, Req, State}.
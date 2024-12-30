-module(organization_handler).
-behavior(cowboy_rest).

-export([init/2]).
-export([allowed_methods/2, content_types_accepted/2, content_types_provided/2]).
-export([handle_json/2]).


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
		single -> {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>], Req, State}
	end.

content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"json">>, '*'}, handle_json}], Req, State}.

content_types_provided(Req, State) ->
	{[{{<<"application">>, <<"json">>, '*'}, handle_json}], Req, State}.

handle_json(Req, State) ->
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
			handle_delete(Req, State)
	end.

handle_get_all() ->
	redis_handler:list_all("org").

handle_get_by_id(Id) ->
	redis_handler:read("org", Id).
	

handle_create_user(Req, State) ->
	% Parse request, create user in Redis
	Id = cowboy_req:binding(Req, id),
	{ok, Body, Req2} = cowboy_req:body(Req),
	case jsx:decode(Body) of
		{ok, UserData} ->
		redis_handler:create("org", Id, UserData),
		{<<"{\"status\": \"ok\"}">>, Req2, State};
		_ ->
		{<<"{\"error\": \"Invalid JSON\"}">>, Req, State}
	end.

handle_update_user(Req, State) ->
	% Update user
	{ok, Body, Req2} = cowboy_req:body(Req),
	case jsx:decode(Body) of
		{ok, UserData} ->
			Id = cowboy_req:binding(Req, id),
			redis_handler:update("org", Id, UserData),
			{<<"{\"status\": \"ok\"}">>, Req2, State};
		_ ->
			{<<"{\"error\": \"Invalid JSON\"}">>, Req, State}
	end.

handle_delete(Req, State) ->
	Id = cowboy_req:binding(Req, id),
	redis_handler:delete("org", Id),
	{<<"{\"status\": \"ok\"}">>, Req, State}.

% to_json(Req, State) ->
% 	io:format("to_json hit: ~p~n", [State]),
% 	case maps:get(operation, State) of
% 		list -> 
% 			{redis_handler:list_all("org"), Req, State};
% 		single -> 
% 			Id = maps:get(id, State),
% 			{redis_handler:read("org", Id), Req, State};
% 		unknown -> 
% 			{<<"{\"error\": \"Unknown operation\"}">>, Req, State}
% 	end.
	
% from_json(Req, State) ->
% 	{ok, Req, State}.

% handle_search(Req, State) ->
%     Id = cowboy_req:binding(id, Req),
%     case role_manager_redis:get_role(binary_to_list(Id)) of
%         {ok, Role} -> {Role, Req, State};
%         {ok, undefined} -> {<<"{\"error\": \"Organization not found\"}">>, Req, State}
%     end.
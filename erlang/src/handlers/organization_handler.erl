-module(organization_handler).
-behavior(cowboy_rest).

-export([init/2]).
-export([allowed_methods/2, content_types_accepted/2, content_types_provided/2]).
-export([resource_exists/2, delete_resource/2, is_conflict/2]).
-export([handle_json/2]).


-include("../records/records.hrl").

%% POST - cowboy callback
init(Req, _State) ->
	Path = binary_to_list(cowboy_req:path(Req)),
	case string:split(Path, "/", all) of
        ["", "organization", "all"] -> {cowboy_rest, Req, #{operation => list}};
        ["", "organization", Id] -> {cowboy_rest, Req, #{operation => single, id => Id}};
        ["", "organization"] -> {cowboy_rest, Req, #{operation => single}};
        _ -> {cowboy_rest, Req, #{operation => unknown}}
    end.

allowed_methods(Req, State) ->
	case maps:get(operation, State) of
		list -> {[<<"GET">>], Req, State};
		single ->
			{[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}
		end.
		
%% handle callbacks distinct by method (get-post)
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
			handle_create(Req, State);
		<<"PUT">> ->
			handle_update(Req, State);
		<<"DELETE">> ->
			delete_resource(Req, State)
	end.

handle_get_all() ->
	redis_handler:read_all("org").

handle_get_by_id(Id) ->
	redis_handler:read("org", Id).

resource_exists(Req, State) ->
	case maps:get(operation, State) of
		single -> 
			case maps:is_key(id, State) of
				false -> {false, Req, State};
				true ->
					% {ok, Body, _Req1} = cowboy_req:read_body(Req),
					% BinaryBody = jsx:decode(Body),
					% Name = maps:get(<<"name">>, BinaryBody),
					Id = maps:get(id, State),
					case redis_handler:exists("org", Id) of
						true -> {true, Req, State};
						false -> {false, Req, State}
					end
			end;
		list -> {false, Req, State}
	end.

handle_create(Req, State) ->
	{ok, Body, Req1} = cowboy_req:read_body(Req),
	BinaryBody = jsx:decode(Body),
	Id = uuid:to_string(uuid:uuid4()),
	BinaryBody1 = maps:put(<<"id">>, Id, BinaryBody),

	Name = binary_to_list(maps:get(<<"name">>, BinaryBody1)),
	Organization = #organization{ id = Id, name = Name},

	case json_validator:validate(Organization, BinaryBody1) of
		true ->
			case redis_handler:create("org", jsx:encode(BinaryBody1)) of
				{ok, OrgCreated} ->
					Req2 = cowboy_req:set_resp_body(OrgCreated, Req1),
					{true, Req2, State}
			end;
		false ->
			Req2 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, "Incorrect Json Model", Req1),
			{true, Req2, State}
	end.

handle_update(Req, State) ->
	{ok, Body, Req1} = cowboy_req:read_body(Req),
	BinaryBody = jsx:decode(Body),
	Id = maps:get(<<"id">>, BinaryBody),
	Name = maps:get(<<"name">>, BinaryBody),

	Organization = #organization{id = Id, name = Name},
	case json_validator:validate(Organization, BinaryBody) of
		true ->
			case redis_handler:update("org", Id, jsx:encode(BinaryBody)) of
				{ok, OrgUpdated} ->
					Req2 = cowboy_req:set_resp_body(OrgUpdated, Req1),
					{true, Req2, State};
				_ ->	
					{<<"{\"error\": \"Failed to update organization\"}">>, Req1, State}
			end;
		false ->
			Req2 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, "Incorrect Json Model", Req1),
			{true, Req2, State}
	end.

is_conflict(Req, State) ->

	Req2 = cowboy_req:reply(409, #{<<"content-type">> => <<"application/json">>}, "Conflict", Req),
	{true, Req2, State}.

delete_resource(Req, State) ->
	Id = maps:get(id, State),
	io:format("delete organization: ~p~n", [Id]),
	{redis_handler:delete("org", Id), Req, State}.

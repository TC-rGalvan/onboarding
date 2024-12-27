-module(organization_handler).
-behavior(cowboy_rest).

-export([init/2]).
-export([content_types_accepted/2, content_types_provided/2]).
-export([allow_methods/2, handle_get_list/2, handle_post/2, handle_get_single/2]).
-export([to_json/2, from_json/2]).


init(Req, _State) ->
	Path = binary_to_list(cowboy_req:path(Req)),
	case string:split(Path, "/", all) of
        ["", "organization", "all"] -> {cowboy_rest, Req, #{operation => list}};
        ["", "organization", Id] -> {cowboy_rest, Req, #{operation => single, id => Id}};
        ["", "organization", "search"] -> {cowboy_rest, Req, #{operation => search}};
        _ -> {cowboy_rest, Req, #{operation => unknown}}
    end.

allow_methods(Req, State) ->
	io:format("allow_methods hit: ~p~n", [State]),
	case maps:get(operation, State) of
		list -> {[<<"GET">>], Req, State};
		single -> {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>], Req, State};
		search -> {[<<"GET">>], Req, State};
		unknown -> {[<<"GET">>], Req, State}
	end.

content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.
content_types_provided(Req, State) ->
	{[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.
to_json(Req, State) ->
	case maps:get(operation, State) of
		list ->
			handle_get_list(Req, State);
		single -> handle_get_single(Req, State);
		unknown -> {<<"{\"error\": \"Unknown operation\"}">>, Req, State}
	end.
	
from_json(Req, State) ->
	{ok, Req, State}.

handle_get_list(Req, State) ->
	{ok, C} = eredis:start_link(),
	{ok, Keys} = eredis:q(C, ["KEYS", "org:*"]),
	Organizations = lists:map(fun(Key) ->
		{ok, Value} = eredis:q(C, ["GET", Key]),
		Value
	end, Keys),

	{Organizations, Req, State}.

handle_get_single(Req, State) ->
	{ok, C} = eredis:start_link(),
	Id = maps:get(id, State),
	{ok, Value} = eredis:q(C, ["GET", "org:" ++ Id]),
	{Value, Req, State}.

% handle_get(Req, State) ->
% 	{ok, Roles} = role_store:find_all(),
% 	RolesJson = jiffy:encode([role_records:to_json(R) || R <- Roles]),
% 	{RolesJson, Req, State}.

% handle_search(Req, State) ->
%     Id = cowboy_req:binding(id, Req),
%     case role_manager_redis:get_role(binary_to_list(Id)) of
%         {ok, Role} -> {Role, Req, State};
%         {ok, undefined} -> {<<"{\"error\": \"Organization not found\"}">>, Req, State}
%     end.


% handle_post(Req, State) ->
% 	{ok, Body, Req2} = cowboy_req:read_body(Req),
% 	Role = role_records:from_json(Body),
% 	role_store:save(Role),
% 	Response = role_records:to_json(Role),
% 	Req3 = cowboy_req:set_resp_body(Response, Req2),
% 	{true, Req3, State}.
handle_post(Req, State) ->
	% {ok, Body, Req2} = cowboy_req:read_body(Req),
	% Organization = organization_records:from_json(Body),
	% {ok, C} = eredis:start_link(),
	% % {ok, <<"OK">>} = eredis:q(C, ["SET", "org:" ++ Organization#organization.id, organization_records:to_json(Organization)]),
	{<<"{\"status\": \"ok\"}">>, Req, State}.
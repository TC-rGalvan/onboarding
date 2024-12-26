-module(organization_handler).
-behavior(cowboy_rest).

-export([init/2]).
-export([content_types_accepted/2, content_types_provided/2]).
-export([allow_methods/2, handle_get/2]).


init(Req, State) ->
	{cowboy_rest, Req, State}.

allow_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"json">>, '*'}, handle_post}], Req, State}.
content_types_provided(Req, State) ->
	{[{{<<"application">>, <<"json">>, '*'}, handle_get}], Req, State}.

handle_get(Req, State) ->
	{ok, C} = eredis:start_link(),
	{ok, Keys} = eredis:q(C, ["KEYS", "org:*"]),
	Organizations = lists:map(fun(Key) ->
		{ok, Value} = eredis:q(C, ["GET", Key]),
		Value
	end, Keys),

	{Organizations, Req, State}.

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


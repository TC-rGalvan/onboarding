-module(user_event_handler).

-behavior(cowboy_rest).

-export([init/2]).
-export([content_types_provided/2, content_types_accepted/2, allowed_methods/2,
         handle_post/2]).

-include("../records/records.hrl").

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

%% handle callbacks distinct by method (get-post)
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, handle_post}], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, handle_post}], Req, State}.

handle_post(Req, State) ->
    io:format("Handling POST request~n"),
	{ok, Body, Req1} = cowboy_req:read_body(Req),
    BinaryBody = jsx:decode(Body),
    BinaryBody1 = redis_handler:put_id(BinaryBody),

	Id = binary_to_list(maps:get(<<"id">>, BinaryBody1)),
	UserId = binary_to_list(maps:get(<<"userId">>, BinaryBody)),
	EventId = binary_to_list(maps:get(<<"eventId">>, BinaryBody)),
	UserEvent = #user_event{userId = UserId, eventId = EventId},

	case user_event_validator:validate(UserEvent, BinaryBody1) of
		{ok, User, Event} ->
			case redis_handler:create("user_event", Id, BinaryBody1) of
				{ok, <<"OK">>} ->
					EventType = maps:get(<<"type">>, Event),
					UserName = maps:get(<<"first_name">>, User),
					UserLastName = maps:get(<<"last_name">>, User),
					UserGender = maps:get(<<"gender">>, User),
					Mrn = maps:get(<<"mrn">>, User),
					% OrganizationName = binary_to_list(maps:get(<<"organization">>, User)),
					% RoleName = binary_to_list(maps:get(<<"role">>, User)),


					% BinaryBody2 = maps:remove(<<"organization">>, BinaryBody1),
					% BinaryBody3 = maps:remove(<<"role">>, BinaryBody2),
					% BinaryBody4 = maps:put(<<"organization">>, list_to_binary(OrganizationName), BinaryBody3),
					% BinaryBody5 = maps:put(<<"role">>, list_to_binary(RoleName), BinaryBody4),
					

					UserJson = #{<<"name">> => UserName,
								 <<"last_name">> => UserLastName,
								 <<"gender">> => UserGender,
								 <<"mrn">> => Mrn
								},

					io:format("UserJson: ~p~n", [UserJson]),

					ReturnBody = #{<<"event_type">> => EventType,
						<<"data">> => UserJson},

						cowboy_req:reply(201,
						#{<<"content-type">> => <<"application/json">>},
						jsx:encode(ReturnBody),
						Req1)
			end;
		{error, Reason} ->
			Req2 = cowboy_req:reply(400,
				#{<<"content-type">> => <<"application/json">>},
				jsx:encode(#{error => binary_to_list(Reason)}),
				Req1),
			{true, Req2, State}
	end.

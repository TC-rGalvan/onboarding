-module(user_event_validator).
-export([validate/2]).

-include("../records/records.hrl").

validate(#user_event{userId = UserId, eventId = EventId}, BinarBody) ->
    case json_validator:validate(#user_event{userId = UserId, eventId = EventId}, BinarBody) of
        true ->
            case validate_user(UserId) of
                {ok, User} ->
                    case validate_event(EventId) of
                        {ok, Event} ->
                            {ok, User, Event};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            {error, "Incorrect JSon Model"}
    end.

validate_user(UserId) ->
    User = redis_handler:read("user", UserId),
    case User =:= undefined of
        false ->
            {ok, jsx:decode(User)};
        true  ->
            {error, "User does not exist."}
    end.

validate_event(EventId) ->
    Event = redis_handler:read("event", EventId),
    case Event =:= undefined of
        false ->
            {ok, jsx:decode(Event)};
        true  ->
            {error, "Event does not exist."}
    end.
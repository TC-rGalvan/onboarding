-module(json_validator).

-export([validate/2]).

-include("../records/records.hrl").

validate_fields([], _, _) ->
    true;
validate_fields([Field | Rest], Record, Json) ->
    FieldName = atom_to_binary(Field, utf8),
    case maps:find(FieldName, Json) of
        error ->
            false;
        {ok, _Value} ->
            validate_fields(Rest, Record, Json)
    end.

validate(Record, BinaryBody) ->
    case Record of
        #organization{} ->
            validate_organization(Record, BinaryBody);
        #event{} ->
            validate_event(Record, BinaryBody);
        #user{} ->
            validate_user(Record, BinaryBody);
        #role{} ->
            validate_role(Record, BinaryBody);
        _ ->
            false
    end.

validate_organization(Record, BinaryBody) ->
    BinaryFields = maps:keys(BinaryBody),
    Fields = [binary_to_atom(Field, utf8) || Field <- BinaryFields],
    RecordFields = record_info(fields, organization),
    case lists:usort(RecordFields) =:= lists:usort(Fields) of
        true ->
            validate_fields(RecordFields, Record, BinaryBody);
        false ->
            false
    end.

validate_event(Record, BinaryBody) ->
    BinaryFields = maps:keys(BinaryBody),
    Fields = [binary_to_atom(Field, utf8) || Field <- BinaryFields],
    RecordFields = record_info(fields, event),

    case lists:usort(RecordFields) =:= lists:usort(Fields) of
        true ->
            validate_fields(RecordFields, Record, BinaryBody);
        false ->
            false
    end.

validate_user(Record, BinaryBody) ->
    BinaryFields = maps:keys(BinaryBody),
    Fields = [binary_to_atom(Field, utf8) || Field <- BinaryFields],
    RecordFields = record_info(fields, user),

    case lists:usort(RecordFields) =:= lists:usort(Fields) of
        true ->
            validate_fields(RecordFields, Record, BinaryBody);
        false ->
            false
    end.

validate_role(Record, BinaryBody) ->
    BinaryFields = maps:keys(BinaryBody),
    Fields = [binary_to_atom(Field, utf8) || Field <- BinaryFields],
    RecordFields = record_info(fields, role),

    case lists:usort(RecordFields) =:= lists:usort(Fields) of
        true ->
            validate_fields(RecordFields, Record, BinaryBody);
        false ->
            false
    end.

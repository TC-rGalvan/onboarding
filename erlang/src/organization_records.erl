-module(organization_records).
-export([new_organization/2, to_json/1, from_json/1]).

-record(organization, {
	id :: string(),
	name :: string()
}).

new_organization(Id, Name) ->
    #organization{id=Id, name=Name}.

to_json(#organization{id = Id, name = Name}) ->
    jsx:encode(#{
        <<"id">> => list_to_binary(Id),
        <<"name">> => list_to_binary(Name)
    }).

from_json(Json) ->
    #{
        <<"id">> := Id,
        <<"name">> := Name
    } = jsx:decode(Json, [return_maps]),

    #organization{id=binary_to_list(Id), name=binary_to_list(Name)}.
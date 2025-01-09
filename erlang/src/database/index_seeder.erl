-module(index_seeder).
-export([create_all_indexes/0]).


generate_fields(Record) ->
    case Record of
        organization -> [ {id, "TEXT"}, {name, "TEXT"} ];
        event        -> [ {id, "TEXT"}, {name, "TEXT"} ];
        role         -> [ {id, "TEXT"}, {name, "TEXT"} ];
        user         -> 
            [ 
                {id, "TEXT"}, 
                {first_name, "TEXT"},
                {last_name, "TEXT"},
                {gender, "TEXT"},
                {mrn, "TEXT"},
                {organization, "TEXT"}
            ];
        _ -> []
    end.

%% Function to create the index for a given record type.
create_index(RecordType) ->
    io:format("create_index hit: ~p~n", [RecordType]),
    Fields = generate_fields(RecordType),
    io:format("Fileds: ~p~n", [Fields]),
    IndexName = atom_to_list(RecordType),
    io:format("IndexName: ~p~n", [IndexName]),
    IndexNameWithPrefix = string:join([IndexName, "index"], "_"),
    io:format("IndexNameWithPrefix: ~p~n", [IndexNameWithPrefix]),
    % IndexDefinition = lists:flatten([["FT.CREATE ", IndexNameWithPrefix, " SCHEMA"] | lists:map(fun({Field, Type}) -> [" ", Field, " ", Type] end, Fields)]),
    % io:format("IndexDefinition: ~p~n", [IndexDefinition]),
    Schema = construct_schema(RecordType),
    IndexDef = string:join([IndexNameWithPrefix,"ON", "JSON", "PREFIX", "1", IndexName ++ ":"], " "),
    io:format("IndexDef: ~p~n", [IndexDef]),
    RedisArgs = string:join(["FT.CREATE", IndexDef, "SCHEMA" | Schema ], " "),
    io:format("RedisArgs: ~p~n", [RedisArgs]),
    {ok, C} = eredis:start_link(),
    case eredis:q(C, [RedisArgs]) of
        {ok, _} -> 
            io:format("Index created successfully for ~p", [RecordType]),
            ok;
        {error, Reason} -> 
            io:format("Failed to create index for ~p. Reason: ~p", [RecordType, Reason]),
            {error, Reason}
    end.

%% Entry point function to create all indexes.
create_all_indexes() ->
    ok = create_index(organization),
    ok = create_index(event),
    ok = create_index(user),
    ok = create_index(role),
    io:format("All indexes have been created.").
% construct_schema([]) ->
%     [];
% construct_schema([{FieldName, FieldType} | Rest]) ->
%     string:join(["$.", FieldName, "AS", FieldType, construct_schema(Rest)], " ").

construct_schema(RecordType) ->
    %% Get the fields for the record
    Fields = generate_fields(RecordType),
    io:format("Construct: ~p~n", [Fields]),
    %% Construct the schema part for the FT.CREATE command
    SchemaParts = lists:map(fun({Field, Type}) -> 
        io:format("JsonMAtch: ~n"),
        JsonMatch = string:join(["$", atom_to_list(Field)], "."),
        io:format("~p~n", [JsonMatch]),
        FieldAndType = string:join([JsonMatch, "AS", atom_to_list(Field), Type], " "),
        FieldAndType
    end, Fields),

    io:format("SchemaPArts ~p~n", [SchemaParts]),

    SchemaParts.



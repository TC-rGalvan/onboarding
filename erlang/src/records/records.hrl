-record(organization, {
	id :: integer(),
	name :: string()
}).

-record(event, {
    id :: integer(),
    name :: string()
}).

-record(user, {
    id :: integer(),
    first_name :: string(),
    last_name :: string(),
    gender :: string(),
    mrn :: string(),
    organization :: string()
}).

-record(role, {
    id :: integer(),
    name :: string()
}).


-record(organization, {
	id :: string(),
	name :: string()
}).

-record(event, {
    id :: string(),
type :: string()
}).

-record(user, {
    id :: string(),
    first_name :: string(),
    last_name :: string(),
    gender :: string(),
    mrn :: string(),
    organization :: string(),
    role :: string()
}).

-record(role, {
    id :: string(),
    name :: string()
}).

-record(user_event, {
    userId :: string(),
    eventId :: string()
}).


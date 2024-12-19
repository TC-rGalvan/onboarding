{application, 'erlang', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['erlang_app','erlang_sup','hello_handler','json_handler']},
	{registered, [erlang_sup]},
	{applications, [kernel,stdlib,cowboy]},
	{optional_applications, []},
	{mod, {erlang_app, []}},
	{env, []}
]}.
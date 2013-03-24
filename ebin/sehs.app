%%%-------------------------------------------------------------------
%%% Created :  4 Nov 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

{application, sehs, [
	{id, "sehs"},
	{description, "Simple Erlang based HTTP server."},
	{vsn, "0.0.1"},
	{modules, [
		sehs,
		sehs_app,
		sehs_sup,
		sehs_server,
		sehs_handler,
		sehs_reader
	]},
	{registered, [sehs_server]},
	{applications, [kernel, stdlib, sasl, utils]},
	{mod, {sehs_app, []}}
]}.

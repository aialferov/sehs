%%%-------------------------------------------------------------------
%%% Created : 29 Jun 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

{application, sehs_usage, [
	{id, "sehs_usage"},
	{description, "Sehs usage example"},
	{vsn, "0.0.1"},
	{modules, [
		sehs,
		sehs_app,
		sehs_sup,
		sehs_usage,
		sehs_reader,
		sehs_server,
		sehs_handler
	]},
	{registered, [sehs_server]},
	{applications, [kernel, stdlib, sasl, utils]},
	{mod, {sehs_app, []}}
]}.

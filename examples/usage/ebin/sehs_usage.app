%%%-------------------------------------------------------------------
%%% Created : 29 Jun 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

{application, sehs_usage, [
	{id, "sehs_usage"},
	{description, "Sehs usage example"},
	{vsn, "0.0.1"},
	{modules, [sehs_usage]},
	{registered, [sehs_usage]},
	{applications, [kernel, stdlib, sasl, sehs]},
	{mod, {sehs_app, [
		{name, sehs_usage},
		{handlers, [
			{request_handler, sehs_usage, handle_request},
			{log_handler, sehs_usage, set_log_file, log_report}
		]}
	]}}
]}.

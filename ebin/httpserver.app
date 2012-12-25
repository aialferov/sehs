%%%-------------------------------------------------------------------
%%% Created :  4 Nov 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

{application, httpserver, [
	{id, "websbsn"},
	{description, "SBSN Web Server"},
	{vsn, "0.0.1"},
	{modules, [
		httpserver,
		http_server_app,
		http_server_sup,
		http_server,
		http_handler,
		http_reader
	]},
	{registered, [http_server]},
	{applications, [kernel, stdlib, sasl]},
	{mod, {http_server_app, []}}
]}.

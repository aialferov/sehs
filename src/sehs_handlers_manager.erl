%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 29 Jun 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(sehs_handlers_manager).

-export([read_handlers/1, update_handlers/2]).
-export([handle_request/3]).
-export([set_log_file/2, log_report/2]).

-record(handlers, {request, log}).
-record(request_handler, {module, handle}).
-record(log_handler, {module, set_file, report}).

read_handlers(Handlers) -> #handlers{
	request = lists:keyfind(request_handler, 1, Handlers),
	log = lists:keyfind(log_handler, 1, Handlers)
}.
update_handlers(Rh = #request_handler{}, H = _Handlers) ->
	H#handlers{request = Rh};
update_handlers(Lh = #log_handler{}, H = _Handlers) -> H#handlers{log = Lh}.

handle_request(Rid, Request, #handlers{request = R}) ->
	(R#request_handler.module):(R#request_handler.handle)(Rid, Request).

set_log_file(File, #handlers{log = L}) ->
	(L#log_handler.module):(L#log_handler.set_file)(File).

log_report(Info, #handlers{log = L}) ->
	(L#log_handler.module):(L#log_handler.report)(Info).

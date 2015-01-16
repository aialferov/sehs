%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created : 16 Oct 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(sehs_handler).
-export([accept/3]).

-include("sehs_logs.hrl").

-define(SP, " ").
-define(CRLF, "\r\n").

-define(HttpVersion, "HTTP/1.0").
-define(ResponseHeaders,
	"Server: " ++ utils_app:get_key(id) ++
	"/" ++ utils_app:get_key(vsn) ++ ?CRLF
).
-define(Status(Code), case Code of
	ok                              -> "200 OK";
	created                         -> "201 Created";
	accepted                        -> "202 Accepted";
	non_authoritative_information   -> "203 Non-Authoritative Information";
	no_content                      -> "204 No Content";
	reset_content                   -> "205 Reset Content";
	partical_content                -> "206 Partial Content";

	multiple_choices                -> "300 Multiple Choices";
	moved_permanently               -> "301 Moved Permanently";
	found                           -> "302 Found";
	see_other                       -> "303 See Other";
	not_modified                    -> "304 Not Modified";
	use_proxy                       -> "305 Use Proxy";
	% (unused)                      -> "306 (Unused)";
	temporary_redirect              -> "307 Temporary Redirect";

	bad_request                     -> "400 Bad Request";
	unauthorized                    -> "401 Unauthorized";
	payment_required                -> "402 Payment Required";
	forbidden                       -> "403 Forbidden";
	not_found                       -> "404 Not Found";
	method_not_allowed              -> "405 Method Not Allowed";
	not_acceptable                  -> "406 Not Acceptable";
	proxy_authentication_required   -> "407 Proxy Authentication Required";
	request_timeout                 -> "408 Request Timeout";
	conflict                        -> "409 Conflict";
	gone                            -> "410 Gone";
	length_required                 -> "411 Length Required";
	precondition_failed             -> "412 Precondition Failed";
	request_entity_too_large        -> "413 Request Entity Too Large";
	request_uri_too_long            -> "414 Request-URI Too Long";
	unsupported_media_type          -> "415 Unsupported Media Type";
	requested_range_not_satisfiable -> "416 Requested Range Not Satisfiable";
	expectation_failed              -> "417 Expectation Failed";

	internal_server_error           -> "500 Internal Server Error";
	not_implemented                 -> "501 Not Implemented";
	bad_gateway                     -> "502 Bad Gateway";
	service_unavailable             -> "503 Service Unavailable";
	gateway_timeout                 -> "504 Gateway Timeout";
	http_version_not_supported      -> "505 HTTP Version Not Supported"
end).

-define(CrashResponse, {internal_server_error, {[], []}}).

accept(HttpServer, Handlers, LSocket) ->
	case gen_tcp:accept(LSocket) of
		{ok, Socket} ->
			gen_server:cast(HttpServer, accept),
			wait_data(Handlers, Socket);
		{error, closed} -> log(?ClosedAtAcceptLog, Handlers)
	end.

wait_data(Handlers, Socket) -> receive
	{set_handlers, NewHandlers} -> wait_data(NewHandlers, Socket);
	{tcp, Socket, Data} ->
		log(?RequestLog(peername(Socket), Data), Handlers),
		handle_result(Handlers, Socket, request, sehs_reader:read(
			Data, {fun wait_more_data/1, {Handlers, Socket}})),
		ok = gen_tcp:close(Socket);
	{tcp_closed, Socket} -> log(?ClosedAtReceiveLog, Handlers);
	{tcp_error, Socket, Reason} -> log(?TcpErrorLog(Reason), Handlers)
end.

wait_more_data({Handlers, Socket}) -> receive
	{tcp, Socket, MoreData} ->
		log(?MoreDataLog(MoreData), Handlers),
		{ok, MoreData};
	{tcp_closed, Socket} -> {error, no_more_data};
	{tcp_error, Socket, _Reason} -> {error, no_more_data}
end.

handle_result(Handlers, Socket, request, {ok, Request}) ->
	handle_result(Handlers, Socket, response,
		try handle_request(Request, Handlers) of Response -> {ok, Response}
		catch _:Reason -> {{error, Reason}, ?CrashResponse} end
	);

handle_result(Handlers, Socket, request, {error, Reason}) ->
	respond(Handlers, Socket, response({Reason, {[], []}}));

handle_result(Handlers, Socket, response, {Result, Response}) ->
	respond(Handlers, Socket, response(Response)), result(Result).

response({StatusCode, {Headers, MessageBody}}) ->
	?HttpVersion ++ ?SP ++ ?Status(StatusCode) ++ ?CRLF ++
	?ResponseHeaders ++ Headers ++ ?CRLF ++ MessageBody.

respond(Handlers, Socket, Response) ->
	log(?ResponseLog(Response), Handlers),
	gen_tcp:send(Socket, Response).

result(ok) -> ok;
result({error, Reason}) -> exit({Reason, erlang:get_stacktrace()}).

log(Report, Handlers) -> sehs_handlers_manager:log_report(Report, Handlers).
handle_request(Request, Handlers) ->
	sehs_handlers_manager:handle_request(self(), Request, Handlers).

peername(Socket) -> read_peername(inet:peername(Socket)).
read_peername({ok, {Address, Port}}) ->
	inet_parse:ntoa(Address) ++ ":" ++ integer_to_list(Port);
read_peername({error, _Reason}) -> [].

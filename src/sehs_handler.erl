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
	ok -> "200 OK";
	bad_request -> "400 Bad Request";
	not_found -> "404 Not Found";
	method_not_allowed -> "405 Method Not Allowed";
	internal_server_error -> "500 Internal Server Error";
	service_unavailable -> "503 Service Unavailable"
end).

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
		log(?RequestLog(Data), Handlers),
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
		sehs_handlers_manager:handle_request(Request, Handlers));

handle_result(Handlers, Socket, request, {error, Reason}) ->
	respond(Handlers, Socket, response({Reason, [], []}));

handle_result(Handlers, Socket, response, Response) ->
	respond(Handlers, Socket, response(Response)).

response({StatusCode, Headers, MessageBody}) ->
	?HttpVersion ++ ?SP ++ ?Status(StatusCode) ++ ?CRLF ++
	?ResponseHeaders ++ Headers ++ ?CRLF ++
	case MessageBody of [] -> []; MessageBody -> MessageBody ++ ?CRLF end.

respond(Handlers, Socket, Response) ->
	log(?ResponseLog(Response), Handlers),
	gen_tcp:send(Socket, Response).

log(Text, Handlers) -> sehs_handlers_manager:log_report(Text, Handlers).

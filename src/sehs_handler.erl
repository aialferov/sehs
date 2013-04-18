%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created : 16 Oct 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(sehs_handler).
-export([accept/4]).

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

accept(HttpServer, RequestHandler, LogHandler, LSocket) ->
	case gen_tcp:accept(LSocket) of
		{ok, Socket} ->
			gen_server:cast(HttpServer, accept),
			wait_data(RequestHandler, LogHandler, Socket);
		{error, closed} -> io:format("TCP closed at accept~n", [])
	end.

wait_data(RequestHandler, LogHandler = {Logger, Report}, Socket) -> receive
	{set_log_handler, NewLogHandler} ->
		wait_data(RequestHandler, NewLogHandler, Socket);
	{set_request_handler, NewRequestHandler} ->
		wait_data(NewRequestHandler, LogHandler, Socket);
	{tcp, Socket, Data} ->
		Logger:Report(?RequestLog(Data)),
		WaitMoreDataFun = {fun wait_more_data/1, {LogHandler, Socket}},
		handle_result(RequestHandler, LogHandler, Socket,
			request, sehs_reader:read(Data, WaitMoreDataFun)),
		ok = gen_tcp:close(Socket);
	{tcp_closed, Socket} -> io:format("TCP closed at receive~n", []);
	{tcp_error, Socket, Reason} -> io:format("TCP error: ~p~n", [Reason])
end.

wait_more_data({{Logger, Report}, Socket}) -> receive
	{tcp, Socket, MoreData} ->
		Logger:Report(?MoreDataLog(MoreData)), {ok, MoreData};
	{tcp_closed, Socket} -> {error, no_more_data};
	{tcp_error, Socket, _Reason} -> {error, no_more_data}
end.

handle_result(
	{RequestHandler, Handle}, LogHandler,
	Socket, request, {ok, Request}
) ->
	handle_result(ok, LogHandler, Socket,
		response, RequestHandler:Handle(Request));

handle_result(_RequestHandler, LogHandler, Socket, request, {error, Reason}) ->
	respond(LogHandler, Socket, response({Reason, [], []}));

handle_result(_RequestHandler, LogHandler, Socket, response, Response) ->
	respond(LogHandler, Socket, response(Response)).

response({StatusCode, Headers, MessageBody}) ->
	?HttpVersion ++ ?SP ++ ?Status(StatusCode) ++ ?CRLF ++
	?ResponseHeaders ++ Headers ++ ?CRLF ++
	case MessageBody of [] -> []; MessageBody -> MessageBody ++ ?CRLF end.

respond({Logger, Report}, Socket, Response) ->
	Logger:Report(?ResponseLog(Response)),
	gen_tcp:send(Socket, Response).

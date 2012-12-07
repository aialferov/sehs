%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created : 16 Oct 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(http_handler).
-export([accept/4]).

-include("http_server_logs.hrl").

-define(Server, "Server: " ++ app_key(id) ++ "/" ++ app_key(vsn) ++ "\r\n").

-define(HttpOK(Response),
	"HTTP/1.0 200 OK\r\n" ++ ?Server ++
	"Content-Type: text/plain; charset=\"utf-8\"\r\n" ++
	"\r\n" ++ Response ++ "\r\n"
).
-define(HttpError(Reason), case Reason of
	bad_request -> ?HttpBadRequest;
	not_found -> ?HttpNotFound;
	method_not_allowed -> ?HttpMethodNotAllowed;
	internal_server_error -> ?HttpInternalServerError;
	service_unavailable -> ?HttpServiceUnavailable
end).
-define(HttpBadRequest,
	"HTTP/1.0 400 Bad Request\r\n" ++ ?Server ++ "\r\n").
-define(HttpNotFound,
	"HTTP/1.0 404 Not Found\r\n" ++ ?Server ++ "\r\n").
-define(HttpMethodNotAllowed,
	"HTTP/1.0 405 Method Not Allowed\r\n" ++ ?Server ++
	"Allow: GET, POST\r\n\r\n"
).
-define(HttpInternalServerError,
	"HTTP/1.0 500 Internal Server Error\r\n" ++ ?Server ++ "\r\n").
-define(HttpServiceUnavailable,
	"HTTP/1.0 503 Service Unavailable\r\n" ++ ?Server ++ "\r\n").

accept(HttpServer, RequestHandler, LogHandler, LSocket) ->
	case gen_tcp:accept(LSocket) of
		{ok, Socket} ->
			gen_server:cast(HttpServer, accept),
			wait_data(RequestHandler, LogHandler, Socket);
		{error, closed} -> io:format("TCP closed~n", [])
	end.

wait_data(RequestHandler, LogHandler = {Logger, Report}, Socket) -> receive
	{request_handler, NewRequestHandler} ->
		wait_data(NewRequestHandler, LogHandler, Socket);
	{tcp, Socket, Data} ->
		Logger:Report(?RequestLog(Data)),
		WaitMoreDataFun = {fun wait_more_data/1, {LogHandler, Socket}},
		handle_result(RequestHandler, LogHandler, Socket,
			request, http_reader:read(Data, WaitMoreDataFun)),
		ok = gen_tcp:close(Socket);
	{tcp_closed, Socket} -> io:format("TCP closed~n", []);
	{tcp_error, Socket, Reason} -> io:format("TCP error: ~p~n", [Reason])
end.

wait_more_data({{Logger, Report}, Socket}) -> receive
	{tcp, Socket, MoreData} ->
		Logger:Report(?MoreDataLog(MoreData)), {ok, MoreData};
	{tcp_closed, Socket} -> {error, no_more_data};
	{tcp_error, Socket, _Reason} -> {error, no_more_data}
end.

handle_result({RequestHandler, Handle},
	LogHandler, Socket, request, {ok, Request})
->
	handle_result(ok, LogHandler, Socket,
		response, RequestHandler:Handle(Request));
handle_result(_, LogHandler, Socket, response, {ok, Response}) ->
	respond(LogHandler, Socket, ?HttpOK(Response));
handle_result(_, LogHandler, Socket, _, {error, Reason}) ->
	respond(LogHandler, Socket, ?HttpError(Reason)).

respond({Logger, Report}, Socket, Response) ->
	Logger:Report(?ResponseLog(Response)),
	gen_tcp:send(Socket, Response).

app_key(Key) -> app_key(application:get_application(), {key, Key}).
app_key({ok, Application}, {key, Key}) ->
	app_key(application:get_key(Application, Key), ok);
app_key({ok, Value}, ok) -> Value;
app_key(undefined, _) -> [].

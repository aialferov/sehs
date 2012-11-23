%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created : 16 Oct 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(http_handler).
-export([accept/3]).

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

accept(HttpServer, RequestHandler, LSocket) ->
	case gen_tcp:accept(LSocket) of
		{ok, Socket} ->
			gen_server:cast(HttpServer, accept),
			wait_data(RequestHandler, Socket);
		{error, closed} -> io:format("TCP closed~n", [])
	end.

wait_data(RequestHandler, Socket) -> receive
	{request_handler, NewRequestHandler} ->
		wait_data(NewRequestHandler, Socket);
	{tcp, Socket, Data} ->
		handle_result(RequestHandler, Socket, request,
			http_reader:read(Data, {fun wait_more_data/1, Socket})),
		ok = gen_tcp:close(Socket);
	{tcp_closed, Socket} -> io:format("TCP closed~n", []);
	{tcp_error, Socket, Reason} -> io:format("TCP error: ~p~n", [Reason])
end.

wait_more_data(Socket) -> receive
	{tcp, Socket, MoreData} -> {ok, MoreData};
	{tcp_closed, Socket} -> {error, no_more_data};
	{tcp_error, Socket, _Reason} -> {error, no_more_data}
end.

handle_result(RequestHandler = {M, F}, Socket, request, {ok, Request}) ->
	handle_result(RequestHandler, Socket, response, M:F(Request));
handle_result(_RequestHandler, Socket, response, {ok, Response}) ->
	gen_tcp:send(Socket, ?HttpOK(Response));
handle_result(_RequestHandler, Socket, _Result, {error, Reason}) ->
	gen_tcp:send(Socket, ?HttpError(Reason)).

app_key(Key) -> case application:get_application() of
	{ok, Application} ->
		case application:get_key(Application, Key) of
			{ok, Value} -> Value; undefined -> [] end;
	undefined -> []
end.

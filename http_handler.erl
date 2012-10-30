%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created : 16 Oct 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(http_handler).
-export([accept/3]).

-define(HttpOK(Response),
	"HTTP/1.0 200 OK\r\n" ++
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
	"HTTP/1.0 400 Bad Request\r\n\r\n").
-define(HttpNotFound,
	"HTTP/1.0 404 Not Found\r\n\r\n").
-define(HttpMethodNotAllowed,
	"HTTP/1.0 405 Method Not Allowed\r\n" ++
	"Allow: GET, POST\r\n\r\n"
).
-define(HttpInternalServerError,
	"HTTP/1.0 500 Internal Server Error\r\n\r\n").
-define(HttpServiceUnavailable,
	"HTTP/1.0 503 Service Unavailable\r\n\r\n").

accept(HttpServer, RequestHandler, LSocket) ->
	{ok, Socket} = gen_tcp:accept(LSocket),
	gen_server:cast(HttpServer, accept),
	wait_data(RequestHandler, Socket).

wait_data(RequestHandler, Socket) -> receive
	{tcp, Socket, Data} ->
%		io:format("Data: ~p~n", [Data]),
%		io:format("Read: ~p~n", [http_reader:read(
%			Data, {fun wait_more_data/1, Socket})]),
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

handle_result(RequestHandler, Socket, request, {ok, Request}) ->
	handle_result(RequestHandler, Socket, response,
		RequestHandler:handle_request(Request));
handle_result(_RequestHandler, Socket, response, {ok, Response}) ->
	gen_tcp:send(Socket, ?HttpOK(Response));
handle_result(_RequestHandler, Socket, _Result, {error, Reason}) ->
	gen_tcp:send(Socket, ?HttpError(Reason)).

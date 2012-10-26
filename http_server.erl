%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created : 16 Oct 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(http_server).
-behaviour(gen_server).

-export([start_link/0, start_link/1]).
-export([request_handler/1]).
-export([listen/1, close/0]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-define(ListenOptions, [{reuseaddr, true}]).

-define(HttpOK(Response),
	"HTTP/1.0 200 OK\r\n" ++
	"Content-Type: text/plain; charset=\"utf-8\"\r\n" ++
	"\r\n" ++ Response ++ "\r\n"
).
-define(HttpError(Reason), case Reason of
	bad_request -> ?HttpBadRequest;
	method_not_allowed -> ?HttpMethodNotAllowed;
	service_unavailable -> ?HttpServiceUnavailable
end).
-define(HttpBadRequest,
	"HTTP/1.0 400 Bad Request\r\n\r\n").
-define(HttpMethodNotAllowed,
	"HTTP/1.0 405 Method Not Allowed\r\n" ++
	"Allow: GET, POST\r\n\r\n"
).
-define(HttpServiceUnavailable,
	"HTTP/1.0 503 Service Unavailable\r\n\r\n").

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
start_link(Args) -> gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

request_handler(Module) -> gen_server:call(?MODULE, {request_handler, Module}).

listen(Port) -> gen_server:call(?MODULE, {listen, Port}).
close() -> gen_server:call(?MODULE, close).

init([]) -> process_flag(trap_exit, true), {ok, []};
init([{config, FileName}, {request_handler, Module}]) ->
	process_flag(trap_exit, true),
	{ok, Config} = file:consult(FileName),
	{listen, {port, Port}} = lists:keyfind(listen, 1, Config),
	{ok, LSocket} = gen_tcp:listen(Port, ?ListenOptions),
	spawn_accept(Module, LSocket),
	{ok, [{request_handler, Module}, {lsocket, LSocket}]}.

handle_call({request_handler, Module}, _From, [_RequestHandler, LSocket]) ->
	{reply, ok, [{request_handler, Module}, LSocket]};
handle_call({request_handler, Module}, _From, _State) ->
	{reply, ok, [{request_handler, Module}]};

handle_call({listen, _}, _From, []) ->
	{reply, {error, no_request_handler}, []};
handle_call({listen, Port}, _From, State = [{request_handler, Module}]) ->
	case gen_tcp:listen(Port, ?ListenOptions) of
		{ok, LSocket} ->
			spawn_accept(Module, LSocket),
			{reply, ok, [{request_handler, Module}, {lsocket, LSocket}]};
		Error -> {reply, Error, State}
	end;
handle_call({listen, _}, _From,
	State = [{request_handler, _}, {lsocket, _}]) ->
		{reply, {error, already_listening}, State};

handle_call(close, _From, [{request_handler, Module}, {lsocket, LSocket}]) ->
	{reply, gen_tcp:close(LSocket), [{request_handler, Module}]};
handle_call(close, _From, State) -> {reply, {error, not_listening}, State}.

handle_cast(accept, State = [{request_handler, Module}, {lsocket, LSocket}]) ->
	spawn_accept(Module, LSocket), {noreply, State}.

handle_info(Info, State) ->
	io:format("Info: ~p~n", [Info]),
	{noreply, State}.

terminate(Reason, [{request_handler, _}, {lsocket, LSocket}]) ->
	io:format("Terminate: ~p~n", [Reason]),
	gen_tcp:close(LSocket);
terminate(Reason, _State) ->
	io:format("Terminate: ~p~n", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

spawn_accept(RequestHandler, LSocket) ->
	proc_lib:spawn_link(fun() -> accept(RequestHandler, LSocket) end).

accept(RequestHandler, LSocket) ->
	{ok, Socket} = gen_tcp:accept(LSocket),
	gen_server:cast(?MODULE, accept),
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

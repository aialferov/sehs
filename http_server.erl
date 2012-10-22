%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created : 16 Oct 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(http_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([callback/1]).
-export([listen/1, close/0]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-define(ListenOptions, [{reuseaddr, true}, {backlog, 32}]).

-define(HttpOK(Response),
	"HTTP/1.0 200 OK\r\n" ++
	"Content-Type: text/plain; charset=\"utf-8\"\r\n" ++
	"\r\n" ++ Response ++ "\r\n"
).
-define(HttpMethodNotAllowed,
	"HTTP/1.0 405 Method Not Allowed\r\n" ++
	"Allow: GET, POST\r\n\r\n"
).
-define(HttpServiceUnavailable,
	"HTTP/1.0 503 Service Unavailable\r\n\r\n").

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

callback(Module) -> gen_server:call(?MODULE, {callback, Module}).
listen(Port) -> gen_server:call(?MODULE, {listen, Port}).
close() -> gen_server:call(?MODULE, close).

init([]) -> process_flag(trap_exit, true), {ok, []}.

handle_call({callback, Module}, _From, [_OldModule, LSocket]) ->
	{reply, ok, [{module, Module}, LSocket]};
handle_call({callback, Module}, _From, _State) ->
	{reply, ok, [{module, Module}]};

handle_call({listen, _}, _From, []) -> {reply, {error, no_callback}, []};
handle_call({listen, Port}, _From, State = [{module, Module}]) ->
	case gen_tcp:listen(Port, ?ListenOptions) of
		{ok, LSocket} ->
			spawn_accept(Module, LSocket),
			{reply, ok, [{module, Module}, {lsocket, LSocket}]};
		Error -> {reply, Error, State}
	end;
handle_call({listen, _}, _From, State = [{module, _}, {lsocket, _}]) ->
	{reply, {error, already_listening}, State};

handle_call(close, _From, [{module, Module}, {lsocket, LSocket}]) ->
	{reply, gen_tcp:close(LSocket), [{module, Module}]};
handle_call(close, _From, State) -> {reply, {error, not_listening}, State}.

handle_cast(accept, State = [{module, Module}, {lsocket, LSocket}]) ->
	spawn_accept(Module, LSocket), {noreply, State}.

handle_info(Info, State) ->
	io:format("Info: ~p~n", [Info]),
	{noreply, State}.

terminate(Reason, [{module, _}, {lsocket, LSocket}]) ->
	io:format("Terminate: ~p~n", [Reason]),
	gen_tcp:close(LSocket);
terminate(Reason, _State) ->
	io:format("Terminate: ~p~n", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

spawn_accept(Module, LSocket) ->
	proc_lib:spawn_link(fun() -> accept(Module, LSocket) end).

accept(Module, LSocket) ->
	{ok, Socket} = gen_tcp:accept(LSocket),
	gen_server:cast(?MODULE, accept),
	wait_data(Module, Socket, []).

wait_data(Module, Socket, ReadData) -> receive
	{tcp, Socket, MoreData} ->
		Data = ReadData ++ MoreData,
%		io:format("Data: ~p~n", [Data]),
%		io:format("Read: ~p~n", [http_reader:read(Data)]),
		case http_reader:read(Data) of
			{ok, Query} -> handle_query(Module, Socket, Query);
			{error, not_complete} -> wait_data(Module, Socket, Data);
			{error, Reason} -> handle_error(Socket, Reason)
		end;
	{tcp_closed, Socket} -> io:format("TCP closed~n", []);
	{tcp_error, Socket, Reason} -> io:format("TCP error: ~p~n", [Reason])
end.

handle_query(Module, Socket, Query) -> case Module:handle_query(Query) of
	{ok, Response} -> send_response(Socket, ?HttpOK(Response));
	{error, Reason} -> handle_error(Socket, Reason)
end.

handle_error(Socket, method_not_allowed) ->
	send_response(Socket, ?HttpMethodNotAllowed);
handle_error(Socket, service_unavailable) ->
	send_response(Socket, ?HttpServiceUnavailable).

send_response(Socket, Response) ->
	ok = gen_tcp:send(Socket, Response),
	ok = gen_tcp:close(Socket).

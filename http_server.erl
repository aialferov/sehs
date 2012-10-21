%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created : 16 Oct 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(http_server).
-behaviour(gen_server).

-export([start_link/0, start_link/1]).
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

start_link() -> start_link([{module, http_server_example}]).
start_link(Args) -> gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

listen(Port) -> gen_server:call(?MODULE, {listen, Port}).
close() -> gen_server:call(?MODULE, close).

init([{module, Module}]) ->
	process_flag(trap_exit, true),
	{ok, [{module, Module}]}.

handle_call({listen, Port}, _From, State = [{module, Module}]) ->
	case gen_tcp:listen(Port, ?ListenOptions) of
		{ok, LSocket} ->
			spawn_accept(Module, LSocket),
			{reply, ok, [{module, Module}, {socket, LSocket}]};
		Error -> {reply, Error, State}
	end;
handle_call({listen, _}, _From, State = [{module, _}, {socket, _}]) ->
	{reply, {error, already_listening}, State};

handle_call(close, _From, State = [{module, _}]) ->
	{reply, {error, not_listening}, State};
handle_call(close, _From, [{module, Module}, {socket, LSocket}]) ->
	{reply, gen_tcp:close(LSocket), [{module, Module}]}.

handle_cast(accept, State = [{module, Module}, {socket, LSocket}]) ->
	spawn_accept(Module, LSocket), {noreply, State}.

handle_info(Info, State) ->
	io:format("Info: ~p~n", [Info]),
	{noreply, State}.

terminate(Reason, [{module, _}, {socket, LSocket}]) ->
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
	wait_data(Module, Socket).

wait_data(Module, Socket) -> receive
	{tcp, Socket, Data} ->
%		io:format("Data: ~p~n", [Data]),
%		io:format("Read: ~p~n", [http_reader:read(Data)]),
		handle_data(Module, Socket, Data);
	{tcp_closed, Socket} -> io:format("TCP closed~n", []);
	{tcp_error, Socket, Reason} -> io:format("TCP error: ~p~n", [Reason])
end.

handle_data(Module, Socket, Data) -> case http_reader:read(Data) of
	{ok, Query} -> handle_query(Module, Socket, Query);
	{error, Reason} -> handle_error(Socket, Reason)
end.

handle_query(Module, Socket, Query) -> case Module:handle_query(Query) of
	{ok, Response} -> send_response(Socket, ?HttpOK(Response));
	error -> send_response(Socket, ?HttpServiceUnavailable)
end.

handle_error(Socket, method_not_allowed) ->
	send_response(Socket, ?HttpMethodNotAllowed).

send_response(Socket, Response) ->
	gen_tcp:send(Socket, Response), gen_tcp:close(Socket).

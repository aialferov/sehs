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

-define(ListenOptions, [{reuseaddr, true}, {backlog, 5}]).
-define(AcceptsNumber, 16).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
start_link(Args) -> gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

request_handler(MF) -> gen_server:call(?MODULE, {request_handler, MF}).

listen(Port) -> gen_server:call(?MODULE, {listen, Port}).
close() -> gen_server:call(?MODULE, close).

init([]) -> process_flag(trap_exit, true), {ok, []};
init([{config, Config}, {request_handler, RequestHandler}]) ->
	init([]),
	{listen, {port, Port}} = lists:keyfind(listen, 1, Config),
	{ok, LSocket} = gen_tcp:listen(Port, ?ListenOptions),
	{ok, [{request_handler, RequestHandler}, {listen, LSocket,
		spawn_accepts(RequestHandler, LSocket, ?AcceptsNumber)}]};
init([{file, FileName}, RequestHandler = {request_handler, _}]) ->
	{ok, Config} = file:consult(case filename:pathtype(FileName) of
		absolute -> FileName;
		relative -> filename:dirname(code:which(?MODULE)) ++ "/" ++ FileName
	end),
	init([{config, Config}, RequestHandler]).

handle_call(RequestHandler = {request_handler, _}, _From,
	[{request_handler, _}, Listen = {listen, _, Pids}]
) ->
	[Pid ! RequestHandler || Pid <- Pids],
	{reply, ok, [RequestHandler, Listen]};
handle_call(RequestHandler = {request_handler, _}, _From, _State) ->
	{reply, ok, [RequestHandler]};

handle_call({listen, _}, _From, []) ->
	{reply, {error, no_request_handler}, []};
handle_call({listen, Port}, _From,
	State = [{request_handler, RequestHandler}]
) ->
	case gen_tcp:listen(Port, ?ListenOptions) of
		{ok, LSocket} ->
			{reply, ok, [{request_handler, RequestHandler}, {listen, LSocket,
				spawn_accepts(RequestHandler, LSocket, ?AcceptsNumber)}]};
		Error -> {reply, Error, State}
	end;
handle_call({listen, _}, _From,
	State = [{request_handler, _}, {listen, _, _}]) ->
		{reply, {error, already_listening}, State};

handle_call(close, _From,
	[RequestHandler = {request_handler, _}, {listen, LSocket, _}]) ->
		{reply, gen_tcp:close(LSocket), [RequestHandler]};
handle_call(close, _From, State) -> {reply, {error, not_listening}, State}.

handle_cast(accept,
	State = [{request_handler, RequestHandler}, {listen, LSocket, _}]) ->
		spawn_accept(RequestHandler, LSocket), {noreply, State}.

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

spawn_accept(RequestHandler, LSocket) -> proc_lib:spawn_link(
	http_handler, accept, [?MODULE, RequestHandler, LSocket]).

spawn_accepts(RequestHandler, LSocket, AcceptsNumber) ->
	[spawn_accept(RequestHandler, LSocket) || _ <-lists:seq(1, AcceptsNumber)].

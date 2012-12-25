%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created : 16 Oct 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(http_server).
-behaviour(gen_server).

-export([start_link/1]).
-export([request_handler/1]).
-export([listen/1, close/0]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-include("http_server_logs.hrl").

-define(ListenOptions, [{reuseaddr, true}, {backlog, 5}]).
-define(AcceptsNumber, 16).

start_link(Args) -> gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

request_handler(Handler) ->
	gen_server:call(?MODULE, {request_handler, Handler}).

listen(Port) -> gen_server:call(?MODULE, {listen, Port}).
close() -> gen_server:call(?MODULE, close).

init([{config, Config},
	{request_handler, RequestHandler}, {log_handler, LogHandler}])
->
	process_flag(trap_exit, true),
	{Logger, Report, LogFile} = LogHandler,
	{listen, {port, Port}} = lists:keyfind(listen, 1, Config),
	{logging, {file, LogFileName}} = lists:keyfind(logging, 1, Config),
	{ok, LSocket} = gen_tcp:listen(Port, ?ListenOptions),
	ok = Logger:LogFile(LogFileName),
	ok = Logger:Report(?ListenLog(Port)),
	{ok, [RequestHandler, {Logger, Report}, {LSocket, spawn_accepts(
		RequestHandler, {Logger, Report}, LSocket, ?AcceptsNumber)}]};
init([{config_file, FileName},
	RequestHandler = {request_handler,_}, LogHandler = {log_handler, _}])
->
	{ok, Config} = file:consult(utils:filepath(FileName, ?MODULE)),
	init([{config, Config}, RequestHandler, LogHandler]).

handle_call({request_handler, RequestHandler}, _From,
	[_OldRequestHandler, LogHandler, Listen = {_LSocket, Pids}])
->
	[Pid ! {request_handler, RequestHandler} || Pid <- Pids],
	{reply, ok, [RequestHandler, LogHandler, Listen]};
handle_call({request_handler, RequestHandler}, _From,
	[{_OldRequestHandler}, LogHandler, not_listening])
->
	{reply, ok, [RequestHandler, LogHandler, not_listening]};

handle_call({listen, Port}, _From,
	State = [_RequestHandler, _LogHandler, not_listening])
->
	listen(gen_tcp:listen(Port, ?ListenOptions), State);
handle_call({listen, _}, _From, State) ->
	{reply, {error, already_listening}, State};

handle_call(close, _From, [RequestHandler, LogHandler, {LSocket, _}]) ->
	{reply, gen_tcp:close(LSocket),
		[RequestHandler, LogHandler, not_listening]};
handle_call(close, _From, State) -> {reply, {error, not_listening}, State}.

handle_cast(accept, State = [RequestHandler, LogHandler, {LSocket, _}]) ->
	spawn_accept(RequestHandler, LogHandler, LSocket),
	{noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, [_RequestHandler, _LogHandler, {LSocket, _}]) ->
	gen_tcp:close(LSocket).

code_change(_OldVsn, State, _Extra) -> {ok, State}.


listen({ok, LSocket}, [RequestHandler, LogHandler, _]) ->
	{reply, ok, [RequestHandler, LogHandler, {LSocket, spawn_accepts(
		RequestHandler, LogHandler, LSocket, ?AcceptsNumber)}]};
listen(Error, State) -> {reply, Error, State}.

spawn_accept(RequestHandler, LogHandler, LSocket) -> proc_lib:spawn_link(
	http_handler, accept, [?MODULE, RequestHandler, LogHandler, LSocket]).

spawn_accepts(RequestHandler, LogHandler, LSocket, AcceptsNumber) ->
	[spawn_accept(RequestHandler, LogHandler, LSocket)
		|| _ <-lists:seq(1, AcceptsNumber)].

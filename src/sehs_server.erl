%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created : 16 Oct 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(sehs_server).
-behaviour(gen_server).

-export([start_link/0]).

-export([listen/1, close/0]).
-export([set_log_handler/1, set_request_handler/1]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-include("sehs_logs.hrl").

-define(ListenOptions, [{reuseaddr, true}, {backlog, 5}]).
-define(AcceptsNumber, 16).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE,
	utils_app:get_env([config, request_handler, log_handler]), []).

listen(Port) -> gen_server:call(?MODULE, {listen, Port}).
close() -> gen_server:call(?MODULE, close).

set_log_handler(Handler) ->
	gen_server:call(?MODULE, {set_log_handler, Handler}).

set_request_handler(Handler) ->
	gen_server:call(?MODULE, {set_request_handler, Handler}).

init([{config, Config},
	{request_handler, RequestHandler}, {log_handler, LogHandler}])
->
	process_flag(trap_exit, true),
	Handlers = {RequestHandler, LogHandler},
	{Logger, Report, SetLogFile} = LogHandler,
	{listen, {port, Port}} = lists:keyfind(listen, 1, Config),
	{logging, {file, LogFileName}} = lists:keyfind(logging, 1, Config),
	{ok, LSocket} = gen_tcp:listen(Port, ?ListenOptions),
	ok = Logger:SetLogFile(LogFileName),
	ok = Logger:Report(?ListenLog(Port)),
	{ok, [Config, Handlers, {LSocket, spawn_accepts(
		Handlers, LSocket, ?AcceptsNumber)}]}.

handle_call({set_log_handler, LogHandler}, _From,
	[Config, {RequestHandler, _OldLogHandler}, Listen = {_LSocket, Pids}])
->
	[send_log_handler(Pid, LogHandler) || Pid <- Pids],
	{reply, ok, [Config, {RequestHandler, LogHandler}, Listen]};
handle_call({set_log_handler, LogHandler}, _From,
	[Config, {RequestHandler, _OldLogHandler}, not_listening])
->
	{reply, ok, [Config, {RequestHandler, LogHandler}, not_listening]};

handle_call({set_request_handler, RequestHandler}, _From,
	[Config, {_OldRequestHandler, LogHandler}, Listen = {_LSocket, Pids}])
->
	[send_request_handler(Pid, RequestHandler) || Pid <- Pids],
	{reply, ok, [Config, {RequestHandler, LogHandler}, Listen]};
handle_call({set_request_handler, RequestHandler}, _From,
	[Config, {_OldRequestHandler, LogHandler}, not_listening])
->
	{reply, ok, [Config, {RequestHandler, LogHandler}, not_listening]};

handle_call({listen, Port}, _From, [Config, Handlers, not_listening]) ->
	case gen_tcp:listen(Port, ?ListenOptions) of
		{ok, LSocket} -> {reply, ok, [Config, Handlers,
			{LSocket, spawn_accepts(Handlers, LSocket, ?AcceptsNumber)}]};
		Error -> {reply, Error, [Config, Handlers, not_listening]}
	end;
handle_call({listen, _}, _From, State) ->
	{reply, {error, already_listening}, State};

handle_call(close, _From, [Config, Handlers, {LSocket, _}]) ->
	{reply, gen_tcp:close(LSocket), [Config, Handlers, not_listening]};
handle_call(close, _From, State) -> {reply, {error, not_listening}, State}.

handle_cast(accept, State = [_Config, Handlers, {LSocket, _}]) ->
	spawn_accept(Handlers, LSocket), {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, [_Config, _Handlers, not_listening]) -> ok;
terminate(_Reason, [_Config, _Handlers, {LSocket, _}]) ->
	gen_tcp:close(LSocket).

code_change(_OldVsn, State, _Extra) -> {ok, State}.


spawn_accepts(Handlers, LSocket, AcceptsNumber) ->
	[spawn_accept(Handlers, LSocket) || _ <- lists:seq(1, AcceptsNumber)].

spawn_accept({RequestHandler, {Logger, Report, _}}, LSocket) ->
	proc_lib:spawn_link(sehs_handler, accept,
		[?MODULE, RequestHandler, {Logger, Report}, LSocket]).

send_log_handler(Pid, {Logger, Report, _}) ->
	Pid ! {set_log_handler, {Logger, Report}}.
send_request_handler(Pid, RequestHandler) ->
	Pid ! {set_request_handler, RequestHandler}.

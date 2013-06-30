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
-export([set_handler/1]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-include("sehs_logs.hrl").

-record(state, {config, handlers, listen}).
-record(config, {port, log_file}).

-define(ListenOptions, [{reuseaddr, true}, {backlog, 5}]).
-define(AcceptsNumber, 16).

-define(ConfigPaths, [[listen, port], [logging, file]]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE,
	utils_app:get_env([config, handlers]), []).

listen(Port) -> gen_server:call(?MODULE, {listen, Port}).
close() -> gen_server:call(?MODULE, close).

set_handler(Handler) -> gen_server:call(?MODULE, {set_handler, Handler}).

init([{config, Config}, {handlers, HandlersConfig}]) ->
	process_flag(trap_exit, true),
	[Port, LogFile] = [Value || {ok, Value} <-
		[utils_tree:leaf(Path, Config) || Path <- ?ConfigPaths]],
	Handlers = sehs_handlers_manager:read_handlers(HandlersConfig),
	{ok, LSocket} = gen_tcp:listen(Port, ?ListenOptions),
	ok = sehs_handlers_manager:set_log_file(LogFile, Handlers),
	ok = sehs_handlers_manager:log_report(?ListenLog(Port), Handlers),
	{ok, #state{
		config = #config{port = Port, log_file = LogFile}, handlers = Handlers,
		listen = spawn_accepts(Handlers, LSocket, ?AcceptsNumber)
	}}.

handle_call({set_handler, Handler}, _From, S = #state{listen = Listen}) ->
	{reply, ok, S#state{handlers = send_handlers(sehs_handlers_manager:
		update_handlers(Handler, S#state.handlers), Listen)}};

handle_call({listen, Port}, _From, S = State = #state{
	config = C, handlers = Handlers, listen = not_listening})
->
	case gen_tcp:listen(Port, ?ListenOptions) of
		{ok, LSocket} -> {reply, ok, S#state{config = C#config{port = Port},
			listen = spawn_accepts(Handlers, LSocket, ?AcceptsNumber)}};
		Error -> {reply, Error, State}
	end;
handle_call({listen, _Port}, _From, State) ->
	{reply, {error, already_listening}, State};

handle_call(close, _From, S = #state{listen = {LSocket, _Pids}}) ->
	{reply, gen_tcp:close(LSocket), S#state{listen = not_listening}};
handle_call(close, _From, State) -> {reply, {error, not_listening}, State}.

handle_cast(accept, S = #state{listen = {LSocket, Pids}}) ->
	{noreply, S#state{listen = {LSocket,
		[spawn_accept(S#state.handlers, LSocket)|Pids]}}}.

handle_info({'EXIT', Pid, _Reason}, S = #state{listen = {LSocket, Pids}}) ->
	{noreply, S#state{listen = {LSocket, lists:delete(Pid, Pids)}}};
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, #state{listen = not_listening}) -> ok;
terminate(_Reason, #state{listen = {LSocket, _Pids}}) ->
	gen_tcp:close(LSocket).

code_change(_OldVsn, State, _Extra) -> {ok, State}.


spawn_accepts(Handlers, LSocket, AcceptsNumber) -> {LSocket,
	[spawn_accept(Handlers, LSocket) || _ <- lists:seq(1, AcceptsNumber)]}.

spawn_accept(Handlers, LSocket) ->
	proc_lib:spawn_link(sehs_handler, accept, [?MODULE, Handlers, LSocket]).

send_handlers(Handlers, not_listening) -> Handlers;
send_handlers(Handlers, {_LSocket, Pids}) ->
	[Pid ! {set_handlers, Handlers} || Pid <- Pids], Handlers.

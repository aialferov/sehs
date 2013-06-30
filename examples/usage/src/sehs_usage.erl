%%%-------------------------------------------------------------------
%%% Created : 27 Oct 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(sehs_usage).

-export([start/0, stop/0]).

-export([handle_request/1]).
-export([set_log_file/1, log_report/1]).

-define(Headers, "Content-Type: text/plain; charset=\"utf-8\"\r\n").

start() -> application:start(?MODULE).
stop() -> application:stop(?MODULE).

handle_request(Request) ->
	io:format("Request: ~p~n", [Request]),
	{ok, ?Headers, "OK"}.

set_log_file(FileName) -> io:format("Set log file: ~p~n", [FileName]).
log_report(Report) -> io:format("~p ~s~n", [calendar:local_time(), Report]).

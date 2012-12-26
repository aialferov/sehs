%%%-------------------------------------------------------------------
%%% Created : 27 Oct 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(simple_handler).

-export([handle_request/1, handle_request2/1]).
-export([log_file/1, log_report/1]).

handle_request(Request) -> io:format("Request: ~p~n", [Request]), {ok, "OK"}.
handle_request2(Request) -> io:format("Request2: ~p~n", [Request]), {ok, "OK"}.

log_file(FileName) -> io:format("Log file: ~p~n", [FileName]).
log_report(Report) -> io:format("INFO: ~p~n", [Report]).

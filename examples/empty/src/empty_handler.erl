%%%-------------------------------------------------------------------
%%% Created : 27 Oct 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(empty_handler).

-export([handle_request/1]).
-export([log_file/1, log_report/1]).

handle_request(_) -> {ok, "OK"}.

log_file(_) -> ok.
log_report(_) -> ok.

%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created : 27 Oct 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(http_example_handler).
-export([handle_request/1]).

handle_request(_) -> {ok, "OK"}.

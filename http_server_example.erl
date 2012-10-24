%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created : 21 Oct 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(http_server_example).
-export([handle_query/1]).

handle_query(Query) -> io:format("Query: ~p~n", [Query]), {ok, "OK"}.

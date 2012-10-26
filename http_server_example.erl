%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created : 21 Oct 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(http_server_example).
-export([handle_request/1]).

handle_request({Path, Query}) ->
	io:format("Path: ~p~nQuery: ~p~n", [Path, Query]), {ok, "OK"}.

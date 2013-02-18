%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created :  4 Nov 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(http_server_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Args) -> supervisor:start_link(?MODULE, Args).

init(Args) -> {ok, {{one_for_one, 1, 10}, [
	{http_server, {http_server, start_link, Args},
		permanent, infinity, worker, [http_server]}
]}}.

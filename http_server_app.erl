%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created :  4 Nov 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(http_server_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, StartArgs) -> http_server_sup:start_link(StartArgs).
stop(_State) -> ok.

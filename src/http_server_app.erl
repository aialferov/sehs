%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created :  4 Nov 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(http_server_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) -> http_server_sup:start_link(
	utils:get_env([config_file, config, request_handler, log_handler])).

stop(_State) -> ok.

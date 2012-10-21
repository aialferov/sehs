-module(http_server_example).
-export([handle_query/1]).

handle_query(Query) -> io:format("~p~n", [Query]), {ok, "OK"}.

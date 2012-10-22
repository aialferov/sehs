%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created : 16 Oct 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(http_reader).
-export([read/1]).

read("GET" ++ T) -> read(get, T, []);
read("POST" ++ T) -> read(post, T, []);
read(_) -> {error, method_not_allowed}.

read(Method, "\r\n\r\n" ++ Body, Header) ->
	{ok, read_data(Method, Body, lists:reverse(Header))};
read(Method, [H|T], Header) -> read(Method, T, [H|Header]);
read(_, [], _) -> {error, not_complete}.

read_data(get, _Body, Header) -> read_query(read_get_data(Header), [], []);
read_data(post, Body, _Header) -> read_query(Body, [], []).

read_get_data("?" ++ Data) -> read_get_data(Data, []);
read_get_data([_|T]) -> read_get_data(T);
read_get_data([]) -> [].
read_get_data(" " ++ _, Data) -> lists:reverse(Data);
read_get_data([H|T], Data) -> read_get_data(T, [H|Data]);
read_get_data([], Data) -> lists:reverse(Data).

read_query("=" ++ T, Item, Query) -> read_query(T, [], [Item|Query]);
read_query("&" ++ T, Item, Query) ->
	read_query(T, [], complete_query_item(Item, Query));
read_query([H|T], Item, Query) -> read_query(T, [H|Item], Query);
read_query([], [], []) -> [];
read_query([], Item, Query) ->
	lists:reverse(complete_query_item(Item, Query)).

complete_query_item(Item, [Field|Query]) ->
	[{lists:reverse(Field), http_uri:decode(lists:reverse(Item))}|Query].

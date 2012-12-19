%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created : 16 Oct 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(http_reader).
-export([read/2]).

-define(ContentLengthField, "Content-Length").

read(Request, ContFun) when is_list(Request) ->
	read(read_request_line(Request, [], []), ContFun);

read({request_line, {_, RequestUri, _}}, _ContFun) ->
	read(read_request_uri(RequestUri, []));
read({request_line, RequestLine, Rest}, ContFun) ->
	read({{request_line, RequestLine}, read_header(Rest, [], [])}, ContFun);
read({request_line_incomplete, Rest, Item, RequestLine}, ContFun) ->
	read_cont(fun(MoreData) -> read(read_request_line(
		Rest ++ MoreData, Item, RequestLine), ContFun) end, ContFun);

read({{request_line, {"GET", RequestUri, _}}, {header, _, _}}, _ContFun) ->
	read(read_request_uri(RequestUri, []));
read({{request_line, {"POST", RequestUri, _}},
	{header, Header, Rest}}, ContFun)
->
	{ok, {RequestUri, read_post(Header, Rest, ContFun)}};
read({{request_line, _}, {header, _, _}}, _ContFun) ->
	{error, method_not_allowed};
read({RequestLine = {request_line, _},
	{header_incomplete, Rest, Item, Header}}, ContFun
) ->
	read_cont(fun(MoreData) -> read({RequestLine, read_header(
		Rest ++ MoreData, Item, Header)}, ContFun) end, ContFun).

read({request_uri, Path}) -> {ok, {Path, []}};
read({request_uri, Path, Query}) -> {ok, {Path, Query}}.

read_post(Header, Body, ContFun) when is_list(Header) ->
	read_post(lists:keyfind(?ContentLengthField, 1, Header), Body, ContFun);
read_post({_, ContentLength}, Body, ContFun) ->
	read_post(list_to_integer(ContentLength), Body,
		byte_size(list_to_binary(Body)), ContFun);
read_post(false, Body, _ContFun) -> read_query(Body, [], []).

read_post(ContentLength, Body, BodySize, ContFun)
	when ContentLength > BodySize
->
	read_cont(fun(MoreData) -> NewBody = Body ++ MoreData, read_post(
		ContentLength, NewBody, byte_size(list_to_binary(NewBody)), ContFun)
	end, ContFun);
read_post(_ContentLength, Body, _BodySize, ContFun) ->
	read_post(false, Body, ContFun).

read_cont(ReadFun, {ContFun, Arg}) -> case ContFun(Arg) of
	{ok, MoreData} -> ReadFun(MoreData);
	{error, no_more_data} -> {error, bad_request}
end.

read_request_line("\r\n\r\n" ++ _, LastItem, RequestLine) ->
	{request_line, complete_request_line(LastItem, RequestLine)};
read_request_line("\r\n" ++ T, LastItem, RequestLine) ->
	{request_line, complete_request_line(LastItem, RequestLine), T};
read_request_line(" " ++ T, Item, RequestLine) ->
	read_request_line(T, [], [lists:reverse(Item)|RequestLine]);
read_request_line([H|T], Item, RequestLine) ->
	read_request_line(T, [H|Item], RequestLine);
read_request_line([], Item, RequestLine) ->
	{request_line_incomplete, [], Item, RequestLine}.

complete_request_line(LastItem, RequestLine) ->
	list_to_tuple(lists:reverse([lists:reverse(LastItem)|RequestLine])).

read_header(": " ++ T, FieldName, Header) ->
	read_header(T, [], [lists:reverse(FieldName)|Header]);
read_header("\r\n\r\n" ++ T, FieldValue, Header) ->
	{header, lists:reverse(complete_header_field(FieldValue, Header)), T};
read_header("\r\n" ++ T, FieldValue, Header) ->
	read_header(T, [], complete_header_field(FieldValue, Header));
read_header([H|T], Item, Header) -> read_header(T, [H|Item], Header);
read_header([], Item, Header) -> {header_incomplete, [], Item, Header}.

complete_header_field(FieldValue, [FieldName|Header]) ->
	[{FieldName, lists:reverse(FieldValue)}|Header].

read_request_uri("?" ++ T, Path) ->
	{request_uri, lists:reverse(Path), read_query(T, [], [])};
read_request_uri([H|T], Path) -> read_request_uri(T, [H|Path]);
read_request_uri([], Path) -> {request_uri, lists:reverse(Path)}.

read_query("=" ++ T, Item, Query) -> read_query(T, [], [Item|Query]);
read_query("&" ++ T, Item, Query) ->
	read_query(T, [], complete_query_item(Item, Query));
read_query([H|T], Item, Query) -> read_query(T, [H|Item], Query);
read_query([], [], []) -> [];
read_query([], Item, Query) ->
	lists:reverse(complete_query_item(Item, Query)).

complete_query_item(Item, [Field|Query]) ->
	[{lists:reverse(Field), http_uri:decode(lists:reverse(Item))}|Query].

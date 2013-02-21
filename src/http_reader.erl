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

read({request_line, {Method, RequestUri, Version}}, _ContFun) ->
	result({Method, read_request_uri(RequestUri, []), Version}, [], []);
read({request_line, RequestLine, Rest}, ContFun) ->
	read({{request_line, RequestLine}, read_headers(Rest, [], [])}, ContFun);
read({request_line_incomplete, Rest, Item, RequestLine}, ContFun) ->
	read_cont(fun(MoreData) -> read(read_request_line(
		Rest ++ MoreData, Item, RequestLine), ContFun) end, ContFun);

read({{request_line, {Method, RequestUri, Version}},
	{headers, Headers, Body}}, ContFun
) ->
	result({Method, read_request_uri(RequestUri, []), Version},
		Headers, read_body(Headers, Body, ContFun));
read({RequestLine = {request_line, _},
	{headers_incomplete, Rest, Item, Headers}}, ContFun
) ->
	read_cont(fun(MoreData) -> read({RequestLine, read_headers(
		Rest ++ MoreData, Item, Headers)}, ContFun) end, ContFun).

result({Method, {request_uri, Path, Query}, Version}, Headers, Body) ->
	{ok, {{Method, Path, Version}, Headers, Query, Body}}.

read_body(Headers, Body, ContFun) when is_list(Headers) ->
	read_body(lists:keyfind(?ContentLengthField, 1, Headers), Body, ContFun);
read_body({_, ContentLength}, Body, ContFun) ->
	read_body(list_to_integer(ContentLength), Body,
		byte_size(list_to_binary(Body)), ContFun);
read_body(false, Body, _ContFun) -> Body.

read_body(ContentLength, Body, BodySize, ContFun)
	when ContentLength > BodySize
->
	read_cont(fun(MoreData) -> NewBody = Body ++ MoreData, read_body(
		ContentLength, NewBody, byte_size(list_to_binary(NewBody)), ContFun)
	end, ContFun);
read_body(_ContentLength, Body, _BodySize, ContFun) ->
	read_body(false, Body, ContFun).

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

read_headers(": " ++ T, FieldName, Headers) ->
	read_headers(T, [], [lists:reverse(FieldName)|Headers]);
read_headers("\r\n\r\n" ++ T, FieldValue, Headers) ->
	{headers, lists:reverse(complete_header_field(FieldValue, Headers)), T};
read_headers("\r\n" ++ T, FieldValue, Headers) ->
	read_headers(T, [], complete_header_field(FieldValue, Headers));
read_headers([H|T], Item, Headers) -> read_headers(T, [H|Item], Headers);
read_headers([], Item, Headers) -> {headers_incomplete, [], Item, Headers}.

complete_header_field(FieldValue, [FieldName|Headers]) ->
	[{FieldName, lists:reverse(FieldValue)}|Headers].

read_request_uri("?" ++ T, Path) ->
	{request_uri, lists:reverse(Path), utils_http:read_query(T)};
read_request_uri([H|T], Path) -> read_request_uri(T, [H|Path]);
read_request_uri([], Path) -> {request_uri, lists:reverse(Path), []}.

%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created: 07 Dec 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-define(ListenLog(Port), "listen on " ++ integer_to_list(Port)).

-define(MoreDataLog(Data), pid_to_list(self()) ++ " [REQP] " ++ Data).
-define(RequestLog(Request), pid_to_list(self()) ++ " [REQ] " ++ Request).
-define(ResponseLog(Response),
	pid_to_list(self()) ++ " [RSP] " ++ Response).

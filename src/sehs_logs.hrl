%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created: 07 Dec 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-define(Log(Text), pid_to_list(self()) ++ " " ++ io_lib:format("~p", [Text])).

-define(InfoLog(Text), "[INFO] " ++ ?Log(Text)).
-define(ErrorLog(Text), "[ERROR] " ++ ?Log(Text)).

-define(ListenLog(Port), ?InfoLog("listen on " ++ integer_to_list(Port))).

-define(MoreDataLog(Data), ?InfoLog("[REQP] " ++ Data)).
-define(RequestLog(Request), ?InfoLog("[REQ] " ++ Request)).
-define(ResponseLog(Response), ?InfoLog("[RSP] " ++ Response)).

-define(ClosedAtAcceptLog, ?InfoLog("closed at accept")).
-define(ClosedAtReceiveLog, ?InfoLog("closed at receive")).

-define(TcpErrorLog(Reason), ?ErrorLog("[TCP] " ++ Reason)).

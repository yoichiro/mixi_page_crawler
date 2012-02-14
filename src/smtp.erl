-module(smtp).
-export([connect/1]).

-include("data.hrl").

connect(Feed) ->
    {ok, Socket} = gen_tcp:connect("smtp.eisbahn.jp", 25, [{active, false}], 3000),
    recv(Socket),
    send(Socket, "HELO localhost"),
    send(Socket, "AUTH LOGIN"),
    send(Socket, binary_to_list(base64:encode("yoichiro"))),
    send(Socket, binary_to_list(base64:encode(""))),
    send(Socket, "MAIL FROM: <yoichiro@eisbahn.jp>"),
    send(Socket, "RCPT TO: <yoichiro6642@gmail.com>"),
    send(Socket, "DATA"),
    send_no_receive(Socket, "FROM: <yoichiro@eisbahn.jp>"),
    send_no_receive(Socket, "TO: <yoichiro6642@gmail.com>"),
    send_no_receive(Socket, "Date: Wed, 25 Jan 2012 21:47:00 +0900"),
    send_no_receive(Socket, "Subject: Test message"),
    send_no_receive(Socket, "Content-Type: text/html; charset=\"iso-2022-jp\""),
    send_no_receive(Socket, ""),
%%    send_no_receive(Socket, creole:from_string("これはテストです。", utf8)),
%%    send_no_receive(Socket, "<h1>これはテストです。</h1>"),
    send_no_receive(Socket, Feed#feed.text),
    send_no_receive(Socket, ""),
    send(Socket, "."),
    send(Socket, "QUIT"),
    gen_tcp:close(Socket).

send_no_receive(Socket, Data) ->
%%    gen_tcp:send(Socket, Data ++ "\r\n"),
    gen_tcp:send(Socket, Data),
    gen_tcp:send(Socket, "\r\n").

send(Socket, Data) ->
%%    gen_tcp:send(Socket, Data ++ "\r\n"),
    gen_tcp:send(Socket, Data),
    gen_tcp:send(Socket, "\r\n"),
    recv(Socket).

recv(Socket) ->
    case gen_tcp:recv(Socket, 0, 3000) of
        {ok, Return} ->
            io:format("~p~n", [Return]);
        {error, Reason} ->
            io:format("ERROR: ~p~n", [Reason])
    end.

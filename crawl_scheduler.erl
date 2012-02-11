-module(crawl_scheduler).

%% Exported for a client.
-export([start_server/3,
         command/1]).

%% Exported for the OTP.
-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2]).

%% Exported functions for a client.
start_server(Incr, Max, Parallels) ->
    OffsetList = lists:seq(1, Max, Incr),
    gen_server:start_link(
      {local, crawl_scheduler_server},
      ?MODULE,
      {Incr, Parallels, OffsetList},
      []),
    Command = spawn(?MODULE, command, [[]]),
    Kicker = timer:send_interval(15000, Command, crawl),
    Command ! {set_kicker, Kicker},
    io:format("Crawl server started.~n", []),
    {started, Command}.

command(State) ->
    receive
        {set_kicker, Kicker} ->
            command([{kicker, Kicker} | State]);
        crawl ->
            io:format("~p~n", [crawl()]),
            command(State);
        exit ->
            {kicker, Kicker} = lists:keyfind(kicker, 1, State),
            timer:cancel(Kicker),
            io:format("~p~n", [stop()])
    end.

%% Exported functions for the OTP.
init({Incr, Parallels, OffsetList}) ->
    {ok, {Incr, Parallels, OffsetList, OffsetList}}.

handle_cast(crawl,
            {Incr, Parallels, OffsetList, [PageID|Tail]}) ->
    crawler:start(PageID, Incr, Parallels),
    io:format("~p~p: Crawled. ~p - ~p~n",
              [date(), time(), PageID, PageID + Incr - 1]),
    case Tail of
        [] ->
            {noreply,
             {Incr, Parallels, OffsetList, OffsetList}};
        _ ->
            {noreply,
             {Incr, Parallels, OffsetList, Tail}}
    end.

handle_call(stop, {_From, _Ref}, State) ->
    {stop, "Terminated by client", State}.

terminate(Reason, _State) ->
    io:format("~p~n", [Reason]),
    ok.

%% Internal functions

crawl() ->
    gen_server:cast(crawl_scheduler_server, crawl).

stop() ->
    gen_server:call(crawl_scheduler_server, stop).

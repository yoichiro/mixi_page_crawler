-module(crawl_scheduler).

%% Exported for a client.
-export([start_server/4,
         command/1]).

%% Exported for the OTP.
-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

%% Exported functions for a client.
start_server(Incr, Max, Parallels, Interval) ->
    logger:start(),
    OffsetList = lists:seq(1, Max, Incr),
    gen_server:start_link(
      {global, crawl_scheduler_server},
      ?MODULE,
      {Incr, Parallels, OffsetList},
      []),
    Command = spawn(?MODULE, command, [[]]),
    Kicker = timer:send_interval(Interval * 1000, Command, crawl),
    Command ! {set_kicker, Kicker},
    logger:log("Crawl server started."),
    {started, Command}.

command(State) ->
    receive
        {set_kicker, Kicker} ->
            logger:log("Receive: {set_kicker, ~p}", [Kicker]),
            command([{kicker, Kicker} | State]);
        crawl ->
            logger:log("Receive: crawl"),
            crawl(),
            command(State);
        exit ->
            logger:log("Receive: exit"),
            {kicker, Kicker} = lists:keyfind(kicker, 1, State),
            timer:cancel(Kicker),
            stop()
    end.

%% Exported functions for the OTP.
init({Incr, Parallels, OffsetList}) ->
    {ok, {Incr, Parallels, OffsetList, OffsetList}}.

handle_cast(crawl,
            {Incr, Parallels, OffsetList, [PageID|Tail]}) ->
    logger:log("Starting crawl. ~p - ~p", [PageID, PageID + Incr - 1]),
    crawler:start(PageID, Incr, Parallels),
    logger:log("Finish crawl. ~p - ~p", [PageID, PageID + Incr - 1]),
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

handle_info(Info, State) ->
    logger:log("crawl_scheduler:handle_info(~p) called.", [Info]),
    {norely, State}.

terminate(Reason, _State) ->
    logger:log("crawl_scheduler:terminate, Reason=~p", [Reason]),
    logger:stop(),
    ok.

%% Internal functions

crawl() ->
    logger:log("crawl_scheduler:crawl() called."),
    gen_server:cast({global, crawl_scheduler_server}, crawl).

stop() ->
    logger:log("crawl_scheduler:stop() called."),
    gen_server:call({global, crawl_scheduler_server}, stop).

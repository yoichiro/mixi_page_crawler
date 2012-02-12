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
    gen_event:start_link({global, logger}),
    gen_event:add_handler({global, logger}, file_logger, "crawler.log"),
    OffsetList = lists:seq(1, Max, Incr),
    gen_server:start_link(
      {global, crawl_scheduler_server},
      ?MODULE,
      {Incr, Parallels, OffsetList},
      []),
    Command = spawn(?MODULE, command, [[]]),
    Kicker = timer:send_interval(10000, Command, crawl),
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
    log(io_lib:format(
          "Starting crawl. ~p - ~p",
          [PageID, PageID + Incr - 1])),
    crawler:start(PageID, Incr, Parallels),
    log(io_lib:format(
          "Finish crawl. ~p - ~p",
          [PageID, PageID + Incr - 1])),
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
    gen_event:delete_handler({global, logger}, file_logger, []),
    io:format("~p~n", [Reason]),
    ok.

%% Internal functions

crawl() ->
    gen_server:cast({global, crawl_scheduler_server}, crawl).

stop() ->
    gen_server:call({global, crawl_scheduler_server}, stop).

log(Message) ->
    gen_event:notify({global, logger}, Message).

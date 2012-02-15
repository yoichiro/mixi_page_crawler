-module(crawler).
-export([start/3,
         fetch/2]).

start(PageID, Incr, Parallels) ->
    logger:log("Starting crawler:start(), PageID=~p, Incr=~p, Parallels=~p",
               [PageID, Incr, Parallels]),
    PageIDList = lists:seq(PageID, PageID + Incr - 1),
    crawl(PageIDList, 0, Parallels, Parallels),
    logger:log("Finish crawler:start(), PageID=~p, Incr=~p, Parallels=~p",
               [PageID, Incr, Parallels]),
    ok.

crawl([], Current, NextPos, Parallels) ->
    wait(Parallels - (NextPos - Current)),
    ok;
crawl(PageIDList, Current, NextPos, Parallels) when Current == NextPos ->
    wait(Parallels),
    crawl(PageIDList, Current, NextPos + Parallels, Parallels);
crawl([PageID | PageIDList], Current, NextPos, Parallels) ->
    spawn(?MODULE, fetch, [self(), PageID]),
    crawl(PageIDList, Current + 1, NextPos, Parallels).

wait(0) ->
    ok;
wait(Count) ->
    receive
        {crawled, PageID} ->
            logger:log("crawler:wait(~p) crawled. PageID=~p",
                       [Count, PageID]),
            wait(Count - 1);
        {skipped, PageID} ->
            logger:log("crawler:wait(~p) skipped. PageID=~p",
                       [Count, PageID]),
            wait(Count - 1)
    after 60000 ->
            logger:log("Timeout(~p).", [Count]),
            ok
    end.

fetch(From, PageID) ->
    logger:log("Starting crawler:fetch(~p, ~p)", [From, PageID]),
    case fetcher:fetch_page(PageID) of
        {PageInfo, Feeds} ->
            db:store_page(PageInfo),
            db:store_feeds(Feeds),
            From ! {crawled, PageID};
        skip ->
            From ! {skipped, PageID}
    end,
    logger:log("Finish crawler:fetch(~p, ~p)", [From, PageID]).

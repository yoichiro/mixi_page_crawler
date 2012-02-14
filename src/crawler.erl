-module(crawler).
-export([start/3,
         fetch/2]).

start(PageID, Incr, Parallels) ->
    PageIDList = lists:seq(PageID, PageID + Incr - 1),
    [PageID | _] = PageIDList,
    crawl(PageIDList, 0, Parallels, Parallels).

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
        {crawled, _PageID} ->
%            io:format("~p: crawled.~n", [PageID]),
            wait(Count - 1);
        {skipped, _PageID} ->
%            io:format("~p: skipped.~n", [PageID])
            wait(Count - 1)
    after 60000 ->
            io:format("Timeout(~p).~n", [Count]),
            ok
    end.

fetch(From, PageID) ->
    case fetcher:fetch_page(PageID) of
        {PageInfo, Feeds} ->
            db:store_page(PageInfo),
            db:store_feeds(Feeds),
            From ! {crawled, PageID};
        skip ->
            From ! {skipped, PageID}
    end.


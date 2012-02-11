-module(test).
-export([run/0]).

run() ->
%    mnesia:start(),
%    inets:start(),
    lists:foreach(
      fun(PageId) ->
              case fetcher:fetch_page(PageId) of
                  {PageInfo, Feeds} ->
                      db:store_page(PageInfo),
                      db:store_feeds(Feeds),
                      io:format("~p: crawled.~n", [PageId]);
                  skip ->
%                      io:format("~p: skipped.~n", [PageId]),
                      ok
              end
      end,
%      lists:seq(1, 158633)).
      lists:seq(5000, 5100)).
%    inets:stop(),
%    mnesia:stop().

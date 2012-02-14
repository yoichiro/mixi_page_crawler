-module(db).
-export([setup/0,
         drop_all/0,
         store_page/1,
         store_feeds/1,
         select_all_pages/0,
         select_all_feeds/1,
         select_feeds_by_keyword/1,
         delete_all_old_feeds/0
        ]).

-include_lib("stdlib/include/qlc.hrl").
-include("data.hrl").

setup() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    case mnesia:create_table(
           page,
           [ {disc_copies, [node()]},
             {attributes, record_info(fields, page)} ]) of
        {atomic, ok} ->
            ok;
        Other ->
            io:format("~p~n", [Other])
    end,
    case mnesia:create_table(
           feed,
           [ {disc_copies, [node()]},
             {attributes, record_info(fields, feed)} ]) of
        {atomic, ok} ->
            ok;
        Other1 ->
            io:format("~p~n", [Other1])
    end,
    mnesia:stop().

drop_all() ->
    mnesia:start(),
    mnesia:delete_table(page),
    mnesia:delete_table(feed),
    mnesia:stop().

select_all_pages() ->
    mnesia:transaction(
      fun() ->
              H = qlc:q([X || X <- mnesia:table(page)]),
              Pages = qlc:e(H),
              lists:foreach(
                fun(Page) ->
                        io:format("~p~n", [Page])
                end,
                Pages)
      end).

select_all_feeds(PageId) ->
    mnesia:transaction(
      fun() ->
              H = qlc:q([X || X <- mnesia:table(feed), X#feed.page_id == PageId]),
              Feeds = qlc:e(H),
              lists:foreach(
                fun(Feed) ->
                        io:format("~p~n", [Feed])
                end,
                Feeds)
      end).

select_feeds_by_keyword(Keyword) ->
    mnesia:transaction(
      fun() ->
              H = qlc:q([X || X <- mnesia:table(feed), string:str(X#feed.text, Keyword) /= 0]),
              Feeds = qlc:e(H),
              lists:foreach(
                fun(Feed) ->
                        io:format("~p~n", [Feed])
                end,
                Feeds),
              Feeds
      end).

store_page(Page) ->
    update_entity(Page).

store_feeds(Feeds) ->
    lists:foreach(
      fun(Feed) ->
              update_entity(Feed)
      end,
      Feeds
     ).

update_entity(Entity) ->
    case mnesia:transaction(
           fun() ->
                   mnesia:write(Entity)
           end
          ) of
        {atomic, ok} ->
            ok;
        Other ->
            io:format("~p~n", [Other]),
            Other
    end.

delete_all_old_feeds() ->
%%    io:format("Starting delete_all_old_feeds...~n"),
    Now = date(),
    SevenDaysBefore = utils:create_date_integer(
                        utils:calc_sub_7days(Now)),
    mnesia:transaction(
      fun() ->
              H = qlc:q([X || X <- mnesia:table(feed),
                              X#feed.timestamp < SevenDaysBefore]),
              Feeds = qlc:e(H),
              lists:foreach(
                fun(Feed) ->
                        mnesia:delete_object(Feed),
                        io:format("Deleted: ~p~n", [Feed#feed.page_id])
                end,
                Feeds)
%%              io:format("Finish delete_all_old_feeds.~n")
      end
     ).

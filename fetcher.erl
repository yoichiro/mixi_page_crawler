-module(fetcher).
-export([fetch_page/1]).
-include("data.hrl").

fetch_page(PageId) ->
    Url = lists:concat(
            ["http://page.mixi.jp/recent_page_feed.pl?page_id=",
             PageId
            ]),
    case get_recent_page_feed_page(Url) of
        {Status, Body} ->
            parse_response(Status, PageId, Url, Body);
        error ->
            skip
    end.

get_recent_page_feed_page(Url) ->
    case httpc:request(
           get,
           {Url, []},
           [],
           []) of
        {ok, {{_, Status, _}, _, Body}} ->
            {Status, Body};
        _ ->
            error
    end.


parse_response(200, PageId, Url, Body) ->
    parse_response(200, PageId, Url, Body, has_feed_list(Body));
parse_response(_, _, _, _) ->
    skip.

parse_response(200, PageId, Url, Body, true) ->
    Title = get_title(PageId, Body),
    Feeds = get_feeds(PageId, Body),
    PageInfo = #page{id = PageId, title = Title, url = Url},
    {PageInfo, Feeds};
parse_response(_, _, _, _, false) ->
    skip.

get_title(_PageId, Body) ->
    {ok, MP} = re:compile("<title>.+</title>"),
    case re:run(Body, MP) of
        {match, Captured} ->
            [{Start, Len}] = Captured,
            lists:sublist(Body, Start + 8, Len - 46);
        nomatch ->
            skip
    end.

has_feed_list(Body) ->
    {ok, MP} = re:compile("<div class=\"feedStream\">"),
    case re:run(Body, MP) of
        {match, _Captured} ->
            true;
        nomatch ->
            false
    end.

get_feeds(PageId, Body) ->
    {ok, MP1} = re:compile("<div class=\"feedText\s?(withPhoto)?\">(.*?)</div>", [dotall]),
    {match, Captured1} = re:run(Body, MP1, [global, {capture, [2]}]),
    {ok, MP2} = re:compile("<li class=\"timeStamp\">(.*)</li>"),
    {match, Captured2} = re:run(Body, MP2, [global, {capture, [1]}]),
    {ok, MP3} = re:compile("<li class=\"viewLink\">(.*)</li>"),
    {match, Captured3} = re:run(Body, MP3, [global, {capture, [1]}]),
%    io:format("~p~n", [Captured1]),
%    io:format("~p~n", [Captured2]),
%    io:format("~p~n", [Captured3]),
    create_feeds(PageId, Body, Captured1, Captured2, Captured3, []).

get_captured_string(Source, Capture) ->
    [{Start, Len}] = Capture,
    lists:sublist(Source, Start + 1, Len).

create_feeds(_, _, [], [], [], Result) ->
    lists:reverse(Result);
create_feeds(PageId,
             Body,
             [ContentPos | Captured1],
             [TimeStampPos | Captured2],
             [ViewLinkPos | Captured3],
             Result) ->
    Content = get_captured_string(Body, ContentPos),
    {ok, MP} = re:compile("<p class=\"comment\">(.+)</p>"),
    case re:run(Content, MP, [global, {capture, [1]}]) of
        {match, [Captured4 | _]} ->
            case Captured4 of
                [] ->
                    Text = "";
                _ ->
                    Text = get_captured_string(Content, Captured4)
            end;
        nomatch ->
            Text = ""
    end,
    TimeStamp = get_captured_string(Body, TimeStampPos),
    ViewLink = get_captured_string(Body, ViewLinkPos),
    Feed = #feed{id = md5:md5_hex(Content),
                 page_id = PageId,
                 text = Text,
                 timestamp = create_timestamp(TimeStamp),
                 view_link = ViewLink},
    create_feeds(PageId, Body, Captured1, Captured2, Captured3,
                 [Feed | Result]).

create_timestamp(Source) ->
    {CurrentYear, CurrentMonth, CurrentDate} = date(),
    {ok, MP} = re:compile("([01]?[0-9])月([01]?[0-9])日"),
    case re:run(Source, MP, [global, {capture, [1, 2]}]) of
        {match, [[Captured1, Captured2]]} ->
            {Month, _} = string:to_integer(get_captured_string(Source, [Captured1])),
            {Date, _} = string:to_integer(get_captured_string(Source, [Captured2])),
            if CurrentMonth < Month ->
                    Year = CurrentYear - 1;
               true ->
                    Year = CurrentYear
            end,
            utils:create_date_integer({Year, Month, Date});
        _ ->
            utils:create_date_integer({CurrentYear, CurrentMonth, CurrentDate})
    end.

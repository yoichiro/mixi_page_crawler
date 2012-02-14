-record(page, {id,
               title,
               url}).

-record(feed, {id,
               page_id,
               text,
               timestamp,
               view_link}).

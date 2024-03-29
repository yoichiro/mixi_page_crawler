-module(file_logger).
-export([init/1,
         handle_event/2,
         terminate/2]).

init(File) ->
    {ok, Fd} = file:open(File, [append]),
    {ok, Fd}.

handle_event(Message, Fd) ->
    {Year, Month, Date} = date(),
    {Hour, Minute, Second} = time(),
    io:format(
      Fd,
      "[~4..0b/~2..0b/~2..0b ~2..0b:~2..0b:~2..0b] ~s~n",
      [Year, Month, Date,
       Hour, Minute, Second,
       Message]),
    {ok, Fd}.

terminate(_Args, Fd) ->
    file:close(Fd).


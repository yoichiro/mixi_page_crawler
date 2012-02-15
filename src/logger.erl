-module(logger).
-export([start/0,
         stop/0,
         log/1,
         log/2]).

start() ->
    gen_event:start_link({global, logger}),
    gen_event:add_handler({global, logger}, file_logger, "crawler.log").

stop() ->
    gen_event:delete_handler({global, logger}, file_logger, []).

log(Message) ->
    gen_event:notify({global, logger}, Message).

log(Template, Args) ->
    Message = io_lib:format(Template, Args),
    log(Message).

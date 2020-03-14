-module(erlskat).
-export([start/0]).

start() ->
    error_logger:info_report(
      [{module, ?MODULE},
       {line, ?LINE},
       {function, ?FUNCTION_NAME},
       starting]
     ),
    application:ensure_all_started(?MODULE).

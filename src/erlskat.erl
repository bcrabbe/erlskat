-module(erlskat).
-export([start/0]).
-export_type([player_id/0, player/0]).

-type player_id() :: binary().

-type player() :: #{id => binary(),
                    socket => pid()}.

start() ->
    error_logger:info_report(
      [{module, ?MODULE},
       {line, ?LINE},
       {function, ?FUNCTION_NAME},
       starting]
     ),
    application:ensure_all_started(?MODULE).

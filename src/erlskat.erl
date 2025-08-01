-module(erlskat).
-export([start/0]).
-export_type([player_id/0, player/0, suit/0, rank/0, card/0, cards/0, skat/0]).

-type player_id() :: binary().
-type player() :: #{id => player_id(),
                    socket => pid()}.

-type suit() :: clubs | diamonds | hearts | spades.
-type rank() ::
        ace |
        seven |
        eight |
        nine |
        ten |
        jack |
        queen |
        king.

-type card() :: #{rank => rank(), suit => suit()}.
-type cards() :: list(erlskat:card()).
-type skat() :: cards().

start() ->
    error_logger:info_report(
      [{module, ?MODULE},
       {line, ?LINE},
       {function, ?FUNCTION_NAME},
       starting]
     ),
    application:ensure_all_started(?MODULE).

-module(erlskat).
-export([start/0]).

-spec start() -> {ok, [atom()]} | {error, term()}.
-export_type([player_id/0, player/0, players/0, suit/0, rank/0, card/0, cards/0,
               skat/0, suit_game_type/0, game_type/0]).

-type player_id() :: binary().
-type player() :: #{id => player_id()}.
-type players() :: list(player()).

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
-type suit_game_type() :: clubs | spades | hearts | diamonds.
-type game_type() :: suit_game_type() | grand | null.

start() ->
    error_logger:info_report(
      [{module, ?MODULE},
       {line, ?LINE},
       {function, ?FUNCTION_NAME},
       starting]
     ),
    application:ensure_all_started(?MODULE).

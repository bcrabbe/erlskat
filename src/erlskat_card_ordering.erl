%%%-------------------------------------------------------------------
%%% @author Ben Crabbe <ben.crabbe.dev@gmail.com>
%%% @copyright (C) 2019, Ben Crabbe
%%% @doc
%%% Card ordering functions for different Skat game types
%%% Provides consistent card ordering across bidding and gameplay phases
%%% @end
%%% Created : 1 Aug 2025 by Ben Crabbe <ben.crabbe.dev@gmail.com>
%%%-------------------------------------------------------------------
-module(erlskat_card_ordering).

-include_lib("kernel/include/logger.hrl").

%% API exports
-export([
    order_cards_for_skat/1,
    order_cards_for_game_type/2,
    get_card_ordering_for_game_type/1,
    reorder_all_hands_for_game_type/2
]).

%% Helper function exports
-export([
    get_suit_game_ordering/1,
    get_grand_game_ordering/0,
    get_null_game_ordering/0
]).

%% Skat card ordering: jacks first, then suits (clubs, spades, hearts, diamonds)
%% Within suits: A, 10, K, Q, 9, 8, 7
-define(SKAT_ORDERING, [
                        %% Jacks (highest)
                        #{rank => jack, suit => clubs},
                        #{rank => jack, suit => spades},
                        #{rank => jack, suit => hearts},
                        #{rank => jack, suit => diamonds},
                        %% Clubs
                        #{rank => ace, suit => clubs},
                        #{rank => ten, suit => clubs},
                        #{rank => king, suit => clubs},
                        #{rank => queen, suit => clubs},
                        #{rank => nine, suit => clubs},
                        #{rank => eight, suit => clubs},
                        #{rank => seven, suit => clubs},
                        %% Spades
                        #{rank => ace, suit => spades},
                        #{rank => ten, suit => spades},
                        #{rank => king, suit => spades},
                        #{rank => queen, suit => spades},
                        #{rank => nine, suit => spades},
                        #{rank => eight, suit => spades},
                        #{rank => seven, suit => spades},
                        %% Hearts
                        #{rank => ace, suit => hearts},
                        #{rank => ten, suit => hearts},
                        #{rank => king, suit => hearts},
                        #{rank => queen, suit => hearts},
                        #{rank => nine, suit => hearts},
                        #{rank => eight, suit => hearts},
                        #{rank => seven, suit => hearts},
                        %% Diamonds
                        #{rank => ace, suit => diamonds},
                        #{rank => ten, suit => diamonds},
                        #{rank => king, suit => diamonds},
                        #{rank => queen, suit => diamonds},
                        #{rank => nine, suit => diamonds},
                        #{rank => eight, suit => diamonds},
                        #{rank => seven, suit => diamonds}
                       ]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% Order cards according to standard Skat ordering (for bidding phase)
-spec order_cards_for_skat(erlskat:cards()) -> erlskat:cards().
order_cards_for_skat(Cards) ->
    % Sort cards according to their position in the ordering
    OrderMap = maps:from_list([{Card, Index} || {Index, Card} <- lists:enumerate(?SKAT_ORDERING)]),

    % Sort cards according to their position in the ordering
    sort_cards_by_order_map(Cards, OrderMap).

%% Order cards according to specific game type rules
-spec order_cards_for_game_type(erlskat:cards(), erlskat:game_type()) -> erlskat:cards().
order_cards_for_game_type(Cards, GameType) ->
    Ordering = get_card_ordering_for_game_type(GameType),
    OrderMap = maps:from_list([{Card, Index} || {Index, Card} <- lists:enumerate(Ordering)]),
    sort_cards_by_order_map(Cards, OrderMap).

-spec sort_cards_by_order_map(erlskat:cards(), map()) -> erlskat:cards().
sort_cards_by_order_map(Cards, OrderMap) ->
    lists:sort(fun(Card1, Card2) ->
                       maps:get(Card1, OrderMap, 999) =< maps:get(Card2, OrderMap, 999)
               end, Cards).

%% Get the complete card ordering for a specific game type
-spec get_card_ordering_for_game_type(erlskat:game_type()) -> [erlskat:card()].
get_card_ordering_for_game_type(clubs) ->
    get_suit_game_ordering(clubs);
get_card_ordering_for_game_type(spades) ->
    get_suit_game_ordering(spades);
get_card_ordering_for_game_type(hearts) ->
    get_suit_game_ordering(hearts);
get_card_ordering_for_game_type(diamonds) ->
    get_suit_game_ordering(diamonds);
get_card_ordering_for_game_type(grand) ->
    get_grand_game_ordering();
get_card_ordering_for_game_type(null_game) ->
    get_null_game_ordering().

%% Reorder all player hands according to game type
-spec reorder_all_hands_for_game_type([map()], erlskat:game_type()) -> [map()].
reorder_all_hands_for_game_type(Hands, GameType) ->
    [Hand#{hand => order_cards_for_game_type(maps:get(hand, Hand), GameType)} || Hand <- Hands].

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% Get card ordering for suit games (clubs, spades, hearts, diamonds)
-spec get_suit_game_ordering(erlskat:suit()) -> [erlskat:card()].
get_suit_game_ordering(TrumpSuit) ->
    % Jacks are always trumps (highest), then trump suit cards, then other suits
    Jacks = [#{rank => jack, suit => clubs},
             #{rank => jack, suit => spades},
             #{rank => jack, suit => hearts},
             #{rank => jack, suit => diamonds}],

    TrumpCards = [#{rank => ace, suit => TrumpSuit},
                  #{rank => ten, suit => TrumpSuit},
                  #{rank => king, suit => TrumpSuit},
                  #{rank => queen, suit => TrumpSuit},
                  #{rank => nine, suit => TrumpSuit},
                  #{rank => eight, suit => TrumpSuit},
                  #{rank => seven, suit => TrumpSuit}],

    OtherSuits = [clubs, spades, hearts, diamonds] -- [TrumpSuit],
    NonTrumpCards = lists:flatmap(fun(Suit) ->
        [#{rank => ace, suit => Suit},
         #{rank => ten, suit => Suit},
         #{rank => king, suit => Suit},
         #{rank => queen, suit => Suit},
         #{rank => nine, suit => Suit},
         #{rank => eight, suit => Suit},
         #{rank => seven, suit => Suit}]
    end, OtherSuits),

    Jacks ++ TrumpCards ++ NonTrumpCards.

%% Get card ordering for grand games (only jacks are trumps)
-spec get_grand_game_ordering() -> [erlskat:card()].
get_grand_game_ordering() ->
    % Only Jacks are trumps, all other cards in normal suit order
    Jacks = [#{rank => jack, suit => clubs},
             #{rank => jack, suit => spades},
             #{rank => jack, suit => hearts},
             #{rank => jack, suit => diamonds}],

    NonTrumpCards = lists:flatmap(fun(Suit) ->
        [#{rank => ace, suit => Suit},
         #{rank => ten, suit => Suit},
         #{rank => king, suit => Suit},
         #{rank => queen, suit => Suit},
         #{rank => nine, suit => Suit},
         #{rank => eight, suit => Suit},
         #{rank => seven, suit => Suit}]
    end, [clubs, spades, hearts, diamonds]),

    Jacks ++ NonTrumpCards.

%% Get card ordering for null games (no trumps, jacks are regular cards)
-spec get_null_game_ordering() -> [erlskat:card()].
get_null_game_ordering() ->
    % No trumps, Jacks are regular cards with different ranking: A > K > Q > J > 10 > 9 > 8 > 7
    lists:flatmap(fun(Suit) ->
        [#{rank => ace, suit => Suit},
         #{rank => king, suit => Suit},
         #{rank => queen, suit => Suit},
         #{rank => jack, suit => Suit},
         #{rank => ten, suit => Suit},
         #{rank => nine, suit => Suit},
         #{rank => eight, suit => Suit},
         #{rank => seven, suit => Suit}]
    end, [clubs, spades, hearts, diamonds]).

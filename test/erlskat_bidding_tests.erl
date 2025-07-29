-module(erlskat_bidding_tests).

-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

card_ordering_test_() ->
    [{"Cards are ordered correctly for Skat",
      fun test_card_ordering/0},
     {"Ordering works with partial hands",
      fun test_partial_hand_ordering/0},
     {"Ordering handles empty lists",
      fun test_empty_ordering/0},
     {"Complete deck ordering is consistent",
      fun test_complete_deck_ordering/0}].

bidding_logic_test_() ->
    [{"Next valid bid calculation",
      fun test_next_valid_bid/0},
     {"Bidding pair determination",
      fun test_bidding_pair_determination/0},
     {"Bidding pair with passed players",
      fun test_bidding_pair_with_passed_players/0},
     {"Edge cases for bidding logic",
      fun test_bidding_edge_cases/0}].

deck_and_dealing_test_() ->
    [{"Shuffled deck has correct number of cards",
      fun test_shuffled_deck/0},
     {"Dealing creates correct hand structure",
      fun test_dealing/0},
     {"Dealt hands have correct number of cards",
      fun test_dealt_hands/0}].

helper_functions_test_() ->
    [{"Player lookup by ID",
      fun test_get_player_by_id/0},
     {"Player lookup with non-existent ID",
      fun test_get_player_by_id_not_found/0}].

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

test_card_ordering() ->
    %% Test that cards are ordered according to Skat rules
    Cards = [#{rank => ace, suit => hearts},
             #{rank => jack, suit => clubs},
             #{rank => ten, suit => spades},
             #{rank => king, suit => diamonds},
             #{rank => queen, suit => clubs}],
    
    Ordered = erlskat_bidding:order_cards_for_skat(Cards),
    
    %% Jacks should come first, then by suit order (clubs, spades, hearts, diamonds)
    %% Within suits: A, 10, K, Q, 9, 8, 7
    ExpectedOrder = [#{rank => jack, suit => clubs},
                     #{rank => queen, suit => clubs},
                     #{rank => ten, suit => spades},
                     #{rank => ace, suit => hearts},
                     #{rank => king, suit => diamonds}],
    
    ?assertEqual(ExpectedOrder, Ordered).

test_partial_hand_ordering() ->
    %% Test ordering with just a few cards
    Cards = [#{rank => seven, suit => clubs},
             #{rank => ace, suit => clubs}],
    
    Ordered = erlskat_bidding:order_cards_for_skat(Cards),
    
    %% Ace should come before seven in clubs
    ExpectedOrder = [#{rank => ace, suit => clubs},
                     #{rank => seven, suit => clubs}],
    
    ?assertEqual(ExpectedOrder, Ordered).

test_empty_ordering() ->
    %% Test ordering with empty list
    Ordered = erlskat_bidding:order_cards_for_skat([]),
    ?assertEqual([], Ordered).

test_complete_deck_ordering() ->
    %% Test that complete deck ordering follows Skat rules
    verify_card_ordering_consistency().

test_next_valid_bid() ->
    %% Test that next valid bid is calculated correctly
    ?assertEqual(20, erlskat_bidding:get_next_valid_bid(18)),
    ?assertEqual(22, erlskat_bidding:get_next_valid_bid(20)),
    ?assertEqual(23, erlskat_bidding:get_next_valid_bid(22)),
    ?assertEqual(24, erlskat_bidding:get_next_valid_bid(23)),
    ?assertEqual(27, erlskat_bidding:get_next_valid_bid(24)),
    ?assertEqual(30, erlskat_bidding:get_next_valid_bid(27)),
    ?assertEqual(33, erlskat_bidding:get_next_valid_bid(30)),
    ?assertEqual(35, erlskat_bidding:get_next_valid_bid(33)),
    ?assertEqual(36, erlskat_bidding:get_next_valid_bid(35)),
    ?assertEqual(40, erlskat_bidding:get_next_valid_bid(36)),
    ?assertEqual(44, erlskat_bidding:get_next_valid_bid(40)),
    ?assertEqual(45, erlskat_bidding:get_next_valid_bid(44)),
    ?assertEqual(46, erlskat_bidding:get_next_valid_bid(45)),
    ?assertEqual(48, erlskat_bidding:get_next_valid_bid(46)),
    ?assertEqual(50, erlskat_bidding:get_next_valid_bid(48)),
    ?assertEqual(54, erlskat_bidding:get_next_valid_bid(50)),
    ?assertEqual(55, erlskat_bidding:get_next_valid_bid(54)),
    ?assertEqual(59, erlskat_bidding:get_next_valid_bid(55)),
    ?assertEqual(60, erlskat_bidding:get_next_valid_bid(59)),
    ?assertEqual(63, erlskat_bidding:get_next_valid_bid(60)),
    ?assertEqual(66, erlskat_bidding:get_next_valid_bid(63)),
    ?assertEqual(70, erlskat_bidding:get_next_valid_bid(66)),
    ?assertEqual(72, erlskat_bidding:get_next_valid_bid(70)),
    ?assertEqual(77, erlskat_bidding:get_next_valid_bid(72)),
    ?assertEqual(80, erlskat_bidding:get_next_valid_bid(77)),
    ?assertEqual(81, erlskat_bidding:get_next_valid_bid(80)),
    ?assertEqual(84, erlskat_bidding:get_next_valid_bid(81)),
    ?assertEqual(88, erlskat_bidding:get_next_valid_bid(84)),
    ?assertEqual(90, erlskat_bidding:get_next_valid_bid(88)),
    ?assertEqual(96, erlskat_bidding:get_next_valid_bid(90)),
    ?assertEqual(99, erlskat_bidding:get_next_valid_bid(96)),
    ?assertEqual(100, erlskat_bidding:get_next_valid_bid(99)),
    ?assertEqual(108, erlskat_bidding:get_next_valid_bid(100)),
    ?assertEqual(110, erlskat_bidding:get_next_valid_bid(108)),
    ?assertEqual(117, erlskat_bidding:get_next_valid_bid(110)),
    ?assertEqual(120, erlskat_bidding:get_next_valid_bid(117)),
    ?assertEqual(none, erlskat_bidding:get_next_valid_bid(120)).

test_bidding_pair_determination() ->
    %% Test bidding pair determination with all players active
    BiddingOrder = [middlehand, rearhand, forehand],
    PassedPlayers = [],
    
    Result = erlskat_bidding:get_next_bidding_pair(BiddingOrder, PassedPlayers),
    ?assertEqual({middlehand, forehand}, Result).

test_bidding_pair_with_passed_players() ->
    %% Test bidding pair determination when some players have passed
    BiddingOrder = [middlehand, rearhand, forehand],
    
    %% Middlehand passed
    PassedPlayers1 = [middlehand],
    Result1 = erlskat_bidding:get_next_bidding_pair(BiddingOrder, PassedPlayers1),
    ?assertEqual({rearhand, forehand}, Result1),
    
    %% Forehand passed
    PassedPlayers2 = [forehand],
    Result2 = erlskat_bidding:get_next_bidding_pair(BiddingOrder, PassedPlayers2),
    ?assertEqual({rearhand, middlehand}, Result2),
    
    %% Rearhand passed
    PassedPlayers3 = [rearhand],
    Result3 = erlskat_bidding:get_next_bidding_pair(BiddingOrder, PassedPlayers3),
    ?assertEqual({middlehand, forehand}, Result3),
    
    %% Two players passed
    PassedPlayers4 = [middlehand, forehand],
    Result4 = erlskat_bidding:get_next_bidding_pair(BiddingOrder, PassedPlayers4),
    ?assertEqual(no_more_bidders, Result4),
    
    %% All players passed
    PassedPlayers5 = [middlehand, rearhand, forehand],
    Result5 = erlskat_bidding:get_next_bidding_pair(BiddingOrder, PassedPlayers5),
    ?assertEqual(no_more_bidders, Result5).

test_bidding_edge_cases() ->
    %% Test edge cases for bidding logic
    BiddingOrder = [middlehand, rearhand, forehand],
    
    %% Test with invalid bid values
    ?assertEqual(none, erlskat_bidding:get_next_valid_bid(120)),
    ?assertEqual(none, erlskat_bidding:get_next_valid_bid(200)),
    
    %% Test with minimum valid bid
    ?assertEqual(20, erlskat_bidding:get_next_valid_bid(18)),
    
    %% Test with maximum valid bid
    ?assertEqual(none, erlskat_bidding:get_next_valid_bid(120)),
    
    %% Test bidding pair with empty passed players list
    Result = erlskat_bidding:get_next_bidding_pair(BiddingOrder, []),
    ?assertEqual({middlehand, forehand}, Result),
    
    %% Test bidding pair with all players passed
    Result2 = erlskat_bidding:get_next_bidding_pair(BiddingOrder, [middlehand, rearhand, forehand]),
    ?assertEqual(no_more_bidders, Result2).

test_shuffled_deck() ->
    %% Test that shuffled deck has correct number of cards
    Deck = erlskat_bidding:shuffled_deck(),
    ?assertEqual(32, length(Deck)),
    
    %% Test that all cards are unique
    UniqueCards = sets:to_list(sets:from_list(Deck)),
    ?assertEqual(32, length(UniqueCards)),
    
    %% Test that all cards have valid rank and suit
    ValidRanks = [ace, seven, eight, nine, ten, jack, queen, king],
    ValidSuits = [clubs, diamonds, hearts, spades],
    
    AllValid = lists:all(fun(#{rank := Rank, suit := Suit}) ->
                                 lists:member(Rank, ValidRanks) andalso
                                 lists:member(Suit, ValidSuits)
                         end, Deck),
    ?assert(AllValid).

test_dealing() ->
    %% Test that dealing creates correct structure
    Players = [#{id => player1, name => "Player1", socket => self()},
               #{id => player2, name => "Player2", socket => self()},
               #{id => player3, name => "Player3", socket => self()}],
    
    Result = erlskat_bidding:deal(Players),
    
    %% Should have hands and skat
    ?assert(maps:is_key(hands, Result)),
    ?assert(maps:is_key(skat, Result)),
    
    %% Should have 3 hands
    Hands = maps:get(hands, Result),
    ?assertEqual(3, length(Hands)),
    
    %% Each hand should have player and hand fields
    [begin
         ?assert(maps:is_key(player, Hand)),
         ?assert(maps:is_key(hand, Hand))
     end || Hand <- Hands],
    
    %% Skat should have 2 cards
    Skat = maps:get(skat, Result),
    ?assertEqual(2, length(Skat)).

test_dealt_hands() ->
    %% Test that dealt hands have correct number of cards
    Players = [#{id => player1, name => "Player1", socket => self()},
               #{id => player2, name => "Player2", socket => self()},
               #{id => player3, name => "Player3", socket => self()}],
    
    #{hands := Hands} = erlskat_bidding:deal(Players),
    
    %% Each hand should have 10 cards
    [begin
         HandCards = maps:get(hand, Hand),
         ?assertEqual(10, length(HandCards))
     end || Hand <- Hands].

test_get_player_by_id() ->
    %% Test finding player by ID
    Players = [#{id => player1, name => "Player1", socket => self()},
               #{id => player2, name => "Player2", socket => self()},
               #{id => player3, name => "Player3", socket => self()}],
    
    Hands = [#{player => Player, hand => []} || Player <- Players],
    
    Found = erlskat_bidding:get_player_by_id(player2, Hands),
    ?assertNotEqual(undefined, Found),
    ?assertEqual(player2, maps:get(id, maps:get(player, Found))).

test_get_player_by_id_not_found() ->
    %% Test finding non-existent player
    Players = [#{id => player1, name => "Player1", socket => self()},
               #{id => player2, name => "Player2", socket => self()}],
    
    Hands = [#{player => Player, hand => []} || Player <- Players],
    
    Found = erlskat_bidding:get_player_by_id(nonexistent, Hands),
    ?assertEqual(undefined, Found).

%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

%% Helper function to create test players
create_test_players() ->
    [#{id => player1, name => "Player1", socket => self()},
     #{id => player2, name => "Player2", socket => self()},
     #{id => player3, name => "Player3", socket => self()}].

%% Helper function to create test cards
create_test_cards() ->
    [#{rank => ace, suit => clubs},
     #{rank => ten, suit => spades},
     #{rank => king, suit => hearts},
     #{rank => queen, suit => diamonds}].

%% Helper function to create a complete deck for testing
create_complete_deck() ->
    [#{rank => Rank, suit => Suit} ||
        Rank <- [ace, seven, eight, nine, ten, jack, queen, king],
        Suit <- [clubs, diamonds, hearts, spades]].

%% Helper function to verify card ordering is consistent
verify_card_ordering_consistency() ->
    Deck = create_complete_deck(),
    OrderedDeck = erlskat_bidding:order_cards_for_skat(Deck),
    
    %% All cards should be present
    ?assertEqual(32, length(OrderedDeck)),
    
    %% All cards should be unique
    UniqueCards = sets:to_list(sets:from_list(OrderedDeck)),
    ?assertEqual(32, length(UniqueCards)),
    
    %% Jacks should come first
    FirstFour = lists:sublist(OrderedDeck, 4),
    [begin
         ?assertEqual(jack, maps:get(rank, Card))
     end || Card <- FirstFour],
    
    %% Verify jack order: clubs, spades, hearts, diamonds
    ExpectedJackOrder = [clubs, spades, hearts, diamonds],
    JackSuits = [maps:get(suit, Card) || Card <- FirstFour],
    ?assertEqual(ExpectedJackOrder, JackSuits).

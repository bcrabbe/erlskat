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

game_declaration_flow_test_() ->
    [{"Initial choice prompt structure",
      fun test_initial_choice_prompt/0},
     {"Game type prompt structure",
      fun test_game_type_prompt/0},
     {"Multiplier prompt structure",
      fun test_multiplier_prompt/0},
     {"Hand game flow - no multipliers",
      fun test_hand_game_flow_no_multipliers/0},
     {"Hand game flow - with schnieder",
      fun test_hand_game_flow_with_schnieder/0},
     {"Hand game flow - with schnieder and schwartz",
      fun test_hand_game_flow_with_schnieder_schwartz/0},
     {"Hand game flow - with all multipliers",
      fun test_hand_game_flow_with_all_multipliers/0},
     {"Skat game flow - no multipliers",
      fun test_skat_game_flow_no_multipliers/0},
     {"Null game flow - with ouvert",
      fun test_null_game_flow_with_ouvert/0},
     {"Null game flow - without ouvert",
      fun test_null_game_flow_without_ouvert/0},
     {"Multiplier validation - invalid sequence",
      fun test_multiplier_validation_invalid_sequence/0},
     {"Game type validation - invalid game type",
      fun test_game_type_validation_invalid_game_type/0}].

card_reordering_test_() ->
    [{"Reorder all hands for suit game (clubs)",
      fun test_reorder_all_hands_clubs/0},
     {"Reorder all hands for suit game (spades)",
      fun test_reorder_all_hands_spades/0},
     {"Reorder all hands for suit game (hearts)",
      fun test_reorder_all_hands_hearts/0},
     {"Reorder all hands for suit game (diamonds)",
      fun test_reorder_all_hands_diamonds/0},
     {"Reorder all hands for grand game",
      fun test_reorder_all_hands_grand/0},
     {"Reorder all hands for null game",
      fun test_reorder_all_hands_null/0},
     {"Reorder preserves hand structure",
      fun test_reorder_preserves_structure/0},
     {"Reorder with empty hands",
      fun test_reorder_empty_hands/0},
     {"Order cards for specific game types",
      fun test_order_cards_for_game_type/0}].

error_handling_test_() ->
    [{"Unexpected message format in bidding phase returns error",
      fun test_unexpected_message_bidding_phase/0},
     {"Unexpected message format in game declaration returns error",
      fun test_unexpected_message_game_declaration/0},
     {"Unexpected message format in skat exchange returns error",
      fun test_unexpected_message_skat_exchange/0},
     {"Unexpected message format in game type selection returns error",
      fun test_unexpected_message_game_type_selection/0},
     {"Unexpected message format in multiplier selection returns error",
      fun test_unexpected_message_multiplier_selection/0},
     {"Unexpected message format in completed state returns error",
      fun test_unexpected_message_completed/0}].

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

    Ordered = erlskat_card_ordering:order_cards_for_skat(Cards),

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

    Ordered = erlskat_card_ordering:order_cards_for_skat(Cards),

    %% Ace should come before seven in clubs
    ExpectedOrder = [#{rank => ace, suit => clubs},
                     #{rank => seven, suit => clubs}],

    ?assertEqual(ExpectedOrder, Ordered).

test_empty_ordering() ->
    %% Test ordering with empty list
    Ordered = erlskat_card_ordering:order_cards_for_skat([]),
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

test_initial_choice_prompt() ->
    %% Test that initial choice prompt has correct structure
    Player = #{id => player1, name => "Player1", socket => self()},
    Hand = #{player => Player, hand => []},

    %% Test that the function can be called without error
    try
        erlskat_bidding:send_initial_choice_prompt_to_player(Hand),
        ?assert(true)
    catch
        _:_ -> ?assert(false)
    end.

test_game_type_prompt() ->
    %% Test that game type prompt has correct structure
    Player = #{id => player1, name => "Player1", socket => self()},
    Hand = #{player => Player, hand => []},
    GameTypes = [grand, clubs, spades],

    %% Test that the function can be called without error
    try
        erlskat_bidding:send_game_type_prompt_to_player(Hand, GameTypes, []),
        ?assert(true)
    catch
        _:_ -> ?assert(false)
    end.

test_multiplier_prompt() ->
    %% Test that multiplier prompt has correct structure
    Player = #{id => player1, name => "Player1", socket => self()},
    Hand = #{player => Player, hand => []},
    Multipliers = [<<"schnieder">>, <<"schwartz">>],
    GameType = grand,

    %% Test that the function can be called without error
    try
        erlskat_bidding:send_multiplier_prompt_to_player(Hand, Multipliers, GameType),
        ?assert(true)
    catch
        _:_ -> ?assert(false)
    end.

test_hand_game_flow_no_multipliers() ->
    %% Test hand game flow without any multipliers
    Players = create_test_players(),
    Hands = [#{player => Player, hand => create_test_cards()} || Player <- Players],
    Skat = create_test_cards(),

    %% Initial data setup
    Data = #{hands => Hands,
             skat => Skat,
             bid => 20,
             coordinator_pid => self(),
             current_bidder => player1,
             responding_player => player2,
             passed_players => [],
             bidding_order => [player1, player2, player3],
             highest_bidder => player1,
             game_declaration_step => initial_choice},

    %% Test initial choice -> hand
    Result1 = erlskat_bidding:game_declaration(cast,
        {socket_request, #{player => #{id => player1},
                          msg => <<"hand">>}},
        Data),

    %% Should transition to game_type_selection with is_hand_game = true
    case Result1 of
        {next_state, game_type_selection, NewData1} ->
            ?assert(maps:get(is_hand_game, NewData1)),
            ?assertEqual(game_type_choice, maps:get(game_declaration_step, NewData1)),

            %% Test game type selection -> grand
            Result2 = erlskat_bidding:game_type_selection(cast,
                {socket_request, #{player => #{id => player1},
                                  msg => <<"grand">>}},
                NewData1),

            %% Should transition to multiplier_selection
            case Result2 of
                {next_state, multiplier_selection, NewData2} ->
                    ?assertEqual(grand, maps:get(chosen_game, NewData2)),
                    ?assertEqual(multiplier_choice, maps:get(game_declaration_step, NewData2)),

                    %% Test skip multipliers
                    Result3 = erlskat_bidding:multiplier_selection(cast,
                        {socket_request, #{player => #{id => player1},
                                          msg => <<"skip">>}},
                        NewData2),

                    %% Should complete bidding (hand game)
                    case Result3 of
                        {next_state, completed, FinalData, _Timeout} ->
                            ?assertEqual(grand, maps:get(chosen_game, FinalData)),
                            ?assertEqual([], maps:get(selected_multipliers, FinalData, [])),
                            ?assert(maps:get(is_hand_game, FinalData));
                        _ ->
                            ?assert(false)
                    end;
                _ ->
                    ?assert(false)
            end;
        _ ->
            ?assert(false)
    end.

test_hand_game_flow_with_schnieder() ->
    %% Test hand game flow with schnieder multiplier
    Players = create_test_players(),
    Hands = [#{player => Player, hand => create_test_cards()} || Player <- Players],
    Skat = create_test_cards(),

    %% Initial data setup
    Data = #{hands => Hands,
             skat => Skat,
             bid => 20,
             coordinator_pid => self(),
             current_bidder => player1,
             responding_player => player2,
             passed_players => [],
             bidding_order => [player1, player2, player3],
             highest_bidder => player1,
             game_declaration_step => initial_choice},

    %% Test initial choice -> hand
    {next_state, game_type_selection, Data1} = erlskat_bidding:game_declaration(cast,
        {socket_request, #{player => #{id => player1},
                          msg => <<"hand">>}},
        Data),

    %% Test game type selection -> clubs
    {next_state, multiplier_selection, Data2} = erlskat_bidding:game_type_selection(cast,
        {socket_request, #{player => #{id => player1},
                          msg => <<"clubs">>}},
        Data1),

    %% Test schnieder selection
    {keep_state, Data3} = erlskat_bidding:multiplier_selection(cast,
        {socket_request, #{player => #{id => player1},
                          msg => <<"schnieder">>}},
        Data2),

    %% Should have schnieder in selected_multipliers
    ?assert(lists:member(schnieder, maps:get(selected_multipliers, Data3))),

    %% Test skip remaining multipliers
    {next_state, completed, FinalData, _Timeout} = erlskat_bidding:multiplier_selection(cast,
        {socket_request, #{player => #{id => player1},
                          msg => <<"skip">>}},
        Data3),

    %% Should complete with schnieder
    ?assertEqual(clubs, maps:get(chosen_game, FinalData)),
    ?assert(lists:member(schnieder, maps:get(selected_multipliers, FinalData))),
    ?assert(maps:get(is_hand_game, FinalData)).

test_hand_game_flow_with_schnieder_schwartz() ->
    %% Test hand game flow with schnieder and schwartz multipliers
    Players = create_test_players(),
    Hands = [#{player => Player, hand => create_test_cards()} || Player <- Players],
    Skat = create_test_cards(),

    %% Initial data setup
    Data = #{hands => Hands,
             skat => Skat,
             bid => 20,
             coordinator_pid => self(),
             current_bidder => player1,
             responding_player => player2,
             passed_players => [],
             bidding_order => [player1, player2, player3],
             highest_bidder => player1,
             game_declaration_step => initial_choice},

    %% Test initial choice -> hand
    {next_state, game_type_selection, Data1} = erlskat_bidding:game_declaration(
                                                 cast,
                                                 {socket_request,
                                                  #{player => #{id => player1},
                                                    msg => <<"hand">>}},
                                                 Data),

    %% Test game type selection -> spades
    {next_state, multiplier_selection, Data2} = erlskat_bidding:game_type_selection(
                                                  cast,
                                                  {socket_request,
                                                   #{player => #{id => player1},
                                                     msg => <<"spades">>}},
                                                  Data1),

    %% Test schnieder selection
    {keep_state, Data3} = erlskat_bidding:multiplier_selection(
                            cast,
                            {socket_request,
                             #{player => #{id => player1},
                               msg => <<"schnieder">>}},
                            Data2),

    %% Test schwartz selection
    {keep_state, Data4} = erlskat_bidding:multiplier_selection(
                            cast,
                            {socket_request,
                             #{player => #{id => player1},
                               msg => <<"schwartz">>}},
                            Data3),

    %% Should have both schnieder and schwartz
    ?assert(lists:member(schnieder, maps:get(selected_multipliers, Data4))),
    ?assert(lists:member(schwartz, maps:get(selected_multipliers, Data4))),

    %% Test skip remaining multipliers
    {next_state, completed, FinalData, _Timeout} = erlskat_bidding:multiplier_selection(cast,
        {socket_request, #{player => #{id => player1}, msg => <<"skip">>}},
        Data4),

    %% Should complete with both multipliers
    ?assertEqual(spades, maps:get(chosen_game, FinalData)),
    ?assert(lists:member(schnieder, maps:get(selected_multipliers, FinalData))),
    ?assert(lists:member(schwartz, maps:get(selected_multipliers, FinalData))),
    ?assert(maps:get(is_hand_game, FinalData)).

test_hand_game_flow_with_all_multipliers() ->
    %% Test hand game flow with all multipliers (schnieder, schwartz, ouvert)
    Players = create_test_players(),
    Hands = [#{player => Player, hand => create_test_cards()} || Player <- Players],
    Skat = create_test_cards(),

    %% Initial data setup
    Data = #{hands => Hands,
             skat => Skat,
             bid => 20,
             coordinator_pid => self(),
             current_bidder => player1,
             responding_player => player2,
             passed_players => [],
             bidding_order => [player1, player2, player3],
             highest_bidder => player1,
             game_declaration_step => initial_choice},

    %% Test initial choice -> hand
    {next_state, game_type_selection, Data1} = erlskat_bidding:game_declaration(cast,
        {socket_request, #{player => #{id => player1}, msg => <<"hand">>}},
        Data),

    %% Test game type selection -> hearts
    {next_state, multiplier_selection, Data2} = erlskat_bidding:game_type_selection(cast,
        {socket_request, #{player => #{id => player1}, msg => <<"hearts">>}},
        Data1),

    %% Test schnieder selection
    {keep_state, Data3} = erlskat_bidding:multiplier_selection(cast,
        {socket_request, #{player => #{id => player1}, msg => <<"schnieder">>}},
        Data2),

    %% Test schwartz selection
    {keep_state, Data4} = erlskat_bidding:multiplier_selection(cast,
        {socket_request, #{player => #{id => player1}, msg => <<"schwartz">>}},
        Data3),

    %% Test ouvert selection (should be available after schwartz)
    {next_state, completed, FinalData, _Timeout} = erlskat_bidding:multiplier_selection(cast,
        {socket_request, #{player => #{id => player1}, msg => <<"ouvert">>}},
        Data4),

    %% Should complete with all multipliers
    ?assertEqual(hearts, maps:get(chosen_game, FinalData)),
    ?assert(lists:member(schnieder, maps:get(selected_multipliers, FinalData))),
    ?assert(lists:member(schwartz, maps:get(selected_multipliers, FinalData))),
    ?assert(lists:member(ouvert, maps:get(selected_multipliers, FinalData))),
    ?assert(maps:get(is_hand_game, FinalData)).

test_skat_game_flow_no_multipliers() ->
    %% Test skat game flow without multipliers
    Players = create_test_players(),
    Hands = [#{player => Player, hand => create_test_cards()} || Player <- Players],
    Skat = create_test_cards(),

    %% Initial data setup
    Data = #{hands => Hands,
             skat => Skat,
             bid => 20,
             coordinator_pid => self(),
             current_bidder => player1,
             responding_player => player2,
             passed_players => [],
             bidding_order => [player1, player2, player3],
             highest_bidder => player1,
             game_declaration_step => initial_choice},

    %% Test initial choice -> skat
    {next_state, game_type_selection, Data1} = erlskat_bidding:game_declaration(cast,
        {socket_request, #{player => #{id => player1}, msg => <<"skat">>}},
        Data),

    %% Should have is_hand_game = false
    ?assertNot(maps:get(is_hand_game, Data1)),

    %% Test game type selection -> diamonds
    {next_state, skat_exchange, Data2} = erlskat_bidding:game_type_selection(cast,
        {socket_request, #{player => #{id => player1}, msg => <<"diamonds">>}},
        Data1),

    %% Should transition to skat_exchange (not hand game)
    ?assertEqual(diamonds, maps:get(chosen_game, Data2)),
    ?assertNot(maps:get(is_hand_game, Data2, false)).

test_null_game_flow_with_ouvert() ->
    %% Test null game flow with ouvert
    Players = create_test_players(),
    Hands = [#{player => Player, hand => create_test_cards()} || Player <- Players],
    Skat = create_test_cards(),

    %% Initial data setup
    Data = #{hands => Hands,
             skat => Skat,
             bid => 20,
             coordinator_pid => self(),
             current_bidder => player1,
             responding_player => player2,
             passed_players => [],
             bidding_order => [player1, player2, player3],
             highest_bidder => player1,
             game_declaration_step => initial_choice},

    %% Test initial choice -> hand
    {next_state, game_type_selection, Data1} = erlskat_bidding:game_declaration(cast,
        {socket_request, #{player => #{id => player1}, msg => <<"hand">>}},
        Data),

    %% Test game type selection -> null
    {next_state, multiplier_selection, Data2} = erlskat_bidding:game_type_selection(cast,
        {socket_request, #{player => #{id => player1}, msg => <<"null">>}},
        Data1),

    %% Test ouvert selection (should be available for null games)
    {next_state, completed, FinalData, _Timeout} = erlskat_bidding:multiplier_selection(cast,
        {socket_request, #{player => #{id => player1}, msg => <<"ouvert">>}},
        Data2),

    %% Should complete with ouvert
    ?assertEqual(null, maps:get(chosen_game, FinalData)),
    ?assert(lists:member(ouvert, maps:get(selected_multipliers, FinalData))),
    ?assert(maps:get(is_hand_game, FinalData)).

test_null_game_flow_without_ouvert() ->
    %% Test null game flow without ouvert
    Players = create_test_players(),
    Hands = [#{player => Player, hand => create_test_cards()} || Player <- Players],
    Skat = create_test_cards(),

    %% Initial data setup
    Data = #{hands => Hands,
             skat => Skat,
             bid => 20,
             coordinator_pid => self(),
             current_bidder => player1,
             responding_player => player2,
             passed_players => [],
             bidding_order => [player1, player2, player3],
             highest_bidder => player1,
             game_declaration_step => initial_choice},

    %% Test initial choice -> hand
    {next_state, game_type_selection, Data1} = erlskat_bidding:game_declaration(cast,
        {socket_request, #{player => #{id => player1}, msg => <<"hand">>}},
        Data),

    %% Test game type selection -> null
    {next_state, multiplier_selection, Data2} = erlskat_bidding:game_type_selection(cast,
        {socket_request, #{player => #{id => player1}, msg => <<"null">>}},
        Data1),

    %% Test skip ouvert
    {next_state, completed, FinalData, _Timeout} = erlskat_bidding:multiplier_selection(cast,
        {socket_request, #{player => #{id => player1}, msg => <<"skip">>}},
        Data2),

    %% Should complete without ouvert
    ?assertEqual(null, maps:get(chosen_game, FinalData)),
    ?assertEqual([], maps:get(selected_multipliers, FinalData, [])),
    ?assert(maps:get(is_hand_game, FinalData)).

test_multiplier_validation_invalid_sequence() ->
    %% Test that invalid multiplier sequences are rejected
    Players = create_test_players(),
    Hands = [#{player => Player, hand => create_test_cards()} || Player <- Players],
    Skat = create_test_cards(),

    %% Initial data setup
    Data = #{hands => Hands,
             skat => Skat,
             bid => 20,
             coordinator_pid => self(),
             current_bidder => player1,
             responding_player => player2,
             passed_players => [],
             bidding_order => [player1, player2, player3],
             highest_bidder => player1,
             game_declaration_step => initial_choice},

    %% Test initial choice -> hand
    {next_state, game_type_selection, Data1} = erlskat_bidding:game_declaration(cast,
        {socket_request, #{player => #{id => player1}, msg => <<"hand">>}},
        Data),

    %% Test game type selection -> grand
    {next_state, multiplier_selection, Data2} = erlskat_bidding:game_type_selection(cast,
        {socket_request, #{player => #{id => player1}, msg => <<"grand">>}},
        Data1),

    %% Test invalid sequence: try ouvert without schwartz first
    Result = erlskat_bidding:multiplier_selection(cast,
        {socket_request, #{player => #{id => player1}, msg => <<"ouvert">>}},
        Data2),

    %% Should stay in same state (invalid sequence)
    ?assertEqual(keep_state_and_data, Result).

test_game_type_validation_invalid_game_type() ->
    %% Test that invalid game types are rejected
    Players = create_test_players(),
    Hands = [#{player => Player, hand => create_test_cards()} || Player <- Players],
    Skat = create_test_cards(),

    %% Initial data setup
    Data = #{hands => Hands,
             skat => Skat,
             bid => 20,
             coordinator_pid => self(),
             current_bidder => player1,
             responding_player => player2,
             passed_players => [],
             bidding_order => [player1, player2, player3],
             highest_bidder => player1,
             game_declaration_step => initial_choice},

    %% Test initial choice -> hand
    {next_state, game_type_selection, Data1} = erlskat_bidding:game_declaration(cast,
        {socket_request, #{player => #{id => player1}, msg => <<"hand">>}},
        Data),

    %% Test invalid game type
    Result = erlskat_bidding:game_type_selection(cast,
        {socket_request, #{player => #{id => player1}, msg => <<"invalid_game">>}},
        Data1),

    %% Should stay in same state (invalid game type)
    ?assertEqual(keep_state_and_data, Result).

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
    OrderedDeck = erlskat_card_ordering:order_cards_for_skat(Deck),

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

%% Helper function to flush all messages from the test process mailbox
flush_messages() ->
    receive
        _ -> flush_messages()
    after 0 ->
        ok
    end.

%% Helper function to get the position of a card in a list (1-indexed)
get_card_position(Card, CardList) ->
    get_card_position_helper(Card, CardList, 1).

get_card_position_helper(_, [], _) ->
    not_found;
get_card_position_helper(Card, [Card | _], Pos) ->
    Pos;
get_card_position_helper(Card, [_ | Rest], Pos) ->
    get_card_position_helper(Card, Rest, Pos + 1).

%%%%%%%%%%%%%%%%%%%%%%
%%% CARD REORDERING TESTS %%%
%%%%%%%%%%%%%%%%%%%%%%

test_reorder_all_hands_clubs() ->
    %% Test reordering hands for clubs suit game
    Players = create_test_players(),
    TestCards = [#{rank => ace, suit => clubs},      % Trump (after jacks)
                 #{rank => jack, suit => hearts},    % Trump (3rd jack)
                 #{rank => ten, suit => spades},     % Non-trump spades
                 #{rank => king, suit => diamonds},  % Non-trump diamonds
                 #{rank => queen, suit => clubs}],   % Trump (after A, 10, K of clubs)

    Hands = [#{player => Player, hand => TestCards} || Player <- Players],

    ReorderedHands = erlskat_card_ordering:reorder_all_hands_for_game_type(Hands, clubs),

    %% Check that structure is preserved
    ?assertEqual(3, length(ReorderedHands)),

    %% Check first hand ordering (jacks first, then trump suit, then others)
    FirstHand = maps:get(hand, hd(ReorderedHands)),
    ?assertEqual(5, length(FirstHand)),

    %% Jack of hearts should come before ace of clubs (trumps)
    JackHeartsPos = get_card_position(#{rank => jack, suit => hearts}, FirstHand),
    AceClubsPos = get_card_position(#{rank => ace, suit => clubs}, FirstHand),
    ?assert(JackHeartsPos < AceClubsPos),

    %% Ace of clubs should come before queen of clubs
    QueenClubsPos = get_card_position(#{rank => queen, suit => clubs}, FirstHand),
    ?assert(AceClubsPos < QueenClubsPos).

test_reorder_all_hands_spades() ->
    %% Test reordering hands for spades suit game
    Players = create_test_players(),
    TestCards = [#{rank => ace, suit => spades},     % Trump (after jacks)
                 #{rank => jack, suit => clubs},     % Trump (1st jack)
                 #{rank => ten, suit => hearts},     % Non-trump hearts
                 #{rank => king, suit => diamonds},  % Non-trump diamonds
                 #{rank => nine, suit => spades}],   % Trump (after A, 10, K, Q of spades)

    Hands = [#{player => Player, hand => TestCards} || Player <- Players],

    ReorderedHands = erlskat_card_ordering:reorder_all_hands_for_game_type(Hands, spades),

    FirstHand = maps:get(hand, hd(ReorderedHands)),

    %% Jack of clubs should be first (highest trump)
    ?assertEqual(#{rank => jack, suit => clubs}, hd(FirstHand)),

    %% Ace of spades should come before nine of spades
    AceSpadesPos = get_card_position(#{rank => ace, suit => spades}, FirstHand),
    NineSpadesPos = get_card_position(#{rank => nine, suit => spades}, FirstHand),
    ?assert(AceSpadesPos < NineSpadesPos).

test_reorder_all_hands_hearts() ->
    %% Test reordering hands for hearts suit game
    Players = create_test_players(),
    TestCards = [#{rank => jack, suit => diamonds},  % Trump (4th jack)
                 #{rank => ten, suit => hearts},     % Trump (after jacks and ace)
                 #{rank => ace, suit => clubs},      % Non-trump clubs
                 #{rank => king, suit => hearts},    % Trump (after A, 10 of hearts)
                 #{rank => seven, suit => hearts}],  % Trump (lowest)

    Hands = [#{player => Player, hand => TestCards} || Player <- Players],

    ReorderedHands = erlskat_card_ordering:reorder_all_hands_for_game_type(Hands, hearts),

    FirstHand = maps:get(hand, hd(ReorderedHands)),

    %% Jack of diamonds should come before ten of hearts
    JackDiamondsPos = get_card_position(#{rank => jack, suit => diamonds}, FirstHand),
    TenHeartsPos = get_card_position(#{rank => ten, suit => hearts}, FirstHand),
    ?assert(JackDiamondsPos < TenHeartsPos),

    %% Ten of hearts should come before king of hearts
    KingHeartsPos = get_card_position(#{rank => king, suit => hearts}, FirstHand),
    ?assert(TenHeartsPos < KingHeartsPos),

    %% Seven of hearts should be last among hearts trumps
    SevenHeartsPos = get_card_position(#{rank => seven, suit => hearts}, FirstHand),
    ?assert(KingHeartsPos < SevenHeartsPos).

test_reorder_all_hands_diamonds() ->
    %% Test reordering hands for diamonds suit game
    Players = create_test_players(),
    TestCards = [#{rank => jack, suit => spades},    % Trump (2nd jack)
                 #{rank => queen, suit => diamonds}, % Trump (after A, 10, K of diamonds)
                 #{rank => ace, suit => hearts},     % Non-trump hearts
                 #{rank => eight, suit => diamonds}, % Trump (after Q, 9 of diamonds)
                 #{rank => king, suit => clubs}],    % Non-trump clubs

    Hands = [#{player => Player, hand => TestCards} || Player <- Players],

    ReorderedHands = erlskat_card_ordering:reorder_all_hands_for_game_type(Hands, diamonds),

    FirstHand = maps:get(hand, hd(ReorderedHands)),

    %% Jack of spades should come first (trump)
    JackSpadesPos = get_card_position(#{rank => jack, suit => spades}, FirstHand),
    QueenDiamondsPos = get_card_position(#{rank => queen, suit => diamonds}, FirstHand),
    ?assert(JackSpadesPos < QueenDiamondsPos),

    %% Queen of diamonds should come before eight of diamonds
    EightDiamondsPos = get_card_position(#{rank => eight, suit => diamonds}, FirstHand),
    ?assert(QueenDiamondsPos < EightDiamondsPos).

test_reorder_all_hands_grand() ->
    %% Test reordering hands for grand game (only jacks are trumps)
    Players = create_test_players(),
    TestCards = [#{rank => jack, suit => clubs},     % Trump (highest)
                 #{rank => ace, suit => clubs},      % Non-trump (highest in clubs)
                 #{rank => jack, suit => hearts},    % Trump (3rd jack)
                 #{rank => ten, suit => clubs},      % Non-trump (2nd in clubs)
                 #{rank => king, suit => spades}],   % Non-trump spades

    Hands = [#{player => Player, hand => TestCards} || Player <- Players],

    ReorderedHands = erlskat_card_ordering:reorder_all_hands_for_game_type(Hands, grand),

    FirstHand = maps:get(hand, hd(ReorderedHands)),

    %% Jack of clubs should be first (highest trump)
    ?assertEqual(#{rank => jack, suit => clubs}, hd(FirstHand)),

    %% Jack of hearts should come before ace of clubs
    JackHeartsPos = get_card_position(#{rank => jack, suit => hearts}, FirstHand),
    AceClubsPos = get_card_position(#{rank => ace, suit => clubs}, FirstHand),
    ?assert(JackHeartsPos < AceClubsPos),

    %% Ace of clubs should come before ten of clubs (within same suit)
    TenClubsPos = get_card_position(#{rank => ten, suit => clubs}, FirstHand),
    ?assert(AceClubsPos < TenClubsPos).

test_reorder_all_hands_null() ->
    %% Test reordering hands for null game (no trumps, jacks are regular)
    Players = create_test_players(),
    TestCards = [#{rank => jack, suit => clubs},     % Regular card (between Q and 10)
                 #{rank => ace, suit => clubs},      % Highest in clubs
                 #{rank => queen, suit => clubs},    % Higher than jack in clubs
                 #{rank => ten, suit => clubs},      % Lower than jack in clubs
                 #{rank => king, suit => spades}],   % 2nd highest in spades

    Hands = [#{player => Player, hand => TestCards} || Player <- Players],

    ReorderedHands = erlskat_card_ordering:reorder_all_hands_for_game_type(Hands, null),

    FirstHand = maps:get(hand, hd(ReorderedHands)),

    %% In null games: A > K > Q > J > 10 > 9 > 8 > 7
    AceClubsPos = get_card_position(#{rank => ace, suit => clubs}, FirstHand),
    QueenClubsPos = get_card_position(#{rank => queen, suit => clubs}, FirstHand),
    JackClubsPos = get_card_position(#{rank => jack, suit => clubs}, FirstHand),
    TenClubsPos = get_card_position(#{rank => ten, suit => clubs}, FirstHand),

    %% Verify null game ordering within clubs suit
    ?assert(AceClubsPos < QueenClubsPos),
    ?assert(QueenClubsPos < JackClubsPos),
    ?assert(JackClubsPos < TenClubsPos).

test_reorder_preserves_structure() ->
    %% Test that reordering preserves the hand structure (player info etc.)
    Players = create_test_players(),
    TestCards = create_test_cards(),

    OriginalHands = [#{player => Player, hand => TestCards, extra_field => some_value}
                     || Player <- Players],

    ReorderedHands = erlskat_card_ordering:reorder_all_hands_for_game_type(OriginalHands, clubs),

    %% Check that all non-hand fields are preserved
    [begin
         ?assert(maps:is_key(player, Hand)),
         ?assert(maps:is_key(hand, Hand)),
         ?assert(maps:is_key(extra_field, Hand)),
         ?assertEqual(some_value, maps:get(extra_field, Hand))
     end || Hand <- ReorderedHands],

    %% Check that players are preserved
    OriginalPlayerIds = [maps:get(id, maps:get(player, Hand)) || Hand <- OriginalHands],
    ReorderedPlayerIds = [maps:get(id, maps:get(player, Hand)) || Hand <- ReorderedHands],
    ?assertEqual(lists:sort(OriginalPlayerIds), lists:sort(ReorderedPlayerIds)).

test_reorder_empty_hands() ->
    %% Test reordering with empty hands
    Players = create_test_players(),
    EmptyHands = [#{player => Player, hand => []} || Player <- Players],

    ReorderedHands = erlskat_card_ordering:reorder_all_hands_for_game_type(EmptyHands, grand),

    %% Check that structure is preserved
    ?assertEqual(3, length(ReorderedHands)),

    %% Check that hands remain empty
    [begin
         Hand = maps:get(hand, PlayerHand),
         ?assertEqual([], Hand)
     end || PlayerHand <- ReorderedHands].

test_order_cards_for_game_type() ->
    %% Test the underlying order_cards_for_game_type function directly
    %% Include jack, 10, and king of each suit for comprehensive testing
    TestCards = [#{rank => ten, suit => clubs},
                 #{rank => jack, suit => clubs},
                 #{rank => king, suit => clubs},
                 #{rank => ten, suit => spades},
                 #{rank => jack, suit => spades},
                 #{rank => king, suit => spades},
                 #{rank => ten, suit => hearts},
                 #{rank => jack, suit => hearts},
                 #{rank => king, suit => hearts},
                 #{rank => ten, suit => diamonds},
                 #{rank => jack, suit => diamonds},
                 #{rank => king, suit => diamonds}],

    %% Test clubs game ordering
    ClubsOrdered = erlskat_card_ordering:order_cards_for_game_type(TestCards, clubs),
    ?assertEqual(12, length(ClubsOrdered)),

    %% In clubs game: Jacks first (C♣ > J♠ > J♥ > J♦), then trump clubs (A, 10, K, Q, 9, 8, 7), then other suits
    %% First 4 should be jacks in order: clubs, spades, hearts, diamonds
    ?assertEqual(#{rank => jack, suit => clubs}, lists:nth(1, ClubsOrdered)),
    ?assertEqual(#{rank => jack, suit => spades}, lists:nth(2, ClubsOrdered)),
    ?assertEqual(#{rank => jack, suit => hearts}, lists:nth(3, ClubsOrdered)),
    ?assertEqual(#{rank => jack, suit => diamonds}, lists:nth(4, ClubsOrdered)),

    %% Next should be clubs trump cards: 10, K (no Ace in our test set)
    ClubsTrumpPos = get_card_position(#{rank => ten, suit => clubs}, ClubsOrdered),
    ClubsKingPos = get_card_position(#{rank => king, suit => clubs}, ClubsOrdered),
    ?assert(ClubsTrumpPos < ClubsKingPos), % 10 > K in trump suit
    ?assert(ClubsTrumpPos > 4), % Should come after all jacks

    %% Test grand game ordering
    GrandOrdered = erlskat_card_ordering:order_cards_for_game_type(TestCards, grand),
    ?assertEqual(12, length(GrandOrdered)),

    %% In grand: Only jacks are trumps, then regular suit ordering
    ?assertEqual(#{rank => jack, suit => clubs}, lists:nth(1, GrandOrdered)),
    ?assertEqual(#{rank => jack, suit => spades}, lists:nth(2, GrandOrdered)),
    ?assertEqual(#{rank => jack, suit => hearts}, lists:nth(3, GrandOrdered)),
    ?assertEqual(#{rank => jack, suit => diamonds}, lists:nth(4, GrandOrdered)),

    %% After jacks, cards should be ordered by suit, then by rank within suit (A > 10 > K > Q)
    %% In Grand, jacks are trumps so they don't appear in their original suits
    %% Within each non-trump suit: 10 > K (since we don't have Ace or Queen in our test)
    ClubsTenPos = get_card_position(#{rank => ten, suit => clubs}, GrandOrdered),
    ClubsKingPosGrand = get_card_position(#{rank => king, suit => clubs}, GrandOrdered),
    ?assert(ClubsTenPos < ClubsKingPosGrand), % 10 > K in non-trump suits

    %% Test null game ordering
    NullOrdered = erlskat_card_ordering:order_cards_for_game_type(TestCards, null),
    ?assertEqual(12, length(NullOrdered)),

    %% In null games: no trumps, suits ordered clubs > spades > hearts > diamonds
    %% Within each suit: K > J > 10 (since we don't have A or Q in our test)

    %% Check clubs suit ordering: K♣ > J♣ > 10♣
    ClubsKingPosNull = get_card_position(#{rank => king, suit => clubs}, NullOrdered),
    ClubsJackPosNull = get_card_position(#{rank => jack, suit => clubs}, NullOrdered),
    ClubsTenPosNull = get_card_position(#{rank => ten, suit => clubs}, NullOrdered),
    ?assert(ClubsKingPosNull < ClubsJackPosNull),
    ?assert(ClubsJackPosNull < ClubsTenPosNull),

    %% Check that clubs cards come before spades cards
    SpadesKingPos = get_card_position(#{rank => king, suit => spades}, NullOrdered),
    ?assert(ClubsTenPosNull < SpadesKingPos), % Last clubs card before first spades card

    %% Check spades suit ordering: K♠ > J♠ > 10♠
    SpadesJackPos = get_card_position(#{rank => jack, suit => spades}, NullOrdered),
    SpadesTenPos = get_card_position(#{rank => ten, suit => spades}, NullOrdered),
    ?assert(SpadesKingPos < SpadesJackPos),
    ?assert(SpadesJackPos < SpadesTenPos).

%%%%%%%%%%%%%%%%%%%%%%
%%% ERROR HANDLING TESTS %%%
%%%%%%%%%%%%%%%%%%%%%%

%% Test for unexpected message formats that should return error messages
test_unexpected_message_bidding_phase() ->
    %% Set up mock
    TestPid = self(),
    meck:new(erlskat_manager, [unstick]),
    meck:expect(erlskat_manager, socket_response, fun(_PlayerId, Response) ->
        TestPid ! Response, ok
    end),

    %% Flush any existing messages
    flush_messages(),

    %% Set up test data for bidding phase
    Players = create_test_players(),
    Hands = [#{player => Player, hand => create_test_cards()} || Player <- Players],

    Data = #{hands => Hands,
             skat => create_test_cards(),
             bid => 20,
             coordinator_pid => self(),
             current_bidder => player1,
             responding_player => player2,
             passed_players => [],
             bidding_order => [player1, player2, player3],
             highest_bidder => player1},

    %% Test with invalid message format (should be "hold" or "pass")
    Result = erlskat_bidding:bidding_phase(cast,
        {socket_request, #{player => #{id => player1, socket => self()},
                          msg => <<"invalid_bid_message">>}},
        Data),

    %% Should keep state and send error message
    ?assertEqual(keep_state_and_data, Result),

    %% Check that we received an error message
    receive
        ErrorMsg ->
            ?assertEqual(error, maps:get(type, ErrorMsg)),
            ?assert(maps:is_key(message, ErrorMsg)),
            ?assert(maps:is_key(expected_format, ErrorMsg)),
            ExpectedFormat = maps:get(expected_format, ErrorMsg),
            ?assert(maps:is_key(<<"hold">>, ExpectedFormat)),
            ?assert(maps:is_key(<<"pass">>, ExpectedFormat))
    after 100 ->
        ?assert(false) % Should have received an error message
    end,

    %% Cleanup
    meck:unload(erlskat_manager).

test_unexpected_message_game_declaration() ->
    %% Set up mock
    TestPid = self(),
    meck:new(erlskat_manager, [unstick]),
    meck:expect(erlskat_manager, socket_response, fun(_PlayerId, Response) ->
        TestPid ! Response, ok
    end),

    %% Flush any existing messages
    flush_messages(),

    %% Set up test data for game declaration phase
    Players = create_test_players(),
    Hands = [#{player => Player, hand => create_test_cards()} || Player <- Players],

    Data = #{hands => Hands,
             skat => create_test_cards(),
             bid => 20,
             coordinator_pid => self(),
             current_bidder => player1,
             responding_player => player2,
             passed_players => [],
             bidding_order => [player1, player2, player3],
             highest_bidder => player1,
             game_declaration_step => initial_choice},

    %% Test with invalid message format (should be "hand" or "skat")
    Result = erlskat_bidding:game_declaration(cast,
        {socket_request, #{player => #{id => player1, socket => self()},
                          msg => <<"invalid_choice">>}},
        Data),

    %% Should keep state and send error message
    ?assertEqual(keep_state_and_data, Result),

    %% Check that we received an error message
    receive
        ErrorMsg ->
            ?assertEqual(error, maps:get(type, ErrorMsg)),
            ?assert(maps:is_key(message, ErrorMsg)),
            ?assert(maps:is_key(expected_format, ErrorMsg))
    after 100 ->
        ?assert(false) % Should have received an error message
    end.

test_unexpected_message_skat_exchange() ->
    %% Flush any existing messages
    flush_messages(),

    %% Set up test data for skat exchange phase
    Players = create_test_players(),
    Hands = [#{player => Player, hand => create_test_cards()} || Player <- Players],

    Data = #{hands => Hands,
             skat => create_test_cards(),
             bid => 20,
             coordinator_pid => self(),
             current_bidder => player1,
             responding_player => player2,
             passed_players => [],
             bidding_order => [player1, player2, player3],
             highest_bidder => player1,
             chosen_game => clubs,
             is_hand_game => false},

    %% Test with invalid message format (should be array of indices)
    Result = erlskat_bidding:skat_exchange(cast,
        {socket_request, #{player => #{id => player1, socket => self()},
                          msg => <<"invalid_discard">>}},
        Data),

    %% Should keep state and send error message
    ?assertEqual(keep_state_and_data, Result),

    %% Check that we received an error message
    receive
        ErrorMsg ->
            ?assertEqual(error, maps:get(type, ErrorMsg)),
            ?assert(maps:is_key(message, ErrorMsg)),
            ?assert(maps:is_key(expected_format, ErrorMsg)),
            ExpectedFormat = maps:get(expected_format, ErrorMsg),
            ?assertEqual(<<"Array of 2 card indices to discard">>, ExpectedFormat)
    after 100 ->
        ?assert(false) % Should have received an error message
    end.

test_unexpected_message_game_type_selection() ->
    %% Flush any existing messages
    flush_messages(),

    %% Set up test data for game type selection phase
    Players = create_test_players(),
    Hands = [#{player => Player, hand => create_test_cards()} || Player <- Players],

    Data = #{hands => Hands,
             skat => create_test_cards(),
             bid => 20,
             coordinator_pid => self(),
             current_bidder => player1,
             responding_player => player2,
             passed_players => [],
             bidding_order => [player1, player2, player3],
             highest_bidder => player1,
             game_declaration_step => game_type_choice,
             is_hand_game => true},

    %% Test with invalid message format (should be valid game type)
    Result = erlskat_bidding:game_type_selection(cast,
        {socket_request, #{player => #{id => player1, socket => self()},
                          msg => <<"invalid_game">>}},
        Data),

    %% Should keep state and send error message
    ?assertEqual(keep_state_and_data, Result),

    %% Check that we received an error message
    receive
        ErrorMsg ->
            ?assertEqual(error, maps:get(type, ErrorMsg)),
            ?assert(maps:is_key(message, ErrorMsg)),
            ?assert(maps:is_key(expected_format, ErrorMsg)),
            ExpectedFormat = maps:get(expected_format, ErrorMsg),
            ?assert(is_list(ExpectedFormat)),
            ?assert(lists:member(<<"grand">>, ExpectedFormat))
    after 100 ->
        ?assert(false) % Should have received an error message
    end.

test_unexpected_message_multiplier_selection() ->
    %% Flush any existing messages
    flush_messages(),

    %% Set up test data for multiplier selection phase
    Players = create_test_players(),
    Hands = [#{player => Player, hand => create_test_cards()} || Player <- Players],

    Data = #{hands => Hands,
             skat => create_test_cards(),
             bid => 20,
             coordinator_pid => self(),
             current_bidder => player1,
             responding_player => player2,
             passed_players => [],
             bidding_order => [player1, player2, player3],
             highest_bidder => player1,
             game_declaration_step => multiplier_choice,
             chosen_game => clubs,
             is_hand_game => true,
             selected_multipliers => []},

    %% Test with invalid message format (should be valid multiplier or "skip")
    Result = erlskat_bidding:multiplier_selection(cast,
        {socket_request, #{player => #{id => player1, socket => self()},
                          msg => <<"invalid_multiplier">>}},
        Data),

    %% Should keep state and send error message
    ?assertEqual(keep_state_and_data, Result),

    %% Check that we received an error message
    receive
        ErrorMsg ->
            ?assertEqual(error, maps:get(type, ErrorMsg)),
            ?assert(maps:is_key(message, ErrorMsg)),
            ?assert(maps:is_key(expected_format, ErrorMsg)),
            ExpectedFormat = maps:get(expected_format, ErrorMsg),
            ?assert(is_list(ExpectedFormat)),
            ?assert(lists:member(<<"schnieder">>, ExpectedFormat)),
            ?assert(lists:member(<<"skip">>, ExpectedFormat))
    after 100 ->
        ?assert(false) % Should have received an error message
    end.

test_unexpected_message_completed() ->
    %% Flush any existing messages
    flush_messages(),

    %% Set up test data for completed phase
    Players = create_test_players(),
    Hands = [#{player => Player, hand => create_test_cards()} || Player <- Players],

    Data = #{hands => Hands,
             skat => create_test_cards(),
             bid => 20,
             coordinator_pid => self(),
             current_bidder => player1,
             responding_player => player2,
             passed_players => [],
             bidding_order => [player1, player2, player3],
             highest_bidder => player1,
             chosen_game => clubs,
             is_hand_game => true,
             selected_multipliers => []},

    %% Test with any message (no messages should be accepted in completed state)
    Result = erlskat_bidding:completed(cast,
        {socket_request, #{player => #{id => player1, socket => self()},
                          msg => <<"any_message">>}},
        Data),

    %% Should keep state and send error message
    ?assertEqual(keep_state_and_data, Result),

    %% Check that we received an error message
    receive
        ErrorMsg ->
            ?assertEqual(error, maps:get(type, ErrorMsg)),
            ?assert(maps:is_key(message, ErrorMsg)),
            ?assert(maps:is_key(expected_format, ErrorMsg)),
            ExpectedFormat = maps:get(expected_format, ErrorMsg),
            ?assert(maps:is_key(<<"message">>, ExpectedFormat))
    after 100 ->
        ?assert(false) % Should have received an error message
    end.

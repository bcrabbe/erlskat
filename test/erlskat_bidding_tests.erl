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

error_handling_test_() ->
    [{"Expected message format for bidding phase",
      fun test_expected_message_format_bidding_phase/0},
     {"Expected message format for game declaration",
      fun test_expected_message_format_game_declaration/0},
     {"Expected message format for skat exchange",
      fun test_expected_message_format_skat_exchange/0},
     {"Expected message format for game type selection",
      fun test_expected_message_format_game_type_selection/0},
     {"Expected message format for multiplier selection",
      fun test_expected_message_format_multiplier_selection/0},
     {"Expected message format for completed state",
      fun test_expected_message_format_completed/0}].

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
    GameTypes = [<<"grand">>, <<"clubs">>, <<"spades">>],

    %% Test that the function can be called without error
    try
        erlskat_bidding:send_game_type_prompt_to_player(Hand, GameTypes),
        ?assert(true)
    catch
        _:_ -> ?assert(false)
    end.

test_multiplier_prompt() ->
    %% Test that multiplier prompt has correct structure
    Player = #{id => player1, name => "Player1", socket => self()},
    Hand = #{player => Player, hand => []},
    Multipliers = [<<"schnieder">>, <<"schwartz">>],
    GameType = <<"grand">>,

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
        {socket_message, #{player => #{id => player1},
                          msg => #{<<"initial_choice">> => <<"hand">>}}},
        Data),

    %% Should transition to game_type_selection with is_hand_game = true
    case Result1 of
        {next_state, game_type_selection, NewData1} ->
            ?assert(maps:get(is_hand_game, NewData1)),
            ?assertEqual(game_type_choice, maps:get(game_declaration_step, NewData1)),

            %% Test game type selection -> grand
            Result2 = erlskat_bidding:game_type_selection(cast,
                {socket_message, #{player => #{id => player1},
                                  msg => #{<<"game_type">> => <<"grand">>}}},
                NewData1),

            %% Should transition to multiplier_selection
            case Result2 of
                {next_state, multiplier_selection, NewData2} ->
                    ?assertEqual(<<"grand">>, maps:get(chosen_game, NewData2)),
                    ?assertEqual(multiplier_choice, maps:get(game_declaration_step, NewData2)),

                    %% Test skip multipliers
                    Result3 = erlskat_bidding:multiplier_selection(cast,
                        {socket_message, #{player => #{id => player1},
                                          msg => <<"skip">>}},
                        NewData2),

                    %% Should complete bidding (hand game)
                    case Result3 of
                        {next_state, completed, FinalData} ->
                            ?assertEqual(<<"grand">>, maps:get(chosen_game, FinalData)),
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
        {socket_message, #{player => #{id => player1},
                          msg => #{<<"initial_choice">> => <<"hand">>}}},
        Data),

    %% Test game type selection -> clubs
    {next_state, multiplier_selection, Data2} = erlskat_bidding:game_type_selection(cast,
        {socket_message, #{player => #{id => player1},
                          msg => #{<<"game_type">> => <<"clubs">>}}},
        Data1),

    %% Test schnieder selection
    {keep_state, Data3} = erlskat_bidding:multiplier_selection(cast,
        {socket_message, #{player => #{id => player1},
                          msg => #{<<"multiplier">> => <<"schnieder">>}}},
        Data2),

    %% Should have schnieder in selected_multipliers
    ?assert(lists:member(schnieder, maps:get(selected_multipliers, Data3))),

    %% Test skip remaining multipliers
    {next_state, completed, FinalData} = erlskat_bidding:multiplier_selection(cast,
        {socket_message, #{player => #{id => player1},
                          msg => <<"skip">>}},
        Data3),

    %% Should complete with schnieder
    ?assertEqual(<<"clubs">>, maps:get(chosen_game, FinalData)),
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
                                                 {socket_message,
                                                  #{player => #{id => player1},
                                                    msg => #{<<"initial_choice">> => <<"hand">>}}},
                                                 Data),

    %% Test game type selection -> spades
    {next_state, multiplier_selection, Data2} = erlskat_bidding:game_type_selection(
                                                  cast,
                                                  {socket_message,
                                                   #{player => #{id => player1},
                                                     msg => #{<<"game_type">> => <<"spades">>}}},
                                                  Data1),

    %% Test schnieder selection
    {keep_state, Data3} = erlskat_bidding:multiplier_selection(
                            cast,
                            {socket_message,
                             #{player => #{id => player1},
                               msg => #{<<"multiplier">> => <<"schnieder">>}}},
                            Data2),

    %% Test schwartz selection
    {keep_state, Data4} = erlskat_bidding:multiplier_selection(
                            cast,
                            {socket_message,
                             #{player => #{id => player1},
                               msg => #{<<"multiplier">> => <<"schwartz">>}}},
                            Data3),

    %% Should have both schnieder and schwartz
    ?assert(lists:member(schnieder, maps:get(selected_multipliers, Data4))),
    ?assert(lists:member(schwartz, maps:get(selected_multipliers, Data4))),

    %% Test skip remaining multipliers
    {next_state, completed, FinalData} = erlskat_bidding:multiplier_selection(cast,
        {socket_message, #{player => #{id => player1}, msg => <<"skip">>}},
        Data4),

    %% Should complete with both multipliers
    ?assertEqual(<<"spades">>, maps:get(chosen_game, FinalData)),
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
        {socket_message, #{player => #{id => player1}, msg => #{<<"initial_choice">> => <<"hand">>}}},
        Data),

    %% Test game type selection -> hearts
    {next_state, multiplier_selection, Data2} = erlskat_bidding:game_type_selection(cast,
        {socket_message, #{player => #{id => player1}, msg => #{<<"game_type">> => <<"hearts">>}}},
        Data1),

    %% Test schnieder selection
    {keep_state, Data3} = erlskat_bidding:multiplier_selection(cast,
        {socket_message, #{player => #{id => player1}, msg => #{<<"multiplier">> => <<"schnieder">>}}},
        Data2),

    %% Test schwartz selection
    {keep_state, Data4} = erlskat_bidding:multiplier_selection(cast,
        {socket_message, #{player => #{id => player1}, msg => #{<<"multiplier">> => <<"schwartz">>}}},
        Data3),

    %% Test ouvert selection (should be available after schwartz)
    {next_state, completed, FinalData} = erlskat_bidding:multiplier_selection(cast,
        {socket_message, #{player => #{id => player1}, msg => #{<<"multiplier">> => <<"ouvert">>}}},
        Data4),

    %% Should complete with all multipliers
    ?assertEqual(<<"hearts">>, maps:get(chosen_game, FinalData)),
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
        {socket_message, #{player => #{id => player1}, msg => #{<<"initial_choice">> => <<"skat">>}}},
        Data),

    %% Should have is_hand_game = false
    ?assertNot(maps:get(is_hand_game, Data1)),

    %% Test game type selection -> diamonds
    {next_state, skat_exchange, Data2} = erlskat_bidding:game_type_selection(cast,
        {socket_message, #{player => #{id => player1}, msg => #{<<"game_type">> => <<"diamonds">>}}},
        Data1),

    %% Should transition to skat_exchange (not hand game)
    ?assertEqual(<<"diamonds">>, maps:get(chosen_game, Data2)),
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
        {socket_message, #{player => #{id => player1}, msg => #{<<"initial_choice">> => <<"hand">>}}},
        Data),

    %% Test game type selection -> null
    {next_state, multiplier_selection, Data2} = erlskat_bidding:game_type_selection(cast,
        {socket_message, #{player => #{id => player1}, msg => #{<<"game_type">> => <<"null">>}}},
        Data1),

    %% Test ouvert selection (should be available for null games)
    {next_state, completed, FinalData} = erlskat_bidding:multiplier_selection(cast,
        {socket_message, #{player => #{id => player1}, msg => #{<<"multiplier">> => <<"ouvert">>}}},
        Data2),

    %% Should complete with ouvert
    ?assertEqual(<<"null">>, maps:get(chosen_game, FinalData)),
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
        {socket_message, #{player => #{id => player1}, msg => #{<<"initial_choice">> => <<"hand">>}}},
        Data),

    %% Test game type selection -> null
    {next_state, multiplier_selection, Data2} = erlskat_bidding:game_type_selection(cast,
        {socket_message, #{player => #{id => player1}, msg => #{<<"game_type">> => <<"null">>}}},
        Data1),

    %% Test skip ouvert
    {next_state, completed, FinalData} = erlskat_bidding:multiplier_selection(cast,
        {socket_message, #{player => #{id => player1}, msg => <<"skip">>}},
        Data2),

    %% Should complete without ouvert
    ?assertEqual(<<"null">>, maps:get(chosen_game, FinalData)),
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
        {socket_message, #{player => #{id => player1}, msg => #{<<"initial_choice">> => <<"hand">>}}},
        Data),

    %% Test game type selection -> grand
    {next_state, multiplier_selection, Data2} = erlskat_bidding:game_type_selection(cast,
        {socket_message, #{player => #{id => player1}, msg => #{<<"game_type">> => <<"grand">>}}},
        Data1),

    %% Test invalid sequence: try ouvert without schwartz first
    Result = erlskat_bidding:multiplier_selection(cast,
        {socket_message, #{player => #{id => player1}, msg => #{<<"multiplier">> => <<"ouvert">>}}},
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
        {socket_message, #{player => #{id => player1}, msg => #{<<"initial_choice">> => <<"hand">>}}},
        Data),

    %% Test invalid game type
    Result = erlskat_bidding:game_type_selection(cast,
        {socket_message, #{player => #{id => player1}, msg => #{<<"game_type">> => <<"invalid_game">>}}},
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

%%%%%%%%%%%%%%%%%%%%%%
%%% ERROR HANDLING TESTS %%%
%%%%%%%%%%%%%%%%%%%%%%

test_expected_message_format_bidding_phase() ->
    ExpectedFormat = erlskat_bidding:get_expected_message_format(bidding_phase),
    ?assert(is_map(ExpectedFormat)),
    ?assert(maps:is_key(<<"yes">>, ExpectedFormat)),
    ?assert(maps:is_key(<<"pass">>, ExpectedFormat)),
    ?assertEqual(<<"Accept the current bid">>, maps:get(<<"yes">>, ExpectedFormat)),
    ?assertEqual(<<"Pass on the current bid">>, maps:get(<<"pass">>, ExpectedFormat)).

test_expected_message_format_game_declaration() ->
    ExpectedFormat = erlskat_bidding:get_expected_message_format(game_declaration),
    ?assert(is_map(ExpectedFormat)),
    ?assert(maps:is_key(<<"initial_choice">>, ExpectedFormat)),
    ?assertEqual([<<"hand">>, <<"skat">>], maps:get(<<"initial_choice">>, ExpectedFormat)).

test_expected_message_format_skat_exchange() ->
    ExpectedFormat = erlskat_bidding:get_expected_message_format(skat_exchange),
    ?assert(is_map(ExpectedFormat)),
    ?assert(maps:is_key(<<"discard_cards">>, ExpectedFormat)),
    ?assertEqual(<<"Array of 2 card indices to discard">>, maps:get(<<"discard_cards">>, ExpectedFormat)).

test_expected_message_format_game_type_selection() ->
    ExpectedFormat = erlskat_bidding:get_expected_message_format(game_type_selection),
    ?assert(is_map(ExpectedFormat)),
    ?assert(maps:is_key(<<"game_type">>, ExpectedFormat)),
    ExpectedGameTypes = [<<"grand">>, <<"clubs">>, <<"spades">>, <<"hearts">>, <<"diamonds">>, <<"null">>],
    ?assertEqual(ExpectedGameTypes, maps:get(<<"game_type">>, ExpectedFormat)).

test_expected_message_format_multiplier_selection() ->
    ExpectedFormat = erlskat_bidding:get_expected_message_format(multiplier_selection),
    ?assert(is_map(ExpectedFormat)),
    ?assert(maps:is_key(<<"multiplier">>, ExpectedFormat)),
    ?assert(maps:is_key(<<"skip">>, ExpectedFormat)),
    ExpectedMultipliers = [<<"schnieder">>, <<"schwartz">>, <<"ouvert">>],
    ?assertEqual(ExpectedMultipliers, maps:get(<<"multiplier">>, ExpectedFormat)),
    ?assertEqual(<<"Skip multiplier selection">>, maps:get(<<"skip">>, ExpectedFormat)).

test_expected_message_format_completed() ->
    ExpectedFormat = erlskat_bidding:get_expected_message_format(completed),
    ?assert(is_map(ExpectedFormat)),
    ?assert(maps:is_key(<<"message">>, ExpectedFormat)),
    ?assertEqual(<<"No messages expected in completed state">>, maps:get(<<"message">>, ExpectedFormat)).

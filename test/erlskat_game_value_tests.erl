-module(erlskat_game_value_tests).

-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

basic_calculations_test_() ->
    [{"Base value calculation for all game types",
      fun test_base_values/0},
     {"Multiplier calculation with various options",
      fun test_multiplier_calculation/0},
     {"Basic game value calculation",
      fun test_basic_game_value_calculation/0}].

tops_calculation_test_() ->
    [{"Tops calculation - with consecutive jacks from top",
      fun test_tops_with_consecutive/0},
     {"Tops calculation - without consecutive jacks from top",
      fun test_tops_without_consecutive/0},
     {"Tops calculation - with all four jacks",
      fun test_tops_with_all_jacks/0},
     {"Tops calculation - without any jacks",
      fun test_tops_without_any_jacks/0},
     {"Tops calculation - interrupted sequence",
      fun test_tops_interrupted_sequence/0},
     {"Tops calculation - edge cases",
      fun test_tops_edge_cases/0}].

estimated_vs_actual_test_() ->
    [{"Hand game produces estimated values",
      fun test_hand_game_estimated_values/0},
     {"Skat game produces actual values",
      fun test_skat_game_actual_values/0},
     {"Estimated vs actual value comparison",
      fun test_estimated_vs_actual_comparison/0},
     {"Hand game estimation accuracy",
      fun test_hand_game_estimation_accuracy/0}].

null_games_test_() ->
    [{"Null game basic value",
      fun test_null_game_basic/0},
     {"Null hand game value",
      fun test_null_hand_game/0},
     {"Null ouvert game value",
      fun test_null_ouvert_game/0},
     {"Null hand ouvert game value",
      fun test_null_hand_ouvert_game/0}].

multipliers_test_() ->
    [{"Schneider multiplier effect",
      fun test_schneider_multiplier/0},
     {"Schwarz multiplier effect",
      fun test_schwarz_multiplier/0},
     {"Ouvert multiplier effect",
      fun test_ouvert_multiplier/0},
     {"Combined multipliers effect",
      fun test_combined_multipliers/0},
     {"Multiplier announcements vs achievements",
      fun test_multiplier_announcements_vs_achievements/0}].

trump_sequence_test_() ->
    [{"Trump sequence for suit games",
      fun test_trump_sequence_suit_games/0},
     {"Trump sequence for grand game",
      fun test_trump_sequence_grand/0},
     {"Trump sequence for null game",
      fun test_trump_sequence_null/0}].

validation_test_() ->
    [{"Bid validation against game value",
      fun test_bid_validation/0},
     {"Minimum possible value calculation",
      fun test_minimum_possible_value/0},
     {"Game value display formatting",
      fun test_game_value_display_formatting/0}].

edge_cases_test_() ->
    [{"Empty hand handling",
      fun test_empty_hand_handling/0},
     {"Invalid game type handling",
      fun test_invalid_game_type_handling/0},
     {"Maximum possible game value",
      fun test_maximum_possible_game_value/0},
     {"Minimum possible game value",
      fun test_minimum_possible_game_value/0}].

integration_test_() ->
    [{"Integration with existing card types",
      fun test_integration_with_card_types/0},
     {"Consistency with bidding valid bids",
      fun test_consistency_with_valid_bids/0}].

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

%% Basic Calculations Tests

test_base_values() ->
    %% Test all base values according to International Skat Order
    ?assertEqual(9, erlskat_game_value:get_base_value(<<"diamonds">>)),
    ?assertEqual(10, erlskat_game_value:get_base_value(<<"hearts">>)),
    ?assertEqual(11, erlskat_game_value:get_base_value(<<"spades">>)),
    ?assertEqual(12, erlskat_game_value:get_base_value(<<"clubs">>)),
    ?assertEqual(24, erlskat_game_value:get_base_value(<<"grand">>)),
    ?assertEqual(0, erlskat_game_value:get_base_value(<<"null">>)),
    ?assertEqual(0, erlskat_game_value:get_base_value(<<"invalid">>)).

test_multiplier_calculation() ->
    %% Test basic multiplier (1 + tops + bonuses)
    Options1 = #{tops_count => 2, is_hand_game => false, selected_multipliers => []},
    ?assertEqual(3, erlskat_game_value:calculate_multiplier(Options1)), % 1 + 2 + 0

    %% Test with hand game bonus
    Options2 = #{tops_count => 1, is_hand_game => true, selected_multipliers => []},
    ?assertEqual(3, erlskat_game_value:calculate_multiplier(Options2)), % 1 + 1 + 1

    %% Test with multipliers
    Options3 = #{tops_count => 3, is_hand_game => true, selected_multipliers => [schnieder]},
    ?assertEqual(7, erlskat_game_value:calculate_multiplier(Options3)), % 1 + 3 + 1 + 2 (schnieder bonus + announced)

    %% Test with all bonuses
    Options4 = #{tops_count => 4, is_hand_game => true,
                 selected_multipliers => [schnieder, schwartz, ouvert],
                 is_schneider_achieved => true, is_schwarz_achieved => true},
    ?assertEqual(16, erlskat_game_value:calculate_multiplier(Options4)). % 1 + 4 + 1 + 10 (all bonuses)

test_basic_game_value_calculation() ->
    %% Test basic clubs game with 2 tops, hand game
    Hand = create_hand_with_jacks([clubs, spades]),
    Options = #{is_hand_game => true, selected_multipliers => []},

    Result = erlskat_game_value:calculate_estimated_game_value(<<"clubs">>, Hand, Options),

    ?assertEqual(12, maps:get(base_value, Result)), % Clubs base value
    ?assertEqual(2, maps:get(tops_count, Result)), % 2 jacks
    ?assertEqual(4, maps:get(multiplier, Result)), % 1 + 2 + 1 (hand)
    ?assertEqual(48, maps:get(value, Result)), % 12 * 4
    ?assert(maps:get(is_estimated, Result)),
    ?assertEqual(<<"with 2">>, maps:get(tops_description, Result)).

%% Tops Calculation Tests

test_tops_with_consecutive() ->
    %% Test "with X" tops - consecutive jacks from top
    Hand1 = create_hand_with_jacks([clubs]),
    Result1 = erlskat_game_value:calculate_tops(<<"clubs">>, Hand1),
    ?assertEqual(1, maps:get(count, Result1)),
    ?assertEqual(<<"with 1">>, maps:get(description, Result1)),
    ?assertEqual(with, maps:get(sequence_type, Result1)),

    Hand2 = create_hand_with_jacks([clubs, spades]),
    Result2 = erlskat_game_value:calculate_tops(<<"clubs">>, Hand2),
    ?assertEqual(2, maps:get(count, Result2)),
    ?assertEqual(<<"with 2">>, maps:get(description, Result2)),
    ?assertEqual(with, maps:get(sequence_type, Result2)),

    Hand3 = create_hand_with_jacks([clubs, spades, hearts]),
    Result3 = erlskat_game_value:calculate_tops(<<"clubs">>, Hand3),
    ?assertEqual(3, maps:get(count, Result3)),
    ?assertEqual(<<"with 3">>, maps:get(description, Result3)),
    ?assertEqual(with, maps:get(sequence_type, Result3)).

test_tops_without_consecutive() ->
    %% Test "without X" tops - missing consecutive jacks from top
    Hand1 = create_hand_with_jacks([spades, hearts, diamonds]), % Missing J♣
    Result1 = erlskat_game_value:calculate_tops(<<"clubs">>, Hand1),
    ?assertEqual(1, maps:get(count, Result1)),
    ?assertEqual(<<"without 1">>, maps:get(description, Result1)),
    ?assertEqual(without, maps:get(sequence_type, Result1)),

    Hand2 = create_hand_with_jacks([hearts, diamonds]), % Missing J♣ and J♠
    Result2 = erlskat_game_value:calculate_tops(<<"clubs">>, Hand2),
    ?assertEqual(2, maps:get(count, Result2)),
    ?assertEqual(<<"without 2">>, maps:get(description, Result2)),
    ?assertEqual(without, maps:get(sequence_type, Result2)),

    Hand3 = create_hand_with_jacks([diamonds]), % Missing J♣, J♠, J♥
    Result3 = erlskat_game_value:calculate_tops(<<"clubs">>, Hand3),
    ?assertEqual(3, maps:get(count, Result3)),
    ?assertEqual(<<"without 3">>, maps:get(description, Result3)),
    ?assertEqual(without, maps:get(sequence_type, Result3)).

test_tops_with_all_jacks() ->
    %% Test with all four jacks
    Hand = create_hand_with_jacks([clubs, spades, hearts, diamonds]),
    Result = erlskat_game_value:calculate_tops(<<"clubs">>, Hand),
    ?assertEqual(4, maps:get(count, Result)),
    ?assertEqual(<<"with 4">>, maps:get(description, Result)),
    ?assertEqual(with, maps:get(sequence_type, Result)).

test_tops_without_any_jacks() ->
    %% Test without any jacks
    Hand = create_hand_without_jacks(),
    Result = erlskat_game_value:calculate_tops(<<"clubs">>, Hand),
    ?assertEqual(4, maps:get(count, Result)),
    ?assertEqual(<<"without 4">>, maps:get(description, Result)),
    ?assertEqual(without, maps:get(sequence_type, Result)).

test_tops_interrupted_sequence() ->
    %% Test interrupted sequences as described in the rules
    % Example: J♣ J♥ (missing J♠) = "with 1" (interrupted by missing J♠)
    Hand1 = create_hand_with_jacks([clubs, hearts]), % Missing J♠
    Result1 = erlskat_game_value:calculate_tops(<<"clubs">>, Hand1),
    ?assertEqual(1, maps:get(count, Result1)),
    ?assertEqual(<<"with 1">>, maps:get(description, Result1)),
    ?assertEqual(with, maps:get(sequence_type, Result1)),

    % Example: J♥ alone = "without 2" (counting interrupted by present J♥)
    Hand2 = create_hand_with_jacks([hearts]), % Only J♥
    Result2 = erlskat_game_value:calculate_tops(<<"clubs">>, Hand2),
    ?assertEqual(2, maps:get(count, Result2)),
    ?assertEqual(<<"without 2">>, maps:get(description, Result2)),
    ?assertEqual(without, maps:get(sequence_type, Result2)).

test_tops_edge_cases() ->
    %% Test with grand game (only jacks count)
    Hand = create_hand_with_jacks([clubs, spades]),
    Result = erlskat_game_value:calculate_tops(<<"grand">>, Hand),
    ?assertEqual(2, maps:get(count, Result)),
    ?assertEqual(<<"with 2">>, maps:get(description, Result)),

    %% Test with null game (no trumps)
    Result2 = erlskat_game_value:calculate_tops(<<"null">>, Hand),
    ?assertEqual(0, maps:get(count, Result2)),
    ?assertEqual(<<"with 0">>, maps:get(description, Result2)).

%% Estimated vs Actual Tests

test_hand_game_estimated_values() ->
    %% Hand games should always produce estimated values
    Hand = create_hand_with_jacks([clubs, spades]),
    Options = #{is_hand_game => true, selected_multipliers => []},

    Result = erlskat_game_value:calculate_game_value(<<"clubs">>, Hand, Options),

    ?assert(maps:get(is_estimated, Result)),
    ?assert(erlskat_game_value:is_estimated(Result)).

test_skat_game_actual_values() ->
    %% Skat games should produce actual values
    Hand = create_hand_with_jacks([clubs, spades]),
    Skat = [#{rank => ace, suit => clubs}, #{rank => ten, suit => clubs}],
    Options = #{is_hand_game => false, selected_multipliers => []},

    Result = erlskat_game_value:calculate_actual_game_value(<<"clubs">>, Hand, Skat, Options),

    ?assertNot(maps:get(is_estimated, Result)),
    ?assertNot(erlskat_game_value:is_estimated(Result)).

test_estimated_vs_actual_comparison() ->
    %% Compare estimated vs actual values
    Hand = create_hand_with_jacks([clubs, spades]),
    Skat = [#{rank => ace, suit => clubs}, #{rank => ten, suit => clubs}],
    Options = #{is_hand_game => true, selected_multipliers => []},

    EstimatedResult = erlskat_game_value:calculate_estimated_game_value(<<"clubs">>, Hand, Options),
    ActualResult = erlskat_game_value:calculate_actual_game_value(<<"clubs">>, Hand, Skat, Options),

    ?assert(maps:get(is_estimated, EstimatedResult)),
    ?assertNot(maps:get(is_estimated, ActualResult)),

    %% Actual value should be higher due to additional trump cards in skat
    ?assert(maps:get(value, ActualResult) >= maps:get(value, EstimatedResult)).

test_hand_game_estimation_accuracy() ->
    %% Test that hand games show correct estimation indicators
    Hand = create_hand_with_jacks([clubs]),
    Options = #{is_hand_game => true, selected_multipliers => []},

    Result = erlskat_game_value:calculate_estimated_game_value(<<"clubs">>, Hand, Options),
    Display = erlskat_game_value:format_game_value_display(Result, <<"clubs">>),

    ?assert(binary:match(Display, <<"Est. Value:">>) =/= nomatch),
    ?assert(binary:match(Display, <<"+">>) =/= nomatch).

%% Null Games Tests

test_null_game_basic() ->
    %% Basic null game value (23)
    Options = #{is_hand_game => false, selected_multipliers => []},
    Result = erlskat_game_value:calculate_game_value(<<"null">>, [], Options),

    ?assertEqual(23, maps:get(value, Result)),
    ?assertEqual(0, maps:get(tops_count, Result)),
    ?assertEqual(<<"null game">>, maps:get(tops_description, Result)),
    ?assertNot(maps:get(is_estimated, Result)).

test_null_hand_game() ->
    %% Null hand game value (35)
    Options = #{is_hand_game => true, selected_multipliers => []},
    Result = erlskat_game_value:calculate_game_value(<<"null">>, [], Options),

    ?assertEqual(35, maps:get(value, Result)).

test_null_ouvert_game() ->
    %% Null ouvert game value (46)
    Options = #{is_hand_game => false, selected_multipliers => [ouvert]},
    Result = erlskat_game_value:calculate_game_value(<<"null">>, [], Options),

    ?assertEqual(46, maps:get(value, Result)).

test_null_hand_ouvert_game() ->
    %% Null hand ouvert game value (59)
    Options = #{is_hand_game => true, selected_multipliers => [ouvert]},
    Result = erlskat_game_value:calculate_game_value(<<"null">>, [], Options),

    ?assertEqual(59, maps:get(value, Result)).

%% Multipliers Tests

test_schneider_multiplier() ->
    %% Test schneider multiplier adds +2 (bonus + announced)
    Hand = create_hand_with_jacks([clubs]),

    OptionsWithout = #{is_hand_game => true, selected_multipliers => []},
    OptionsWith = #{is_hand_game => true, selected_multipliers => [schnieder]},

    ResultWithout = erlskat_game_value:calculate_estimated_game_value(<<"clubs">>, Hand, OptionsWithout),
    ResultWith = erlskat_game_value:calculate_estimated_game_value(<<"clubs">>, Hand, OptionsWith),

    ?assertEqual(maps:get(multiplier, ResultWith),
                maps:get(multiplier, ResultWithout) + 2),
    ?assertEqual(maps:get(value, ResultWith),
                maps:get(value, ResultWithout) + 24). % Base value * 2

test_schwarz_multiplier() ->
    %% Test schwarz multiplier adds +2 (bonus + announced)
    Hand = create_hand_with_jacks([clubs]),

    OptionsWithSchneider = #{is_hand_game => true, selected_multipliers => [schnieder]},
    OptionsWithSchwarz = #{is_hand_game => true, selected_multipliers => [schnieder, schwartz]},

    ResultSchneider = erlskat_game_value:calculate_estimated_game_value(<<"clubs">>, Hand, OptionsWithSchneider),
    ResultSchwarz = erlskat_game_value:calculate_estimated_game_value(<<"clubs">>, Hand, OptionsWithSchwarz),

    ?assertEqual(maps:get(multiplier, ResultSchwarz),
                maps:get(multiplier, ResultSchneider) + 2).

test_ouvert_multiplier() ->
    %% Test ouvert multiplier adds +6 (total bonus)
    Hand = create_hand_with_jacks([clubs]),

    OptionsWithout = #{is_hand_game => true, selected_multipliers => [schnieder, schwartz]},
    OptionsWith = #{is_hand_game => true, selected_multipliers => [schnieder, schwartz, ouvert]},

    ResultWithout = erlskat_game_value:calculate_estimated_game_value(<<"clubs">>, Hand, OptionsWithout),
    ResultWith = erlskat_game_value:calculate_estimated_game_value(<<"clubs">>, Hand, OptionsWith),

    ?assertEqual(maps:get(multiplier, ResultWith),
                maps:get(multiplier, ResultWithout) + 6).

test_combined_multipliers() ->
    %% Test all multipliers together
    Hand = create_hand_with_jacks([clubs, spades, hearts, diamonds]),
    Options = #{is_hand_game => true,
                selected_multipliers => [schnieder, schwartz, ouvert]},

    Result = erlskat_game_value:calculate_estimated_game_value(<<"clubs">>, Hand, Options),

    %% Expected: 1 (base) + 4 (tops) + 1 (hand) + 1 (schnieder) + 1 (schwartz) +
    %%           1 (schnieder announced) + 1 (schwarz announced) + 6 (ouvert) = 16
    ?assertEqual(16, maps:get(multiplier, Result)),
    ?assertEqual(192, maps:get(value, Result)). % 12 * 16

test_multiplier_announcements_vs_achievements() ->
    %% Test difference between announced and achieved multipliers
    Hand = create_hand_with_jacks([clubs]),

    % Only announced
    OptionsAnnounced = #{is_hand_game => true, selected_multipliers => [schnieder]},

    % Announced and achieved
    OptionsAchieved = #{is_hand_game => true, selected_multipliers => [schnieder],
                       is_schneider_achieved => true},

    ResultAnnounced = erlskat_game_value:calculate_estimated_game_value(<<"clubs">>, Hand, OptionsAnnounced),
    ResultAchieved = erlskat_game_value:calculate_actual_game_value(<<"clubs">>, Hand, [], OptionsAchieved),

    % Both should have same value (announced gives +1, achieved gives +1, but they overlap in calculation)
    ?assertEqual(maps:get(multiplier, ResultAnnounced), maps:get(multiplier, ResultAchieved)).

%% Trump Sequence Tests

test_trump_sequence_suit_games() ->
    %% Test trump sequences for suit games
    ClubsSequence = erlskat_game_value:get_trump_sequence(<<"clubs">>),

    % Should start with all 4 jacks
    ?assertEqual(#{rank => jack, suit => clubs}, lists:nth(1, ClubsSequence)),
    ?assertEqual(#{rank => jack, suit => spades}, lists:nth(2, ClubsSequence)),
    ?assertEqual(#{rank => jack, suit => hearts}, lists:nth(3, ClubsSequence)),
    ?assertEqual(#{rank => jack, suit => diamonds}, lists:nth(4, ClubsSequence)),

    % Then clubs trump cards
    ?assertEqual(#{rank => ace, suit => clubs}, lists:nth(5, ClubsSequence)),
    ?assertEqual(#{rank => ten, suit => clubs}, lists:nth(6, ClubsSequence)),

    % Should have 11 total trump cards for suit games (4 jacks + 7 suit cards)
    ClubsTrumps = lists:filter(fun(Card) ->
        maps:get(rank, Card) =:= jack orelse maps:get(suit, Card) =:= clubs
    end, ClubsSequence),
    ?assertEqual(11, length(ClubsTrumps)).

test_trump_sequence_grand() ->
    %% Test trump sequence for grand (only jacks)
    GrandSequence = erlskat_game_value:get_trump_sequence(<<"grand">>),

    ?assertEqual(4, length(GrandSequence)),
    ?assertEqual(#{rank => jack, suit => clubs}, lists:nth(1, GrandSequence)),
    ?assertEqual(#{rank => jack, suit => spades}, lists:nth(2, GrandSequence)),
    ?assertEqual(#{rank => jack, suit => hearts}, lists:nth(3, GrandSequence)),
    ?assertEqual(#{rank => jack, suit => diamonds}, lists:nth(4, GrandSequence)).

test_trump_sequence_null() ->
    %% Test trump sequence for null (no trumps)
    NullSequence = erlskat_game_value:get_trump_sequence(<<"null">>),
    ?assertEqual([], NullSequence).

%% Validation Tests

test_bid_validation() ->
    %% Test bid validation against calculated game values
    Hand = create_hand_with_jacks([clubs, spades]),
    Options = #{is_hand_game => true, selected_multipliers => []},

    Result = erlskat_game_value:calculate_estimated_game_value(<<"clubs">>, Hand, Options),
    GameValue = maps:get(value, Result),

    % Should accept bids equal to or less than game value
    ?assert(erlskat_game_value:validate_bid_possible(GameValue, Result, Options)),
    ?assert(erlskat_game_value:validate_bid_possible(GameValue - 10, Result, Options)),

    % Should reject bids higher than game value
    ?assertNot(erlskat_game_value:validate_bid_possible(GameValue + 10, Result, Options)).

test_minimum_possible_value() ->
    %% Test minimum possible value calculation
    Hand = create_hand_with_jacks([clubs, spades]),

    MinValue = erlskat_game_value:get_minimum_possible_value(<<"clubs">>, Hand),

    % Minimum: base value (12) * minimum multiplier (1 + 2 tops) = 36
    ?assertEqual(36, MinValue).

test_game_value_display_formatting() ->
    %% Test display formatting
    Hand = create_hand_with_jacks([clubs]),
    Options = #{is_hand_game => true, selected_multipliers => []},

    Result = erlskat_game_value:calculate_estimated_game_value(<<"clubs">>, Hand, Options),
    Display = erlskat_game_value:format_game_value_display(Result, <<"clubs">>),

    % Should contain estimated value indicator
    ?assert(binary:match(Display, <<"Est. Value:">>) =/= nomatch),
    ?assert(binary:match(Display, <<"+">>) =/= nomatch),
    ?assert(binary:match(Display, <<"with 1">>) =/= nomatch),
    ?assert(binary:match(Display, <<"clubs">>) =/= nomatch).

%% Edge Cases Tests

test_empty_hand_handling() ->
    %% Test with empty hand
    EmptyHand = [],
    Options = #{is_hand_game => true, selected_multipliers => []},

    Result = erlskat_game_value:calculate_estimated_game_value(<<"clubs">>, EmptyHand, Options),

    % Should handle empty hand gracefully - for clubs game, 11 trump cards total
    ?assertEqual(11, maps:get(tops_count, Result)), % "without 11"
    ?assertEqual(<<"without 11">>, maps:get(tops_description, Result)).

test_invalid_game_type_handling() ->
    %% Test with invalid game type
    ?assertEqual(0, erlskat_game_value:get_base_value(<<"invalid_game">>)).

test_maximum_possible_game_value() ->
    %% Test maximum possible game value scenario
    % All 4 jacks + all trump cards + hand + all multipliers
    Hand = create_hand_with_all_trumps(),
    Options = #{is_hand_game => true,
                selected_multipliers => [schnieder, schwartz, ouvert]},

    Result = erlskat_game_value:calculate_estimated_game_value(<<"clubs">>, Hand, Options),

    % Should be a very high value
    ?assert(maps:get(value, Result) > 200).

test_minimum_possible_game_value() ->
    %% Test minimum possible game value scenario
    %% without 1
    Hand = create_hand_with_jacks([spades]),
    Options = #{is_hand_game => false, selected_multipliers => []},

    Result = erlskat_game_value:calculate_estimated_game_value(<<"diamonds">>, Hand, Options),

    % Minimum: diamonds (9) * (without 1, plays = 2) = 18
    ?assertEqual(18, maps:get(value, Result)).

%% Integration Tests

test_integration_with_card_types() ->
    %% Test integration with existing card type system
    Hand = [#{rank => jack, suit => clubs},
            #{rank => ace, suit => hearts},
            #{rank => ten, suit => spades}],
    Options = #{is_hand_game => true, selected_multipliers => []},

    % Should work with existing card format
    Result = erlskat_game_value:calculate_estimated_game_value(<<"grand">>, Hand, Options),

    ?assertEqual(1, maps:get(tops_count, Result)),
    ?assertEqual(24, maps:get(base_value, Result)),
    ?assert(maps:get(value, Result) > 0).

test_consistency_with_valid_bids() ->
    %% Test that calculated values align with valid bid sequence
    ValidBids = [18, 20, 22, 23, 24, 27, 30, 33, 35, 36, 40, 44, 45, 46, 48, 50],

    Hand = create_hand_with_jacks([clubs]),
    Options = #{is_hand_game => true, selected_multipliers => []},

    Result = erlskat_game_value:calculate_estimated_game_value(<<"clubs">>, Hand, Options),
    GameValue = maps:get(value, Result),

    % Game value should match one of the valid bid values or be achievable
    ?assert(GameValue >= 18),
    ?assert(lists:any(fun(Bid) -> Bid =< GameValue end, ValidBids)).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

%% Helper to create a hand with specific jacks
create_hand_with_jacks(JackSuits) ->
    Jacks = [#{rank => jack, suit => Suit} || Suit <- JackSuits],
    NonJacks = [#{rank => ace, suit => hearts},
                #{rank => ten, suit => spades},
                #{rank => king, suit => diamonds},
                #{rank => queen, suit => clubs}],
    Jacks ++ NonJacks.

%% Helper to create a hand without any jacks
create_hand_without_jacks() ->
    [#{rank => ace, suit => clubs},
     #{rank => ten, suit => spades},
     #{rank => king, suit => hearts},
     #{rank => queen, suit => diamonds},
     #{rank => nine, suit => clubs},
     #{rank => eight, suit => spades},
     #{rank => seven, suit => hearts}].

%% Helper to create a hand with all possible trumps for a suit
create_hand_with_all_trumps() ->
    [#{rank => jack, suit => clubs},
     #{rank => jack, suit => spades},
     #{rank => jack, suit => hearts},
     #{rank => jack, suit => diamonds},
     #{rank => ace, suit => clubs},
     #{rank => ten, suit => clubs},
     #{rank => king, suit => clubs},
     #{rank => queen, suit => clubs},
     #{rank => nine, suit => clubs},
     #{rank => eight, suit => clubs}].

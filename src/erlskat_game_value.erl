%%%-------------------------------------------------------------------
%%% @author Ben Crabbe <ben.crabbe.dev@gmail.com>
%%% @copyright (C) 2019, Ben Crabbe
%%% @doc
%%% Game value calculation module for Skat according to International Skat Order
%%% Handles both estimated (hand games) and actual (with Skat) game value calculations
%%% @end
%%% Created : 31 Jul 2025 Ben Crabbe <ben.crabbe.dev@gmail.com>
%%%-------------------------------------------------------------------
-module(erlskat_game_value).

-include_lib("kernel/include/logger.hrl").

%% API exports
-export([
    calculate_game_value/3,
    calculate_estimated_game_value/3,
    calculate_actual_game_value/4,
    is_estimated/1,
    calculate_tops/2,
    get_base_value/1,
    calculate_multiplier/1,
    format_game_value_display/2,
    validate_bid_possible/3,
    get_minimum_possible_value/2
]).

%% Helper function exports
-export([
    get_trump_sequence/1,
    count_consecutive_tops/2,
    format_tops_description/1,
    apply_bonuses/2
]).

%% Type definitions
% Game types: clubs, spades, hearts, diamonds, grand, null
-type game_type() :: binary().
-type game_options() :: #{
    is_hand_game => boolean(),
    selected_multipliers => [atom()], % [schnieder, schwartz, ouvert]
    is_schneider_achieved => boolean(),
    is_schwarz_achieved => boolean()
}.

-type game_value_result() :: #{
    value := integer(),
    is_estimated := boolean(),
    tops_count := integer(),
    tops_description := binary(),
    multiplier := integer(),
    base_value := integer(),
    calculation_details := map()
}.

-type tops_result() :: #{
    count := integer(),
    description := binary(),
    sequence_type := with | without
}.

-export_type([game_type/0, game_options/0, game_value_result/0, tops_result/0]).

%% Base values according to International Skat Order
-define(BASE_VALUES, #{
    <<"diamonds">> => 9,
    <<"hearts">> => 10,
    <<"spades">> => 11,
    <<"clubs">> => 12,
    <<"grand">> => 24
}).

%% Fixed Null game values
-define(NULL_VALUES, #{
    null => 23,
    null_hand => 35,
    null_ouvert => 46,
    null_hand_ouvert => 59
}).

%% Trump card ordering - Jacks are always highest trumps
-define(JACK_ORDER, [
    #{rank => jack, suit => clubs},
    #{rank => jack, suit => spades},
    #{rank => jack, suit => hearts},
    #{rank => jack, suit => diamonds}
]).

%% Non-jack trump ordering within suits
-define(SUIT_ORDER, [ace, ten, king, queen, nine, eight, seven]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% Main game value calculation function
-spec calculate_game_value(game_type(), erlskat:cards(), game_options()) -> game_value_result().
calculate_game_value(GameType, PlayerHand, Options) ->
    IsHandGame = maps:get(is_hand_game, Options, false),
    case IsHandGame of
        true ->
            calculate_estimated_game_value(GameType, PlayerHand, Options);
        false ->
            calculate_actual_game_value(GameType, PlayerHand, [], Options)
    end.

%% Calculate estimated game value for hand games (Skat unknown)
-spec calculate_estimated_game_value(game_type(), erlskat:cards(), game_options()) ->
          game_value_result().
calculate_estimated_game_value(GameType, PlayerHand, Options) ->
    case GameType of
        <<"null">> ->
            calculate_null_game_value(Options);
        _ ->
            BaseValue = get_base_value(GameType),
            TopsResult = calculate_tops(GameType, PlayerHand),
            Multiplier = calculate_multiplier(#{
                tops_count => maps:get(count, TopsResult),
                is_hand_game => maps:get(is_hand_game, Options, true),
                selected_multipliers => maps:get(selected_multipliers, Options, [])
            }),
            Value = BaseValue * Multiplier,
            #{
                value => Value,
                is_estimated => true,
                tops_count => maps:get(count, TopsResult),
                tops_description => maps:get(description, TopsResult),
                multiplier => Multiplier,
                base_value => BaseValue,
                calculation_details => #{
                    game_type => GameType,
                    player_hand => PlayerHand,
                    skat_known => false,
                    options => Options
                }
            }
    end.

%% Calculate actual game value with known Skat cards
-spec calculate_actual_game_value(game_type(), erlskat:cards(), erlskat:skat(),
                                   game_options()) -> game_value_result().
calculate_actual_game_value(GameType, PlayerHand, SkatCards, Options) ->
    case GameType of
        <<"null">> ->
            calculate_null_game_value(Options);
        _ ->
            FullHand = PlayerHand ++ SkatCards,
            BaseValue = get_base_value(GameType),
            TopsResult = calculate_tops(GameType, FullHand),
            Multiplier = calculate_multiplier(#{
                tops_count => maps:get(count, TopsResult),
                is_hand_game => maps:get(is_hand_game, Options, false),
                selected_multipliers => maps:get(selected_multipliers, Options, []),
                is_schneider_achieved => maps:get(is_schneider_achieved, Options, false),
                is_schwarz_achieved => maps:get(is_schwarz_achieved, Options, false)
            }),
            Value = BaseValue * Multiplier,
            #{
                value => Value,
                is_estimated => false,
                tops_count => maps:get(count, TopsResult),
                tops_description => maps:get(description, TopsResult),
                multiplier => Multiplier,
                base_value => BaseValue,
                calculation_details => #{
                    game_type => GameType,
                    player_hand => PlayerHand,
                    skat_cards => SkatCards,
                    skat_known => true,
                    options => Options
                }
            }
    end.

%% Check if a game value result is estimated
-spec is_estimated(game_value_result()) -> boolean().
is_estimated(#{is_estimated := IsEstimated}) ->
    IsEstimated.

%% Calculate tops (matadors) for a given game type and hand
-spec calculate_tops(game_type(), erlskat:cards()) -> tops_result().
calculate_tops(GameType, Hand) ->
    TrumpSequence = get_trump_sequence(GameType),
    case count_consecutive_tops(TrumpSequence, Hand) of
        {with, Count} ->
            #{
                count => Count,
                description => format_tops_description({with, Count}),
                sequence_type => with
            };
        {without, Count} ->
            #{
                count => Count,
                description => format_tops_description({without, Count}),
                sequence_type => without
            }
    end.

%% Get base value for a game type
-spec get_base_value(game_type()) -> integer().
get_base_value(GameType) ->
    maps:get(GameType, ?BASE_VALUES, 0).

%% Calculate multiplier from game options
-spec calculate_multiplier(map()) -> integer().
calculate_multiplier(#{tops_count := TopsCount} = Options) ->
    Base = 1,
    Tops = TopsCount,
    Hand = case maps:get(is_hand_game, Options, false) of
        true -> 1;
        false -> 0
    end,
    Bonuses = apply_bonuses(maps:get(selected_multipliers, Options, []), Options),
    Base + Tops + Hand + Bonuses.

%% Format game value for display to client
-spec format_game_value_display(game_value_result(), binary()) -> binary().
format_game_value_display(#{value := Value, is_estimated := IsEstimated,
                           tops_description := TopsDesc}, GameType) ->
    EstimatePrefix = case IsEstimated of
        true -> <<"Est. Value: ">>;
        false -> <<"Value: ">>
    end,
    EstimateSuffix = case IsEstimated of
        true -> <<"+">>;
        false -> <<"">>
    end,
    iolist_to_binary([
        EstimatePrefix,
        integer_to_list(Value),
        EstimateSuffix,
        <<" (">>,
        TopsDesc,
        <<", ">>,
        GameType,
        <<")">>
    ]).

%% Validate if a calculated game value meets the bid requirement
-spec validate_bid_possible(integer(), game_value_result(), game_options()) -> boolean().
validate_bid_possible(BidValue, #{value := CalculatedValue,
                       is_estimated := IsEstimated}, _Options) ->
    case IsEstimated of
        true ->
            CalculatedValue >= BidValue;
        false ->
            CalculatedValue >= BidValue
    end.

%% Get minimum possible game value for conservative estimates
-spec get_minimum_possible_value(game_type(), erlskat:cards()) -> integer().
get_minimum_possible_value(GameType, PlayerHand) ->
    BaseValue = get_base_value(GameType),
    TopsResult = calculate_tops(GameType, PlayerHand),
    MinMultiplier = 1 + maps:get(count, TopsResult),
    BaseValue * MinMultiplier.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% Get complete trump sequence for a game type
-spec get_trump_sequence(game_type()) -> [erlskat:card()].
get_trump_sequence(<<"grand">>) ->
    ?JACK_ORDER;
get_trump_sequence(<<"null">>) ->
    [];
get_trump_sequence(GameType) when GameType =:= <<"clubs">>;
                                  GameType =:= <<"spades">>;
                                  GameType =:= <<"hearts">>;
                                  GameType =:= <<"diamonds">> ->
    TrumpSuit = binary_to_atom(GameType, utf8),
    TrumpSuitCards = [#{rank => Rank, suit => TrumpSuit} || Rank <- ?SUIT_ORDER],
    ?JACK_ORDER ++ TrumpSuitCards.

%% Count consecutive tops from the trump sequence
-spec count_consecutive_tops([erlskat:card()], erlskat:cards()) -> {with | without, integer()}.
count_consecutive_tops([], _Hand) ->
    {with, 0};
count_consecutive_tops(TrumpSequence, Hand) ->
    case count_with_tops(TrumpSequence, Hand, 0) of
        0 ->
            count_without_tops(TrumpSequence, Hand, 0);
        WithCount ->
            {with, WithCount}
    end.

%% Count "with" tops - consecutive sequence starting from highest trump
count_with_tops([], _Hand, Count) ->
    Count;
count_with_tops([TopCard | RestTrumps], Hand, Count) ->
    case lists:member(TopCard, Hand) of
        true ->
            count_with_tops(RestTrumps, Hand, Count + 1);
        false ->
            Count
    end.

%% Count "without" tops - consecutive missing cards from top
count_without_tops([], _Hand, Count) ->
    {without, Count};
count_without_tops([TopCard | RestTrumps], Hand, Count) ->
    case lists:member(TopCard, Hand) of
        false ->
            count_without_tops(RestTrumps, Hand, Count + 1);
        true ->
            {without, Count}
    end.

%% Format tops description for display
-spec format_tops_description({with | without, integer()}) -> binary().
format_tops_description({with, Count}) ->
    iolist_to_binary([<<"with ">>, integer_to_list(Count)]);
format_tops_description({without, Count}) ->
    iolist_to_binary([<<"without ">>, integer_to_list(Count)]).

%% Apply bonuses from multipliers and achievements
-spec apply_bonuses([atom()], map()) -> integer().
apply_bonuses(SelectedMultipliers, Options) ->
    SchneiderBonus = case lists:member(schnieder, SelectedMultipliers) of
        true -> 1;
        false ->
            case maps:get(is_schneider_achieved, Options, false) of
                true -> 1;
                false -> 0
            end
    end,

    SchwarzBonus = case lists:member(schwartz, SelectedMultipliers) of
        true -> 1;
        false ->
            case maps:get(is_schwarz_achieved, Options, false) of
                true -> 1;
                false -> 0
            end
    end,

    SchneiderAnnouncedBonus = case lists:member(schnieder, SelectedMultipliers) of
        true -> 1;
        false -> 0
    end,

    SchwarzAnnouncedBonus = case lists:member(schwartz, SelectedMultipliers) of
        true -> 1;
        false -> 0
    end,

    OuvertBonus = case lists:member(ouvert, SelectedMultipliers) of
        true -> 6;
        false -> 0
    end,

    SchneiderBonus + SchwarzBonus + SchneiderAnnouncedBonus + SchwarzAnnouncedBonus + OuvertBonus.

%% Calculate Null game values (fixed values)
-spec calculate_null_game_value(game_options()) -> game_value_result().
calculate_null_game_value(Options) ->
    IsHandGame = maps:get(is_hand_game, Options, false),
    SelectedMultipliers = maps:get(selected_multipliers, Options, []),
    IsOuvert = lists:member(ouvert, SelectedMultipliers),

    Value = case {IsHandGame, IsOuvert} of
        {false, false} -> maps:get(null, ?NULL_VALUES);
        {true, false} -> maps:get(null_hand, ?NULL_VALUES);
        {false, true} -> maps:get(null_ouvert, ?NULL_VALUES);
        {true, true} -> maps:get(null_hand_ouvert, ?NULL_VALUES)
    end,

    #{
        value => Value,
        is_estimated => false,
        tops_count => 0,
        tops_description => <<"null game">>,
        multiplier => 1,
        base_value => Value,
        calculation_details => #{
            game_type => <<"null">>,
            is_hand_game => IsHandGame,
            is_ouvert => IsOuvert,
            options => Options
        }
    }.

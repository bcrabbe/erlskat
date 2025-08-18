%%%-------------------------------------------------------------------
%%% @author Ben Crabbe <ben.crabbe.dev@gmail.com>
%%% @copyright (C) 2019, Ben Crabbe
%%% @doc
%%% Centralized message type definitions and constructors for client communications
%%% All messages sent to websocket clients should use these functions
%%% @end
%%%-------------------------------------------------------------------
-module(erlskat_client_responses).

%% Message constructor exports
-export([
    %% Bidding messages
    bid_prompt/1,
    awaiting_bid/2,
    game_type_prompt/1,
    game_type_prompt_with_values/2,
    multiplier_prompt/2,
    multiplier_prompt_with_values/4,
    initial_choice_prompt/0,
    skat_flipped/1,
    hand_with_skat/1,
    hand_after_discard/1,
    discard_prompt/1,
    bidding_complete/1,
    bidding_winner_notification/2,
    game_declaration_broadcast/2,
    game_type_broadcast/2,
    hand_reorder_broadcast/6,
    bid_broadcast/2,
    pass_broadcast/2,
    bidding_roles/1,
    cards_dealt/1,
    %% Game play messages
    card_play_prompt/4,
    awaiting_card/1,
    game_start_broadcast/4,
    card_played_broadcast/3,
    trick_won_broadcast/2,
    game_complete_broadcast/1,
    invalid_card_error/0,
    card_play_error/1,
    %% Connection management messages
    player_disconnected/2,
    player_reconnected/2,
    player_timed_out/1,
    game_closed/0,
    %% Lobby messages
    lobby_waiting/1,
    lobby_matched/1,
    player_joined/1,
    %% Table messages
    table_started/1,
    %% Scorecard messages
    scores_update_broadcast/1,
    next_hand_starting_broadcast/1,
    %% Error messages
    error_message/2
]).

%% Type exports
-export_type([
    message_type/0,
    client_message/0,
    game_result/0,
    player_hand/0,
    player_bidding_data/0,
    %% Individual message types
    bid_prompt_msg/0,
    awaiting_bid_msg/0,
    game_type_prompt_msg/0,
    multiplier_prompt_msg/0,
    initial_choice_prompt_msg/0,
    skat_flipped_msg/0,
    hand_with_skat_msg/0,
    hand_after_discard_msg/0,
    discard_prompt_msg/0,
    bidding_complete_msg/0,
    bidding_winner_notification_msg/0,
    game_declaration_broadcast_msg/0,
    game_type_broadcast_msg/0,
    hand_reorder_broadcast_msg/0,
    bid_broadcast_msg/0,
    pass_broadcast_msg/0,
    bidding_roles_msg/0,
    cards_dealt_msg/0,
    card_play_prompt_msg/0,
    awaiting_card_msg/0,
    game_start_broadcast_msg/0,
    card_played_broadcast_msg/0,
    trick_won_broadcast_msg/0,
    game_complete_broadcast_msg/0,
    invalid_card_error_msg/0,
    card_play_error_msg/0,
    player_disconnected_msg/0,
    player_reconnected_msg/0,
    player_timed_out_msg/0,
    game_closed_msg/0,
    lobby_status_msg/0,
    player_joined_msg/0,
    table_started_msg/0,
    scores_update_broadcast_msg/0,
    next_hand_starting_broadcast_msg/0,
    error_msg/0
]).

%% Base message type - all client messages must have a type field
-type message_type() ::
    %% Bidding phase messages
    bid_prompt | awaiting_bid | game_type_prompt | multiplier_prompt |
    initial_choice_prompt | skat_flipped | hand_with_skat | discard_prompt | bidding_complete |
    bidding_winner_notification | game_declaration_broadcast | game_type_broadcast |
    hand_reorder_broadcast | bid_broadcast | pass_broadcast | bidding_roles |
    cards_dealt | card_play_prompt | awaiting_card | game_start_broadcast | card_played_broadcast |
    trick_won_broadcast | game_complete_broadcast | invalid_card_error | card_play_error |
    %% Connection management messages
    player_disconnected | player_timed_out | game_closed |
    %% Lobby messages
    lobby_status | player_joined |
    %% Table messages
    table_started |
    %% Scorecard messages
    scores_update_broadcast | next_hand_starting_broadcast |
    %% Error messages
    error.

%% Common message fields
-type player_hand() :: #{
    player => erlskat:player(),
    hand => erlskat:cards()
}.

-type game_result() :: #{
    winner => erlskat:player_id(),
    final_bid => integer(),
    chosen_game => binary(),
    discarded_cards => erlskat:cards(),
    skat_cards => erlskat:skat(),
    player_hands => [player_hand()],
    is_hand_game => boolean(),
    selected_multipliers => [atom()]
}.

%% Bidding Phase Messages
-type bid_prompt_msg() :: #{
    type := bid_prompt,
    bid_value := integer(),
    message := binary(),
    choices := [hold | pass]
}.

-type awaiting_bid_msg() :: #{
    type := awaiting_bid,
    bid_value := integer(),
    waiting_for_player_id := erlskat:player_id(),
    message := binary()
}.

-type game_type_prompt_msg() :: #{
    type := game_type_prompt,
    game_types := [erlskat:game_type()],
    message := binary(),
    game_type_values => [#{game_type := erlskat:game_type(), value_display := binary()}]
}.

-type multiplier_prompt_msg() :: #{
    type := multiplier_prompt,
    multipliers := [binary()],
    game_type := erlskat:game_type(),
    message := binary(),
    current_value => binary(),
    multiplier_values => [#{multiplier := binary(), value_display := binary()}]
}.

-type initial_choice_prompt_msg() :: #{
    type := initial_choice_prompt,
    choices := [binary()],
    message := binary()
}.

-type skat_flipped_msg() :: #{
    type := skat_flipped,
    cards := erlskat:skat(),
    message := binary()
}.

-type hand_with_skat_msg() :: #{
    type := hand_with_skat,
    cards := erlskat:cards(),
    message := binary()
}.

-type hand_after_discard_msg() :: #{
    type := hand_after_discard,
    hand := erlskat:cards(),
    message := binary()
}.

-type discard_prompt_msg() :: #{
    type := discard_prompt,
    count := non_neg_integer(),
    message := binary()
}.

-type bidding_complete_msg() :: #{
    type := bidding_complete,
    result := game_result()
}.

-type bidding_winner_notification_msg() :: #{
    type := bidding_winner_notification,
    winner_id := erlskat:player_id(),
    bid_value := integer(),
    message := binary()
}.

-type game_declaration_broadcast_msg() :: #{
    type := game_declaration_broadcast,
    winner_id := erlskat:player_id(),
    choice := binary(),
    message := binary()
}.

-type game_type_broadcast_msg() :: #{
    type := game_type_broadcast,
    winner_id := erlskat:player_id(),
    game_type := erlskat:game_type(),
    message := binary()
}.

-type hand_reorder_broadcast_msg() :: #{
    type := hand_reorder_broadcast,
    winner_id := erlskat:player_id(),
    game_type := erlskat:game_type(),
    hand := erlskat:cards(),
    message := binary()
}.

-type bid_broadcast_msg() :: #{
    type := bid_broadcast,
    bidder := erlskat:player(),
    bid_value := integer(),
    message := binary()
}.

-type pass_broadcast_msg() :: #{
    type := pass_broadcast,
    passer := erlskat:player(),
    bid_value := integer(),
    message := binary()
}.

-type bidding_roles_msg() :: #{
    type := bidding_roles,
    roles := #{erlskat:player_id() => speaking | listening | waiting}
}.

-type cards_dealt_msg() :: #{
    type := cards_dealt,
    hand := erlskat:cards()
}.

%% Game Play Messages
-type card_play_prompt_msg() :: #{
    type := card_play_prompt,
    hand := erlskat:cards(),
    current_trick := [map()],
    valid_cards := [integer()],
    message := binary()
}.

-type awaiting_card_msg() :: #{
    type := awaiting_card,
    waiting_for_player_id := erlskat:player_id(),
    message := binary()
}.

-type game_start_broadcast_msg() :: #{
    type := game_start_broadcast,
    declarer := erlskat:player_id(),
    game_type := erlskat:game_type(),
    is_hand_game := boolean(),
    selected_multipliers := [atom()],
    message := binary()
}.

-type card_played_broadcast_msg() :: #{
    type := card_played_broadcast,
    player_id := erlskat:player_id(),
    card := erlskat:card(),
    message := binary()
}.

-type trick_won_broadcast_msg() :: #{
    type := trick_won_broadcast,
    winner_id := erlskat:player_id(),
    trick := [map()],
    message := binary()
}.

-type game_complete_broadcast_msg() :: #{
    type := game_complete_broadcast,
    result := erlskat_hand:game_result(),
    message := binary()
}.

-type invalid_card_error_msg() :: #{
    type := invalid_card_error,
    message := binary()
}.

-type card_play_error_msg() :: #{
    type := card_play_error,
    reason := binary(),
    message := binary()
}.

%% Connection Management Messages
-type player_disconnected_msg() :: #{
    type := player_disconnected,
    player_id := erlskat:player_id(),
    reconnection_deadline_ms := integer()
}.

-type player_reconnected_msg() :: #{
    type := player_reconnected,
    player_id := erlskat:player_id(),
    remaining_reconnecting_players := [erlskat:player_id()]
}.

-type player_timed_out_msg() :: #{
    type := player_timed_out,
    player_id := erlskat:player_id()
}.

-type game_closed_msg() :: #{
    type := game_closed,
    message := binary()
}.

%% Lobby Messages
-type lobby_status_msg() :: #{
    type := lobby_status,
    state := waiting | matched,
    players := [erlskat:player_id()]
}.

-type player_joined_msg() :: #{
    type := player_joined,
    player_id := erlskat:player_id()
}.

%% Table Messages
-type table_started_msg() :: #{
    type := table_started,
    players := [erlskat:player_id()]
}.

%% Scorecard Messages
-type scores_update_broadcast_msg() :: #{
    type := scores_update_broadcast,
    player_scores := [#{player_id := erlskat:player_id(), score := integer()}],
    message := binary()
}.

-type next_hand_starting_broadcast_msg() :: #{
    type := next_hand_starting_broadcast,
    hand_number := integer(),
    message := binary()
}.

%% Error Messages
-type error_msg() :: #{
    type := error,
    message := binary(),
    expected_format := map()
}.

%% Union type for all possible client messages
-type client_message() ::
    %% Bidding messages (all have type field)
    bid_prompt_msg() | awaiting_bid_msg() | game_type_prompt_msg() |
    multiplier_prompt_msg() | initial_choice_prompt_msg() | skat_flipped_msg() |
    hand_with_skat_msg() | hand_after_discard_msg() | discard_prompt_msg() |
    bidding_complete_msg() |
    bidding_winner_notification_msg() | game_declaration_broadcast_msg() |
    game_type_broadcast_msg() | hand_reorder_broadcast_msg() | bid_broadcast_msg() |
    pass_broadcast_msg() |
    bidding_roles_msg() | cards_dealt_msg() |
    card_play_prompt_msg() | awaiting_card_msg() | game_start_broadcast_msg() |
    card_played_broadcast_msg() |
    trick_won_broadcast_msg() | game_complete_broadcast_msg() | invalid_card_error_msg() |
    card_play_error_msg() | error_msg() |
    %% Connection messages (legacy format without type field)
    player_disconnected_msg() | player_reconnected_msg() |
    player_timed_out_msg() | game_closed_msg() |
    %% Lobby messages (legacy format without type field)
    lobby_status_msg() | player_joined_msg() |
    %% Table messages
    table_started_msg() |
    %% Scorecard messages
    scores_update_broadcast_msg() | next_hand_starting_broadcast_msg().

%% Helper type for player bidding data with hand information
-type player_bidding_data() :: #{
    player := erlskat:player(),
    hand := erlskat:cards()
}.

%%%===================================================================
%%% Message Constructor Functions
%%%===================================================================

%% Bidding message constructors
-spec bid_prompt(integer()) -> bid_prompt_msg().
bid_prompt(BidValue) ->
    #{type => bid_prompt,
      bid_value => BidValue,
      message => iolist_to_binary([<<"Do you want to bid ">>,
                                  integer_to_list(BidValue),
                                  <<"?">>]),
      choices => [hold, pass]}.

-spec awaiting_bid(integer(), erlskat:player_id()) -> awaiting_bid_msg().
awaiting_bid(BidValue, WaitingForPlayerId) ->
    #{type => awaiting_bid,
      bid_value => BidValue,
      waiting_for_player_id => WaitingForPlayerId,
      message => iolist_to_binary([<<"Waiting for ">>,
                                  WaitingForPlayerId,
                                  <<" to bid ">>,
                                  integer_to_list(BidValue)])}.

-spec game_type_prompt([erlskat:game_type()]) -> game_type_prompt_msg().
game_type_prompt(GameTypes) ->
    #{type => game_type_prompt,
      game_types => GameTypes,
      message => <<"Choose your game type">>}.

-spec game_type_prompt_with_values([erlskat:game_type()], map()) -> game_type_prompt_msg().
game_type_prompt_with_values(GameTypes, PlayerData) ->
    PlayerHand = maps:get(hand, PlayerData),
    % Determine if this is a hand game based on hand size
    % 10 cards = hand game, 12 cards = skat game
    IsHandGame = length(PlayerHand) =:= 10,
    GameTypeValues = [begin
        Options = #{is_hand_game => IsHandGame, selected_multipliers => []},
        ValueResult = erlskat_game_value:calculate_estimated_game_value(
                        GameType,
                        PlayerHand,
                        Options),
        ValueDisplay = erlskat_game_value:format_game_value_display(ValueResult, GameType),
        #{game_type => GameType, value_display => ValueDisplay}
    end || GameType <- GameTypes],
    #{type => game_type_prompt,
      game_types => GameTypes,
      game_type_values => GameTypeValues,
      message => <<"Choose your game type (estimated values shown)">>}.

-spec multiplier_prompt([binary()], erlskat:game_type()) -> multiplier_prompt_msg().
multiplier_prompt(Multipliers, GameType) ->
    #{type => multiplier_prompt,
      multipliers => Multipliers,
      game_type => GameType,
      message => <<"Choose additional multipliers (or skip)">>}.

-spec multiplier_prompt_with_values([binary()], erlskat:game_type(),
                                   map(), map()) -> multiplier_prompt_msg().
multiplier_prompt_with_values(Multipliers, GameType, PlayerData, GameData) ->
    PlayerHand = maps:get(hand, PlayerData),
    IsHandGame = maps:get(is_hand_game, GameData, false),
    SelectedMultipliers = maps:get(selected_multipliers, GameData, []),

    % Calculate current game value
    Options = #{is_hand_game => IsHandGame, selected_multipliers => SelectedMultipliers},
    CurrentValueResult = case IsHandGame of
        true -> erlskat_game_value:calculate_estimated_game_value(GameType, PlayerHand, Options);
        false -> erlskat_game_value:calculate_actual_game_value(GameType, PlayerHand, [], Options)
    end,
    CurrentDisplay = erlskat_game_value:format_game_value_display(CurrentValueResult, GameType),

    % Calculate values for each multiplier option
    MultiplierValues = [begin
        case Multiplier of
            <<"skip">> ->
                #{multiplier => Multiplier, value_display => <<"Skip multipliers">>};
            _ ->
                NewOptions = Options#{selected_multipliers => [binary_to_atom(Multiplier, utf8) |
                                                               SelectedMultipliers]},
                NewValueResult = case IsHandGame of
                    true -> erlskat_game_value:calculate_estimated_game_value(
                              GameType,
                              PlayerHand,
                              NewOptions);
                    false -> erlskat_game_value:calculate_actual_game_value(
                               GameType,
                               PlayerHand,
                               [],
                               NewOptions)
                end,
                NewDisplay = erlskat_game_value:format_game_value_display(NewValueResult, GameType),
                #{multiplier => Multiplier, value_display => NewDisplay}
        end
    end || Multiplier <- Multipliers],

    #{type => multiplier_prompt,
      multipliers => Multipliers,
      game_type => GameType,
      current_value => CurrentDisplay,
      multiplier_values => MultiplierValues,
      message => <<"Choose additional multipliers (values shown) or skip">>}.

-spec initial_choice_prompt() -> initial_choice_prompt_msg().
initial_choice_prompt() ->
    #{type => initial_choice_prompt,
      choices => [<<"hand">>, <<"skat">>],
      message => <<"Do you want to play hand or see the skat?">>}.

-spec skat_flipped(erlskat:skat()) -> skat_flipped_msg().
skat_flipped(SkatCards) ->
    #{type => skat_flipped,
      cards => SkatCards,
      message => <<"The skat has been flipped">>}.

-spec hand_with_skat(erlskat:cards()) -> hand_with_skat_msg().
hand_with_skat(Cards) ->
    #{type => hand_with_skat,
      cards => Cards,
      message => <<"Here are your cards including skat">>}.

-spec hand_after_discard(erlskat:cards()) -> hand_after_discard_msg().
hand_after_discard(Hand) ->
    #{type => hand_after_discard,
      hand => Hand,
      message => <<"Here is your hand after discarding">>}.

-spec discard_prompt(non_neg_integer()) -> discard_prompt_msg().
discard_prompt(Count) ->
    #{type => discard_prompt,
      count => Count,
      message => iolist_to_binary([<<"Discard ">>,
                                  integer_to_list(Count),
                                  <<" cards">>])}.

-spec bidding_complete(game_result()) -> bidding_complete_msg().
bidding_complete(Result) ->
    #{type => bidding_complete,
      result => Result}.

-spec bidding_winner_notification(erlskat:player_id(), integer()) ->
          bidding_winner_notification_msg().
bidding_winner_notification(WinnerId, BidValue) ->
    #{type => bidding_winner_notification,
      winner_id => WinnerId,
      bid_value => BidValue,
      message => iolist_to_binary([<<"Player ">>,
                                  WinnerId,
                                  <<" won the bidding with ">>,
                                  integer_to_list(BidValue)])}.

-spec game_declaration_broadcast(erlskat:player_id(), binary()) ->
          game_declaration_broadcast_msg().
game_declaration_broadcast(WinnerId, Choice) ->
    WinnerIdBin = case is_atom(WinnerId) of
        true -> atom_to_binary(WinnerId, utf8);
        false -> WinnerId
    end,
    #{type => game_declaration_broadcast,
      winner_id => WinnerId,
      choice => Choice,
      message => iolist_to_binary([<<"Player ">>,
                                  WinnerIdBin,
                                  <<" chose to play ">>,
                                  Choice])}.

-spec game_type_broadcast(erlskat:player_id(), erlskat:game_type()) ->
          game_type_broadcast_msg().
game_type_broadcast(WinnerId, GameType) ->
    WinnerIdBin = case is_atom(WinnerId) of
        true -> atom_to_binary(WinnerId, utf8);
        false -> WinnerId
    end,
    #{type => game_type_broadcast,
      winner_id => WinnerId,
      game_type => GameType,
      message => iolist_to_binary([<<"Player ">>,
                                  WinnerIdBin,
                                  <<" chose ">>,
                                  atom_to_binary(GameType, utf8),
                                  <<" game">>])}.

-spec hand_reorder_broadcast(
        erlskat:player_id(),
        erlskat:game_type(),
        map(),
        erlskat:skat(),
        boolean(),
        erlskat:player_id()) ->
          hand_reorder_broadcast_msg().
hand_reorder_broadcast(
  WinnerId,
  GameType,
  Hand,
  Skat,
  IsHandGame,
  ReceivingPlayerId) ->
    FormattedHand = case ReceivingPlayerId of
                        WinnerId when not IsHandGame ->
                           erlskat_card_ordering:order_cards_for_game_type(
                             maps:get(hand, Hand) ++ Skat,
                             GameType);
                        _ ->
                            %
                            maps:get(hand, Hand)
                    end,
    #{type => hand_reorder_broadcast,
      winner_id => WinnerId,
      game_type => GameType,
      hand => FormattedHand,
      message => iolist_to_binary([<<"Hand reordered for ">>,
                                  atom_to_binary(GameType, utf8),
                                  <<" game">>])}.

-spec bid_broadcast(erlskat:player(), integer()) -> bid_broadcast_msg().
bid_broadcast(BiddingPlayer, BidValue) ->
    #{type => bid_broadcast,
      bidder => maps:without([socket], BiddingPlayer),
      bid_value => BidValue,
      message => iolist_to_binary([<<"Player ">>,
                                  maps:get(id, BiddingPlayer),
                                  <<" bids ">>,
                                  integer_to_list(BidValue)])}.

-spec pass_broadcast(erlskat:player(), integer()) -> pass_broadcast_msg().
pass_broadcast(PassingPlayer, BidValue) ->
    #{type => pass_broadcast,
      passer => maps:without([socket], PassingPlayer),
      bid_value => BidValue,
      message => iolist_to_binary([<<"Player ">>,
                                  maps:get(id, PassingPlayer),
                                  <<" passes at ">>,
                                  integer_to_list(BidValue)])}.

-spec bidding_roles(#{erlskat:player_id() => speaking | listening | waiting}) ->
          bidding_roles_msg().
bidding_roles(RoleMap) ->
    #{type => bidding_roles,
      roles => RoleMap}.

-spec cards_dealt(erlskat:cards()) -> cards_dealt_msg().
cards_dealt(Hand) ->
    #{type => cards_dealt,
      hand => Hand}.

%% Game play message constructors
-spec card_play_prompt(erlskat:cards(), [map()], [erlskat:card()], [integer()]) ->
          card_play_prompt_msg().
card_play_prompt(PlayerHand, CurrentTrick, _CardOrdering, PlayableIndexes) ->
    #{type => card_play_prompt,
      hand => PlayerHand,
      current_trick => CurrentTrick,
      valid_cards => PlayableIndexes,
      message => case length(CurrentTrick) of
          0 -> <<"Play a card to lead the trick">>;
          _ -> <<"Play a card to follow">>
      end}.

-spec awaiting_card(erlskat:player_id()) -> awaiting_card_msg().
awaiting_card(WaitingForPlayerId) ->
    #{type => awaiting_card,
      waiting_for_player_id => WaitingForPlayerId,
      message => iolist_to_binary([<<"Waiting for player ">>,
                                  WaitingForPlayerId,
                                  <<" to play...">>])}.

-spec game_start_broadcast(erlskat:player_id(), erlskat:game_type(), boolean(), [atom()]) ->
          game_start_broadcast_msg().
game_start_broadcast(Declarer, GameType, IsHandGame, SelectedMultipliers) ->
    HandGameText = case IsHandGame of
        true -> <<" (hand game)">>;
        false -> <<"">>
    end,
    MultiplierText = case SelectedMultipliers of
        [] -> <<"">>;
        _ -> iolist_to_binary(
               [<<" with ">>,
                lists:join(
                  <<", ">>,
                  [atom_to_binary(M, utf8) || M <- SelectedMultipliers])])
    end,
    DeclarerBin = case is_atom(Declarer) of
        true -> atom_to_binary(Declarer, utf8);
        false -> Declarer
    end,
    #{type => game_start_broadcast,
      declarer => Declarer,
      game_type => GameType,
      is_hand_game => IsHandGame,
      selected_multipliers => SelectedMultipliers,
      message => iolist_to_binary([<<"Game starting: ">>,
                                  DeclarerBin,
                                  <<" is playing ">>,
                                  atom_to_binary(GameType, utf8),
                                  HandGameText,
                                  MultiplierText])}.

-spec card_played_broadcast(erlskat:player_id(), erlskat:card(), integer()) ->
          card_played_broadcast_msg().
card_played_broadcast(PlayerId, Card, CardIndex) ->
    CardText = iolist_to_binary([
        atom_to_binary(maps:get(rank, Card), utf8),
        <<" of ">>,
        atom_to_binary(maps:get(suit, Card), utf8)
    ]),
    #{type => card_played_broadcast,
      player_id => PlayerId,
      card => Card,
      message => iolist_to_binary([PlayerId,
                                  <<" played ">>,
                                  CardText])}.

-spec trick_won_broadcast(erlskat:player_id(), [map()]) -> trick_won_broadcast_msg().
trick_won_broadcast(WinnerId, Trick) ->
    #{type => trick_won_broadcast,
      winner_id => WinnerId,
      trick => Trick,
      message => iolist_to_binary([WinnerId, <<" won the trick">>])}.

-spec game_complete_broadcast(erlskat_hand:game_result()) -> game_complete_broadcast_msg().
game_complete_broadcast(GameResult) ->
    Declarer = maps:get(declarer, GameResult),
    DeclarerWon = maps:get(declarer_won, GameResult),
    DeclarerPoints = maps:get(declarer_points, GameResult),
    GameType = maps:get(game_type, GameResult),

    ResultText = case DeclarerWon of
        true -> iolist_to_binary(
                  [Declarer,
                   <<" won the ">>,
                   atom_to_binary(GameType, utf8),
                   <<" game with ">>,
                   integer_to_list(DeclarerPoints),
                   <<" points">>]);
        false -> iolist_to_binary(
                   [Declarer,
                    <<" lost the ">>,
                    atom_to_binary(GameType, utf8),
                    <<" game with only ">>,
                    integer_to_list(DeclarerPoints),
                    <<" points">>])
    end,

    #{type => game_complete_broadcast,
      result => GameResult,
      message => ResultText}.

-spec invalid_card_error() -> invalid_card_error_msg().
invalid_card_error() ->
    #{type => invalid_card_error,
      message => <<"Invalid card index - please select a valid card">>}.

-spec card_play_error(binary()) -> card_play_error_msg().
card_play_error(Reason) ->
    #{type => card_play_error,
      reason => Reason,
      message => iolist_to_binary([<<"Card play error: ">>, Reason])}.

%% Connection management message constructors
-spec player_disconnected(erlskat:player_id(), integer()) ->
          player_disconnected_msg().
player_disconnected(PlayerId, ReconnectionDeadlineMs) ->
    #{type => player_disconnected,
      player_id => PlayerId,
      reconnection_deadline_ms => ReconnectionDeadlineMs}.

-spec player_reconnected(erlskat:player_id(), [erlskat:player_id()]) ->
          player_reconnected_msg().
player_reconnected(PlayerId, RemainingReconnectingPlayers) ->
    #{type => player_reconnected,
      player_id => PlayerId,
      remaining_reconnecting_players => RemainingReconnectingPlayers}.

-spec player_timed_out(erlskat:player_id()) -> player_timed_out_msg().
player_timed_out(PlayerId) ->
    #{type => player_timed_out,
      player_id => PlayerId}.

-spec game_closed() -> game_closed_msg().
game_closed() ->
    #{type => game_closed, message => <<"Returning to the lobby...">>}.

%% Lobby message constructors
-spec lobby_waiting([erlskat:player_id()]) -> lobby_status_msg().
lobby_waiting(PlayerIds) ->
    #{type => lobby_status,
      state => waiting,
      players => PlayerIds}.

-spec lobby_matched([erlskat:player_id()]) -> lobby_status_msg().
lobby_matched(PlayerIds) ->
    #{type => lobby_status,
      state => matched,
      players => PlayerIds}.

-spec player_joined(erlskat:player_id()) -> player_joined_msg().
player_joined(PlayerId) ->
    #{type => player_joined,
      player_id => PlayerId}.

%% Table message constructors
-spec table_started([erlskat:player_id()]) -> table_started_msg().
table_started(PlayerIds) ->
    #{type => table_started,
      players => PlayerIds}.

%% Scorecard message constructors
-spec scores_update_broadcast([#{player_id := erlskat:player_id(), score := integer()}]) ->
          scores_update_broadcast_msg().
scores_update_broadcast(PlayerScores) ->
    #{type => scores_update_broadcast,
      player_scores => PlayerScores,
      message => <<"Updated player scores">>}.

-spec next_hand_starting_broadcast(integer()) -> next_hand_starting_broadcast_msg().
next_hand_starting_broadcast(HandNumber) ->
    #{type => next_hand_starting_broadcast,
      hand_number => HandNumber,
      message => iolist_to_binary([<<"Hand ">>,
                                  integer_to_list(HandNumber),
                                  <<" starting">>])}.

%% Error message constructor
-spec error_message(binary(), map()) -> error_msg().
error_message(Message, ExpectedFormat) ->
    #{type => error,
      message => Message,
      expected_format => ExpectedFormat}.

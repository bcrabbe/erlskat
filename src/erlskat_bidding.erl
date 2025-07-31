%%%-------------------------------------------------------------------
%%% @author Ben Crabbe <ben.crabbe.dev@gmail.com>
%%% @copyright (C) 2019, Ben Crabbe
%%% @doc
%%%
%%% @end
%%% Created : 27 Jan 2019 by Ben Crabbe <ben.crabbe.dev@gmail.com>
%%%-------------------------------------------------------------------
-module(erlskat_bidding).

-behaviour(gen_statem).
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/2]).

%% Test exports
-export([order_cards_for_skat/1,
         order_cards_for_game_type/2,
         reorder_all_hands_for_game_type/2,
         get_next_valid_bid/1,
         get_next_bidding_pair/2,
         shuffled_deck/0,
         deal/1,
         get_player_by_id/2,
         send_initial_choice_prompt_to_player/1,
         send_game_type_prompt_to_player/2,
         send_multiplier_prompt_to_player/3,
         broadcast_game_declaration_to_other_players/3,
         broadcast_game_type_to_other_players/3,
         get_expected_message_format/1]).

-export_type([game_response/0]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3]).

%% State callbacks
-export([bidding_phase/3, game_declaration/3, skat_exchange/3,
         game_type_selection/3, multiplier_selection/3, completed/3]).

-define(SERVER, ?MODULE).

%% Type definitions
-type color_game() :: erlskat:suit().
-type game_type() :: color_game() | grand | null | null_ouvert | hand_game.
-type game_value() ::
        18 | 20 | 22 | 23 | 24 | 27 | 30 | 33 | 35 | 36 | 40 | 44 | 45 |
        48 | 50 | 54 | 55 | 59 | 60 | 63 | 66 | 70 | 72 | 77 | 80 | 81 |
        84 | 88 | 90 | 96 | 99 | 100 | 108 | 110 | 117 | 120 | 121 |
        126 | 130 | 132 | 135 | 140 | 143 | 144 | 150 | 154 | 156 | 162 |
        165 | 168 | 170 | 176 | 180 | 187 | 192 | 198 | 204 | 216 | 220 |
        225 | 228 | 234 | 240 | 264 | 270 | 273 | 288 | 300 | 306 | 315 |
        330 | 336 | 360 | 363 | 384 | 396 | 405 | 432 | 441 | 450 | 462 |
        480 | 495 | 540 | 546 | 567 | 576 | 594 | 600 | 612 | 624 | 720 |
        792 | 882 | 1080 | 1188 | 1200 | 1296 | 1320 | 1440 | 1584 | 1764.

-type server_state() :: bidding_phase | game_declaration | skat_exchange |
                        game_type_selection | multiplier_selection | completed.



-type player_bidding_data() :: #{player := erlskat:player(),
                                 initial_role := initial_bidding_role(),
                                 current_role := bidding_role(),
                                 hand := erlskat:cards()}.

-type initial_bidding_role() :: deals | listens | speaks.
-type bidding_role() :: initial_bidding_role() | counter_speaks | passed.

-type game_response() ::
        #{state => map(),
          players => list(erlskat:player_id())}.

%% Valid bid sequence for Skat
-define(VALID_BIDS, [18, 20, 22, 23, 24, 27, 30, 33, 35, 36, 40, 44, 45, 46, 48,
                     50, 54, 55, 59, 60, 63, 66, 70, 72, 77, 80, 81, 84, 88, 90,
                     96, 99, 100, 108, 110, 117, 120]).

-define(GAME_TYPES, [<<"grand">>,
                     <<"clubs">>,
                     <<"spades">>,
                     <<"hearts">>,
                     <<"diamonds">>,
                     <<"null">>,
                     <<"null_ouvert">>,
                     <<"hand_game">>]).

-define(REGULAR_GAME_TYPES, [<<"grand">>,
                             <<"clubs">>,
                             <<"spades">>,
                             <<"hearts">>,
                             <<"diamonds">>,
                             <<"null">>]).

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
%%% API
%%%===================================================================

-spec start_link(pid(), list(erlskat:players())) ->
          {ok, Pid :: pid()} |
          ignore |
          {error, Error :: term()}.
start_link(CoordinatorPid, Players) ->
    gen_statem:start_link(?MODULE, {CoordinatorPid, Players}, []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> state_functions.

-spec init(list(Players :: list(erlskat:player()))) ->
          gen_statem:init_result(bidding_phase).
init({CoordinatorPid, Players}) ->
    process_flag(trap_exit, true),
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                players => Players,
                action => new_game_starting}),
    [erlskat_manager:update_player_proc(Player, self()) || Player <- Players],
    #{hands := Hands, skat := Skat} = deal(Players),
    % Determine bidding order based on dealer position
    % (middlehand, rearhand, forehand)
    [Player1, Player2, Player3] = Players,
    BiddingOrder = [maps:get(id, Player2),
                    maps:get(id, Player3),
                    maps:get(id, Player1)],
    [Middlehand, _Rearhand, Forehand] = BiddingOrder,

    InitBiddingData = #{hands => Hands,
                        skat => Skat,
                        bid => 18,
                        coordinator_pid => CoordinatorPid,
                        current_bidder => Middlehand,
                        responding_player => Forehand,
                        passed_players => [],
                        bidding_order => BiddingOrder},

    % Send initial cards to players
    [send_hands_to_players(PlayerBiddingData) ||
        PlayerBiddingData <- Hands],

    % Send bidding role announcements to all players
    send_bidding_roles_to_all_players(Hands, BiddingOrder),

    % Start bidding with middlehand vs forehand
    send_bid_prompt_to_player(get_player_by_id(Middlehand, Hands), 18),
    send_awaiting_bid_to_player(get_player_by_id(Forehand, Hands), 18),

    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                init_bidding_data => InitBiddingData}),
    {ok, bidding_phase, InitBiddingData}.

%%%===================================================================
%%% State callbacks
%%%===================================================================

%% State: bidding_phase
bidding_phase(cast, {socket_message, #{player := Player, msg := <<"hold">>}}, Data) ->
    PlayerId = maps:get(id, Player),
    case PlayerId =:= maps:get(current_bidder, Data) of
        true ->
            handle_bidder_accept(Player, Data);
        false ->
            keep_state_and_data
    end;

bidding_phase(cast, {socket_message, #{player := Player, msg := <<"pass">>}}, Data) ->
    PlayerId = maps:get(id, Player),
    case PlayerId =:= maps:get(current_bidder, Data) of
        true ->
            handle_bidder_pass(Player, Data);
        false ->
            case PlayerId =:= maps:get(responding_player, Data) of
                true ->
                    handle_responder_pass(Player, Data);
                false ->
                    keep_state_and_data
            end
    end;

bidding_phase(EventType, Event, Data) ->
    handle_unexpected_event(EventType, Event, Data, bidding_phase).

%% State: game_declaration winning bidder chooses to see skat or play hand
game_declaration(cast,
                 {socket_message,
                  #{player := Player, msg := Choice}} = Msg,
                 Data) ->
    PlayerId = maps:get(id, Player),
    case PlayerId =:= maps:get(highest_bidder, Data) of
        true ->
            case Choice of
                <<"hand">> ->
                    % Player chooses to play hand game
                    broadcast_game_declaration_to_other_players(
                        maps:get(hands, Data), PlayerId, <<"hand">>),
                    send_game_type_prompt_to_player(
                        get_player_by_id(PlayerId, maps:get(hands, Data)),
                        ?REGULAR_GAME_TYPES),
                    {next_state, game_type_selection,
                     Data#{is_hand_game => true,
                           game_declaration_step => game_type_choice,
                           selected_multipliers => []}};
                <<"skat">> ->
                    % Player chooses to see skat
                    broadcast_game_declaration_to_other_players(
                        maps:get(hands, Data), PlayerId, <<"skat">>),
                    PlayerHand = get_player_by_id(PlayerId, maps:get(hands, Data)),
                    send_skat_cards_to_player(
                        PlayerHand,
                        maps:get(hand, PlayerHand),
                        maps:get(skat, Data)),
                    send_game_type_prompt_to_player(
                        get_player_by_id(PlayerId, maps:get(hands, Data)),
                        ?REGULAR_GAME_TYPES),
                    {next_state, game_type_selection,
                     Data#{is_hand_game => false,
                           game_declaration_step => game_type_choice,
                           selected_multipliers => []}};
                _ ->
                    handle_unexpected_event(cast, Msg, Data, game_declaration),
                    keep_state_and_data
            end;
        false ->
            keep_state_and_data
    end;
game_declaration(EventType, Event, Data) ->
    handle_unexpected_event(EventType, Event, Data, game_declaration).


%% State: skat_exchange
skat_exchange(cast,
              {socket_message,
               #{player := Player, msg := Indices}} = Msg,
              Data) ->
    PlayerId = maps:get(id, Player),
    case PlayerId =:= maps:get(highest_bidder, Data) of
        true ->
            case is_list(Indices) andalso length(Indices) =:= 2 of
                true ->
                    % Get the player's full hand (including skat)
                    PlayerHand = get_player_by_id(PlayerId, maps:get(hands, Data)),
                    FullHand = maps:get(hand, PlayerHand),
                    Skat = maps:get(skat, Data),
                    OrderedHand = order_cards_for_skat(FullHand ++ Skat),

                    % Extract the discarded cards by index
                    DiscardedCards = [lists:nth(Index + 1, OrderedHand) ||
                                     Index <- Indices],

                    % Complete the bidding process
                    complete_bidding(Data#{discarded_cards => DiscardedCards});
                false ->
                    handle_unexpected_event(cast, Msg, Data, skat_exchange),
                    keep_state_and_data
            end;
        false ->
            keep_state_and_data
    end;

skat_exchange(EventType, Event, Data) ->
    handle_unexpected_event(EventType, Event, Data, skat_exchange).

%% State: game_type_selection
game_type_selection(cast,
                    {socket_message,
                     #{player := Player, msg := GameType}} = Msg,
                    Data) ->
    PlayerId = maps:get(id, Player),
    case PlayerId =:= maps:get(highest_bidder, Data) andalso
         lists:member(GameType, ?REGULAR_GAME_TYPES) of
        true ->
            handle_game_type_selection(PlayerId, GameType, Data);
        false ->
            handle_unexpected_event(cast, Msg, Data, game_type_selection),
            keep_state_and_data
    end;

game_type_selection(EventType, Event, Data) ->
    handle_unexpected_event(EventType, Event, Data, game_type_selection).

%% State: multiplier_selection
multiplier_selection(cast,
                     {socket_message,
                      #{player := Player, msg := Multiplier}} = Msg,
                     Data) ->
    PlayerId = maps:get(id, Player),
    case PlayerId =:= maps:get(highest_bidder, Data) of
        true ->
            ValidMultipliers = [<<"schnieder">>, <<"schwartz">>, <<"ouvert">>, <<"skip">>],
            case lists:member(Multiplier, ValidMultipliers) of
                true ->
                    case Multiplier of
                        <<"skip">> ->
                            handle_multiplier_skip(Player, Data);
                        _ ->
                            handle_multiplier_selection(Player, Multiplier, Data)
                    end;
                false ->
                    handle_unexpected_event(cast, Msg, Data, multiplier_selection),
                    keep_state_and_data
            end;
        false ->
            keep_state_and_data
    end;

multiplier_selection(EventType, Event, Data) ->
    handle_unexpected_event(EventType, Event, Data, multiplier_selection).

%% State: completed
completed(EventType, Event, Data) ->
    handle_unexpected_event(EventType, Event, Data, completed).

-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->
          any().
terminate(_Reason, _State, _Data) ->
    void.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_bidder_accept(Player, Data) ->
    CurrentBid = maps:get(bid, Data),
    NextBid = get_next_valid_bid(CurrentBid),
    case NextBid of
        none ->
            % No higher bids possible, current bidder wins
            Winner = maps:get(current_bidder, Data),
            transition_to_game_declaration(Winner, Data);
        ValidNextBid ->
            % Continue bidding
            broadcast_bid_to_all_players(maps:get(hands, Data), Player, ValidNextBid),
            send_bid_prompt_to_player(
                get_player_by_id(maps:get(responding_player, Data),
                                maps:get(hands, Data)),
                ValidNextBid),
            send_awaiting_bid_to_player(
                get_player_by_id(maps:get(current_bidder, Data),
                                maps:get(hands, Data)),
                ValidNextBid),
            {keep_state, Data#{bid => ValidNextBid,
                               current_bidder => maps:get(responding_player, Data),
                               responding_player => maps:get(current_bidder, Data)}}
    end.

handle_bidder_pass(Player, Data) ->
    broadcast_pass_to_all_players(maps:get(hands, Data), Player),
    PassedPlayers = [maps:get(id, Player) | maps:get(passed_players, Data)],
    case get_next_bidding_pair(maps:get(bidding_order, Data), PassedPlayers) of
        {NewCurrentBidder, NewRespondingPlayer} ->
            % Continue bidding with new pair
            send_bid_prompt_to_player(
                get_player_by_id(NewCurrentBidder, maps:get(hands, Data)),
                maps:get(bid, Data)),
            send_awaiting_bid_to_player(
                get_player_by_id(NewRespondingPlayer, maps:get(hands, Data)),
                maps:get(bid, Data)),
            {keep_state, Data#{current_bidder => NewCurrentBidder,
                               responding_player => NewRespondingPlayer,
                               passed_players => PassedPlayers}};
        no_more_bidders ->
            % Only one player left, they win
            Winner = maps:get(responding_player, Data),
            transition_to_game_declaration(Winner, Data)
    end.

handle_responder_pass(Player, Data) ->
    broadcast_pass_to_all_players(maps:get(hands, Data), Player),
    PassedPlayers = [maps:get(id, Player) | maps:get(passed_players, Data)],
    case get_next_bidding_pair(maps:get(bidding_order, Data), PassedPlayers) of
        {NewCurrentBidder, NewRespondingPlayer} ->
            % Continue bidding with current bidder vs new responder
            send_bid_prompt_to_player(
                get_player_by_id(NewCurrentBidder, maps:get(hands, Data)),
                maps:get(bid, Data)),
            send_awaiting_bid_to_player(
                get_player_by_id(NewRespondingPlayer, maps:get(hands, Data)),
                maps:get(bid, Data)),
            {keep_state, Data#{current_bidder => NewCurrentBidder,
                               responding_player => NewRespondingPlayer,
                               passed_players => PassedPlayers}};
        no_more_bidders ->
            % Current bidder wins
            Winner = maps:get(current_bidder, Data),
            transition_to_game_declaration(Winner, Data)
    end.

transition_to_game_declaration(Winner, Data) ->
    WinnerPlayer = get_player_by_id(Winner, maps:get(hands, Data)),
    Hands = maps:get(hands, Data),
    BidValue = maps:get(bid, Data),

    % Send initial choice prompt to the winner
    send_initial_choice_prompt_to_player(WinnerPlayer),

    % Send notification to non-playing players about the winner
    [send_bidding_winner_notification_to_player(Hand, Winner, BidValue) ||
        Hand <- Hands,
        maps:get(id, maps:get(player, Hand)) =/= Winner],

    NewData = Data#{highest_bidder => Winner,
                   game_declaration_step => initial_choice},
    {next_state, game_declaration, NewData}.

handle_multiplier_selection(Player, Multiplier, Data) ->
    PlayerId = maps:get(id, Player),
    CurrentMultipliers = maps:get(selected_multipliers, Data, []),
    GameType = maps:get(chosen_game, Data),
    _IsHandGame = maps:get(is_hand_game, Data, false),

    case Multiplier of
        <<"ouvert">> ->
            case GameType of
                <<"null">> ->
                    % For null games, ouvert completes the selection
                    complete_game_declaration(Data#{selected_multipliers =>
                                                   [ouvert | CurrentMultipliers]});
                _ ->
                    % For other games, ouvert is only available after schwartz
                    case lists:member(schwartz, CurrentMultipliers) of
                        true ->
                            complete_game_declaration(Data#{selected_multipliers =>
                                                           [ouvert | CurrentMultipliers]});
                        false ->
                            keep_state_and_data
                    end
            end;
        <<"schnieder">> ->
            % Offer schwartz next
            send_multiplier_prompt_to_player(
                get_player_by_id(PlayerId, maps:get(hands, Data)),
                [<<"schwartz">>],
                GameType),
            {keep_state, Data#{selected_multipliers => [schnieder | CurrentMultipliers]}};
        <<"schwartz">> ->
            % Offer ouvert next
            send_multiplier_prompt_to_player(
                get_player_by_id(PlayerId, maps:get(hands, Data)),
                [<<"ouvert">>],
                GameType),
            {keep_state, Data#{selected_multipliers => [schwartz | CurrentMultipliers]}};
        _ ->
            keep_state_and_data
    end.

handle_multiplier_skip(_Player, Data) ->
    % Complete the game declaration with current selections
    complete_game_declaration(Data).

handle_game_type_selection(PlayerId, GameType, Data) ->
    % Broadcast the game type choice to other players
    broadcast_game_type_to_other_players(
        maps:get(hands, Data), PlayerId, GameType),

    % Reorder all hands according to the chosen game type
    CurrentHands = maps:get(hands, Data),
    ReorderedHands = reorder_all_hands_for_game_type(CurrentHands, GameType),
    
    % Broadcast the reordered hands to all players
    broadcast_hand_reorder_to_all_players(ReorderedHands, PlayerId, GameType),
    
    UpdatedData = Data#{hands => ReorderedHands},

    case GameType of
        <<"null">> ->
            % For null games, offer ouvert option
            send_multiplier_prompt_to_player(
                get_player_by_id(PlayerId, ReorderedHands),
                [<<"ouvert">>],
                <<"null">>),
            {next_state, multiplier_selection,
             UpdatedData#{chosen_game => GameType,
                          game_declaration_step => multiplier_choice}};
        _ ->
            % For other games, offer schnieder if hand game
            case maps:get(is_hand_game, Data, false) of
                true ->
                    send_multiplier_prompt_to_player(
                        get_player_by_id(PlayerId, ReorderedHands),
                        [<<"schnieder">>],
                        GameType),
                    {next_state, multiplier_selection,
                     UpdatedData#{chosen_game => GameType,
                                  game_declaration_step => multiplier_choice}};
                false ->
                    % Not hand game, complete with skat exchange
                    send_discard_prompt_to_player(
                        get_player_by_id(PlayerId, ReorderedHands),
                        2),
                    {next_state, skat_exchange,
                     UpdatedData#{chosen_game => GameType}}
            end
    end.

complete_game_declaration(Data) ->
    IsHandGame = maps:get(is_hand_game, Data, false),
    case IsHandGame of
        true ->
            % Hand game - complete immediately
            complete_bidding(Data);
        false ->
            % Not hand game - need to exchange skat
            PlayerId = maps:get(highest_bidder, Data),
            send_discard_prompt_to_player(
                get_player_by_id(PlayerId, maps:get(hands, Data)),
                2),
            {next_state, skat_exchange, Data}
    end.

get_next_bidding_pair([Middlehand, Rearhand, Forehand], PassedPlayers) ->
    ActivePlayers = [P || P <- [Middlehand, Rearhand, Forehand],
                          not lists:member(P, PassedPlayers)],
    case length(ActivePlayers) of
        0 -> no_more_bidders;
        1 -> no_more_bidders;
        2 ->
            % Standard Skat bidding logic: middlehand bids first against forehand,
            % then winner against rearhand
            case {lists:member(Middlehand, ActivePlayers),
                  lists:member(Forehand, ActivePlayers),
                  lists:member(Rearhand, ActivePlayers)} of
                {true, true, false} -> {Middlehand, Forehand};
                {false, true, true} -> {Rearhand, Forehand};
                {true, false, true} -> {Rearhand, Middlehand};
                _ -> no_more_bidders
            end;
        3 ->
            % All players active, follow standard order: middlehand vs forehand first
            {Middlehand, Forehand}
    end.

get_next_valid_bid(CurrentBid) ->
    ValidBids = [B || B <- ?VALID_BIDS, B > CurrentBid],
    case ValidBids of
        [] -> none;
        [Next | _] -> Next
    end.

complete_bidding(Data) ->
    % Send completion message to coordinator
    Result = #{winner => maps:get(highest_bidder, Data),
               final_bid => maps:get(bid, Data),
               chosen_game => maps:get(chosen_game, Data),
               discarded_cards => maps:get(discarded_cards, Data, []),
               skat_cards => maps:get(skat, Data),
               player_hands => maps:get(hands, Data),
               is_hand_game => maps:get(is_hand_game, Data, false),
               selected_multipliers => maps:get(selected_multipliers, Data, [])},

    CoordinatorPid = maps:get(coordinator_pid, Data),
    CoordinatorPid ! {bidding_complete, Result},

    % Notify all players of completion
    [send_bidding_complete_to_player(Hand, Result) || Hand <- maps:get(hands, Data)],

    {next_state, completed, Data}.

%%%===================================================================
%%% Socket messages
%%%===================================================================

-spec send_hands_to_players(player_bidding_data()) -> done.
send_hands_to_players(#{player := Player} = PlayerBiddingData) ->
    send_hands_to_players(Player, PlayerBiddingData);
send_hands_to_players(_) ->
    done.

-spec send_hands_to_players(erlskat:player(), player_bidding_data()) -> done.
send_hands_to_players(#{socket := Socket}, #{hand := Hand}) ->
    Socket ! erlskat_client_responses:cards_dealt(Hand),
    done.

-spec send_bid_prompt_to_player(player_bidding_data(), game_value()) -> done.
send_bid_prompt_to_player(#{player := #{socket := Socket}}, BidValue) ->
    Socket ! erlskat_client_responses:bid_prompt(BidValue),
    done.

-spec send_awaiting_bid_to_player(player_bidding_data(), game_value()) -> done.
send_awaiting_bid_to_player(#{player := #{socket := Socket}}, BidValue) ->
    Socket ! erlskat_client_responses:awaiting_bid(BidValue),
    done.

-spec send_game_type_prompt_to_player(player_bidding_data(), [game_type()]) -> done.
send_game_type_prompt_to_player(#{player := #{socket := Socket}}, GameTypes) ->
    Socket ! erlskat_client_responses:game_type_prompt(GameTypes),
    done.

-spec send_multiplier_prompt_to_player(player_bidding_data(), [binary()], binary()) -> done.
send_multiplier_prompt_to_player(#{player := #{socket := Socket}}, Multipliers, GameType) ->
    Socket ! erlskat_client_responses:multiplier_prompt(Multipliers, GameType),
    done.

-spec send_initial_choice_prompt_to_player(player_bidding_data()) -> done.
send_initial_choice_prompt_to_player(#{player := #{socket := Socket}}) ->
    Socket ! erlskat_client_responses:initial_choice_prompt(),
    done.

% First show skat cards, then send combined hand
-spec send_skat_cards_to_player(player_bidding_data(), erlskat:cards(), erlskat:skat()) -> done.
send_skat_cards_to_player(
  #{player := #{socket := Socket}, hand := HandCards},
  HandCards,
  SkatCards) ->
    Socket ! erlskat_client_responses:skat_flipped(SkatCards),
    FullHand = order_cards_for_skat(HandCards ++ SkatCards),
    Socket ! erlskat_client_responses:hand_with_skat(FullHand),
    done.

-spec send_discard_prompt_to_player(player_bidding_data(), non_neg_integer()) -> done.
send_discard_prompt_to_player(#{player := #{socket := Socket}}, Count) ->
    Socket ! erlskat_client_responses:discard_prompt(Count),
    done.

-spec send_bidding_complete_to_player(player_bidding_data(), map()) -> done.
send_bidding_complete_to_player(#{player := #{socket := Socket}}, Result) ->
    Socket ! erlskat_client_responses:bidding_complete(Result),
    done.

% Create a map from card to its position in the ordering
send_bidding_winner_notification_to_player(#{player := #{socket := Socket}}, WinnerId, BidValue) ->
    Socket ! erlskat_client_responses:bidding_winner_notification(WinnerId, BidValue),
    done.

-spec broadcast_bid_to_all_players([player_bidding_data()], erlskat:player(), game_value()) -> done.
broadcast_bid_to_all_players(Hands, BiddingPlayer, BidValue) ->
    BroadcastMsg = erlskat_client_responses:bid_broadcast(BiddingPlayer, BidValue),
    [send_broadcast_msg(Hand, BroadcastMsg) || Hand <- Hands],
    done.

-spec broadcast_pass_to_all_players([player_bidding_data()], erlskat:player()) -> done.
broadcast_pass_to_all_players(Hands, PassingPlayer) ->
    BroadcastMsg = erlskat_client_responses:pass_broadcast(PassingPlayer),
    [send_broadcast_msg(Hand, BroadcastMsg) || Hand <- Hands],
    done.

-spec broadcast_game_declaration_to_other_players(
    [player_bidding_data()], erlskat:player_id(), binary()) -> done.
broadcast_game_declaration_to_other_players(Hands, WinnerId, Choice) ->
    BroadcastMsg = erlskat_client_responses:game_declaration_broadcast(WinnerId, Choice),
    [send_broadcast_msg(Hand, BroadcastMsg) ||
        Hand <- Hands,
        maps:get(id, maps:get(player, Hand)) =/= WinnerId],
    done.

-spec broadcast_game_type_to_other_players(
    [player_bidding_data()], erlskat:player_id(), binary()) -> done.
broadcast_game_type_to_other_players(Hands, WinnerId, GameType) ->
    BroadcastMsg = erlskat_client_responses:game_type_broadcast(WinnerId, GameType),
    [send_broadcast_msg(Hand, BroadcastMsg) ||
        Hand <- Hands,
        maps:get(id, maps:get(player, Hand)) =/= WinnerId],
    done.

-spec broadcast_hand_reorder_to_all_players(
    [player_bidding_data()], erlskat:player_id(), binary()) -> done.
broadcast_hand_reorder_to_all_players(Hands, WinnerId, GameType) ->
    BroadcastMsg = erlskat_client_responses:hand_reorder_broadcast(WinnerId, GameType, Hands),
    [send_broadcast_msg(Hand, BroadcastMsg) || Hand <- Hands],
    done.

-spec send_broadcast_msg(player_bidding_data(), map()) -> done.
send_broadcast_msg(#{player := #{socket := Socket}}, BroadcastMsg) ->
    Socket ! BroadcastMsg,
    done.

-spec send_bidding_roles_to_all_players([player_bidding_data()], [erlskat:player_id()]) -> done.
send_bidding_roles_to_all_players(Hands, BiddingOrder) ->
    [Middlehand, Rearhand, Forehand] = BiddingOrder,
    RoleMap = #{Middlehand => speaking,
                Rearhand => waiting,
                Forehand => listening},
    RoleMsg = erlskat_client_responses:bidding_roles(RoleMap),
    [send_broadcast_msg(Hand, RoleMsg) || Hand <- Hands],
    done.

-spec send_error_message_to_player(player_bidding_data(), binary(), map()) -> done.
send_error_message_to_player(#{player := #{socket := Socket}}, ErrorMessage, ExpectedFormat) ->
    Socket ! erlskat_client_responses:error_message(ErrorMessage, ExpectedFormat),
    done.

-spec send_error_message_to_player_by_id(erlskat:player_id(), [player_bidding_data()],
                                        binary(), map()) -> done.
send_error_message_to_player_by_id(PlayerId, Hands, ErrorMessage, ExpectedFormat) ->
    case get_player_by_id(PlayerId, Hands) of
        undefined -> done;
        PlayerHand -> send_error_message_to_player(PlayerHand, ErrorMessage,
                                                  ExpectedFormat)
    end.

-spec get_expected_message_format(server_state()) -> map().
get_expected_message_format(bidding_phase) ->
    #{<<"hold">> => <<"Accept the current bid">>,
      <<"pass">> => <<"Pass on the current bid">>};
get_expected_message_format(game_declaration) ->
    #{<<"hand">> => <<"leave the skat face down and recieve an extra multiplier">>,
      <<"skat">> => <<"see the skat and then discard 2 cards of your choice">>};
get_expected_message_format(skat_exchange) ->
    <<"Array of 2 card indices to discard">>;
get_expected_message_format(game_type_selection) ->
     [<<"grand">>, <<"clubs">>, <<"spades">>,
      <<"hearts">>, <<"diamonds">>, <<"null">>];
get_expected_message_format(multiplier_selection) ->
    [<<"schnieder">>, <<"schwartz">>, <<"ouvert">>, <<"skip">>];
get_expected_message_format(completed) ->
    #{<<"message">> => <<"No messages expected in completed state">>}.

%%%===================================================================
%%% Helper functions
%%%===================================================================

-spec order_cards_for_skat(erlskat:cards()) -> erlskat:cards().
order_cards_for_skat(Cards) ->
    % Sort cards according to their position in the ordering
    OrderMap = maps:from_list([{Card, Index} || {Index, Card} <- lists:enumerate(?SKAT_ORDERING)]),

    % Sort cards according to their position in the ordering
    lists:sort(fun(Card1, Card2) ->
                       maps:get(Card1, OrderMap, 999) =< maps:get(Card2, OrderMap, 999)
               end, Cards).

-spec order_cards_for_game_type(erlskat:cards(), binary()) -> erlskat:cards().
order_cards_for_game_type(Cards, GameType) ->
    Ordering = get_card_ordering_for_game_type(GameType),
    OrderMap = maps:from_list([{Card, Index} || {Index, Card} <- lists:enumerate(Ordering)]),
    lists:sort(fun(Card1, Card2) ->
                       maps:get(Card1, OrderMap, 999) =< maps:get(Card2, OrderMap, 999)
               end, Cards).

-spec get_card_ordering_for_game_type(binary()) -> [erlskat:card()].
get_card_ordering_for_game_type(<<"clubs">>) ->
    get_suit_game_ordering(clubs);
get_card_ordering_for_game_type(<<"spades">>) ->
    get_suit_game_ordering(spades);
get_card_ordering_for_game_type(<<"hearts">>) ->
    get_suit_game_ordering(hearts);
get_card_ordering_for_game_type(<<"diamonds">>) ->
    get_suit_game_ordering(diamonds);
get_card_ordering_for_game_type(<<"grand">>) ->
    get_grand_game_ordering();
get_card_ordering_for_game_type(<<"null">>) ->
    get_null_game_ordering().

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

-spec reorder_all_hands_for_game_type([player_bidding_data()], binary()) -> [player_bidding_data()].
reorder_all_hands_for_game_type(Hands, GameType) ->
    [Hand#{hand => order_cards_for_game_type(maps:get(hand, Hand), GameType)} || Hand <- Hands].

-spec get_player_by_id(erlskat:player_id(), [player_bidding_data()]) ->
          player_bidding_data() | undefined.
get_player_by_id(PlayerId, Hands) ->
    case lists:filter(fun(#{player := #{id := Id}}) -> Id =:= PlayerId end, Hands) of
        [Hand] -> Hand;
        [] -> undefined
    end.

-spec shuffled_deck() -> erlskat:cards().
shuffled_deck() ->
    Deck = [#{rank => Rank, suit => Suit} ||
               Rank <- [ace, seven, eight, nine,
                        ten, jack, queen, king],
               Suit <- [clubs, diamonds, hearts, spades]],
    [ShuffledCard ||
        {_, ShuffledCard} <- lists:sort(
                               [{rand:uniform(), Card} ||
                                   Card <- Deck])].

-spec deal(list(erlskat:players())) ->
          #{hands => [player_bidding_data()],
            skat => erlskat:skat()}.
deal(Players) ->
    Shuffled = shuffled_deck(),
    HandCards = [lists:sublist(Shuffled, 10),
                 lists:sublist(Shuffled, 11, 10),
                 lists:sublist(Shuffled, 21, 10)],
    Skat = lists:sublist(Shuffled, 31, 2),
    Hands = [#{player => Player,
               hand => order_cards_for_skat(Hand)} ||
                {Player,  Hand} <- lists:zip(Players, HandCards)],
    #{hands => Hands,
      skat => Skat}.

-spec handle_unexpected_event(gen_statem:event_type(), term(), term(), server_state()) ->
          gen_statem:event_handler_result(term()).
handle_unexpected_event(cast, {socket_message, #{player := Player, msg := _Msg} = SocketEvent},
                        Data, StateName) ->
    PlayerId = maps:get(id, Player),
    ?LOG_WARNING(#{module => ?MODULE,
                   line => ?LINE,
                   function => ?FUNCTION_NAME,
                   event_type => cast,
                   event => SocketEvent,
                   state => StateName,
                   player_id => PlayerId,
                   action => unexpected_socket_message}),
    ExpectedFormat = get_expected_message_format(StateName),
    ErrorMessage = iolist_to_binary(["Unexpected message in state '",
                                    atom_to_list(StateName),
                                    "'. Please check the expected format."]),
    send_error_message_to_player_by_id(PlayerId, maps:get(hands, Data),
                                      ErrorMessage, ExpectedFormat),
    keep_state_and_data;

handle_unexpected_event(EventType, Event, Data, _StateName) ->
    ?LOG_WARNING(#{module => ?MODULE,
                   line => ?LINE,
                   function => ?FUNCTION_NAME,
                   event_type => EventType,
                   event => Event,
                   state => Data,
                   action => unhandled_event}),
    keep_state_and_data.

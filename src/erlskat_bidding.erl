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

-export_type([game_response/0]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3]).

%% State callbacks
-export([bidding_phase/3, game_declaration/3, skat_exchange/3, completed/3]).

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

-type server_state() :: bidding_phase | game_declaration | skat_exchange | completed.

-type server_data() :: bidding_data().

-type bidding_data() :: #{hands := [player_bidding_data()],
                          bid := game_value(),
                          skat := erlskat:skat(),
                          coordinator_pid := pid(),
                          current_bidder := erlskat:player_id(),
                          responding_player := erlskat:player_id(),
                          passed_players := [erlskat:player_id()],
                          bidding_order := [erlskat:player_id()],
                          highest_bidder => erlskat:player_id(),
                          chosen_game => game_type(),
                          discarded_cards => erlskat:cards()}.

-type player_bidding_data() :: #{player := erlskat:player(),
                                 initial_role := initial_bidding_role(),
                                 current_role := bidding_role(),
                                 hand := erlskat:cards()}.

-type hand_position() :: 0 | 1 | 2.  %% forehand, mittlehand, rearhand
-type initial_bidding_role() :: deals | listens | speaks.
-type bidding_role() :: initial_bidding_role() | counter_speaks | passed.

-type game_response() ::
        #{state => map(),
          players => list(erlskat:player_id())}.

-type bid_message() :: #{bid => game_value() | pass}.

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
    [player_bidding_data_msg(PlayerBiddingData) ||
        PlayerBiddingData <- Hands],

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
bidding_phase(cast, {socket_message, #{player := Player, msg := <<"yes">>}}, Data) ->
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
    handle_unexpected_event(EventType, Event, Data).

%% State: game_declaration
game_declaration(cast,
                 {socket_message,
                  #{player := Player, msg := #{<<"game_type">> := GameType}}},
                 Data) ->
    PlayerId = maps:get(id, Player),
    case PlayerId =:= maps:get(highest_bidder, Data) of
        true ->
            case lists:member(GameType, ?GAME_TYPES) of
                true ->
                    case GameType of
                        hand_game ->
                                                % Player chooses to play without skat
                            complete_bidding(Data#{chosen_game => GameType});
                        _ ->
                                                % Player takes skat, move to exchange phase
                            PlayerHand = get_player_by_id(PlayerId, maps:get(hands, Data)),
                            send_skat_cards_to_player(
                              PlayerHand,
                              maps:get(hand, PlayerHand),
                              maps:get(skat, Data)),
                            send_discard_prompt_to_player(
                              get_player_by_id(PlayerId, maps:get(hands, Data)),
                              2),
                            {next_state, skat_exchange, Data#{chosen_game => GameType}}
                    end;
                false ->
                    keep_state_and_data
            end;
        false ->
            keep_state_and_data
    end;

game_declaration(EventType, Event, Data) ->
    handle_unexpected_event(EventType, Event, Data).

%% State: skat_exchange
skat_exchange(cast,
              {socket_message,
               #{player := Player, msg := #{<<"discard_cards">> := Indices}}},
              Data) ->
    PlayerId = maps:get(id, Player),
    case PlayerId =:= maps:get(highest_bidder, Data) of
        true ->
            case length(Indices) =:= 2 of
                true ->
                                                % Get the player's full hand (including skat)
                    PlayerHand = get_player_by_id(PlayerId, maps:get(hands, Data)),
                    FullHand = maps:get(hand, PlayerHand),
                    Skat = maps:get(skat, Data),
                    OrderedHand = order_cards_for_skat(FullHand ++ Skat),

                                                % Extract the discarded cards by index
                    DiscardedCards = [lists:nth(Index + 1, OrderedHand) || Index <- Indices],

                                                % Complete the bidding process
                    complete_bidding(Data#{discarded_cards => DiscardedCards});
                false ->
                    keep_state_and_data
            end;
        false ->
            keep_state_and_data
    end;

skat_exchange(EventType, Event, Data) ->
    handle_unexpected_event(EventType, Event, Data).

%% State: completed
completed(EventType, Event, Data) ->
    handle_unexpected_event(EventType, Event, Data).

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
                get_player_by_id(maps:get(responding_player, Data), maps:get(hands, Data)),
                ValidNextBid),
            send_awaiting_bid_to_player(
                get_player_by_id(maps:get(current_bidder, Data), maps:get(hands, Data)),
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

                                                % Send game declaration prompt to the winner
    send_game_declaration_prompt_to_player(WinnerPlayer, ?GAME_TYPES),

% Send notification to non-playing players about the winner
    [send_bidding_winner_notification_to_player(Hand, Winner, BidValue) ||
        Hand <- Hands,
        maps:get(id, maps:get(player, Hand)) =/= Winner],

    NewData = Data#{highest_bidder => Winner},
    {next_state, game_declaration, NewData}.

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
               player_hands => maps:get(hands, Data)},

    CoordinatorPid = maps:get(coordinator_pid, Data),
    CoordinatorPid ! {bidding_complete, Result},

                                                % Notify all players of completion
    [send_bidding_complete_to_player(Hand, Result) || Hand <- maps:get(hands, Data)],

    {next_state, completed, Data}.

%%%===================================================================
%%% Socket messages
%%%===================================================================

-spec player_bidding_data_msg(player_bidding_data()) -> done.
player_bidding_data_msg(#{player := Player} = PlayerBiddingData) ->
    player_bidding_data_msg(Player, PlayerBiddingData);
player_bidding_data_msg(_) ->
    done.

-spec player_bidding_data_msg(erlskat:player(), player_bidding_data()) -> done.
player_bidding_data_msg(#{socket := Socket}, PlayerBiddingData) ->
    Msg = maps:without([player], PlayerBiddingData),
    Socket ! Msg,
    done.

-spec send_bid_prompt_to_player(player_bidding_data(), game_value()) -> done.
send_bid_prompt_to_player(#{player := #{socket := Socket}}, BidValue) ->
    BidPrompt = #{type => bid_prompt,
                  bid_value => BidValue,
                  message => iolist_to_binary(["Do you want to bid ",
                                              integer_to_list(BidValue),
                                              "?"])},
    Socket ! BidPrompt,
    done.

-spec send_awaiting_bid_to_player(player_bidding_data(), game_value()) -> done.
send_awaiting_bid_to_player(#{player := #{socket := Socket}}, BidValue) ->
    AwaitingMsg = #{type => awaiting_bid,
                    bid_value => BidValue,
                    message => iolist_to_binary(["Waiting for opponent to bid ",
                                                integer_to_list(BidValue)])},
    Socket ! AwaitingMsg,
    done.

-spec send_game_declaration_prompt_to_player(player_bidding_data(), [game_type()]) -> done.
send_game_declaration_prompt_to_player(#{player := #{socket := Socket}}, GameTypes) ->
    GamePrompt = #{type => game_declaration_prompt,
                   game_types => GameTypes,
                   message => <<"Choose your game type">>},
    Socket ! GamePrompt,
    done.

% Combine hand and skat, then order according to Skat rules
-spec send_skat_cards_to_player(player_bidding_data(), erlskat:cards(), erlskat:skat()) -> done.
send_skat_cards_to_player(
  #{player := #{socket := Socket}, hand := HandCards},
  HandCards,
  SkatCards) ->
    FullHand = order_cards_for_skat(HandCards ++ SkatCards),
    SkatMsg = #{type => skat_cards,
                cards => FullHand,
                message => <<"Here are your cards including skat">>},
    Socket ! SkatMsg,
    done.

-spec send_discard_prompt_to_player(player_bidding_data(), non_neg_integer()) -> done.
send_discard_prompt_to_player(#{player := #{socket := Socket}}, Count) ->
    DiscardPrompt = #{type => discard_prompt,
                      count => Count,
                      message => iolist_to_binary(["Discard ",
                                                  integer_to_list(Count),
                                                  " cards"])},
    Socket ! DiscardPrompt,
    done.

-spec send_bidding_complete_to_player(player_bidding_data(), map()) -> done.
send_bidding_complete_to_player(#{player := #{socket := Socket}}, Result) ->
    CompleteMsg = #{type => bidding_complete,
                    result => Result},
    Socket ! CompleteMsg,
    done.

% Create a map from card to its position in the ordering
send_bidding_winner_notification_to_player(#{player := #{socket := Socket}}, WinnerId, BidValue) ->
    NotificationMsg = #{type => bidding_winner_notification,
                        winner_id => WinnerId,
                        bid_value => BidValue,
                        message => iolist_to_binary(["Player ",
                                                    WinnerId,
                                                    " won the bidding with ",
                                                    integer_to_list(BidValue)])},
    Socket ! NotificationMsg,
    done.

-spec broadcast_bid_to_all_players([player_bidding_data()], erlskat:player(), game_value()) -> done.
broadcast_bid_to_all_players(Hands, BiddingPlayer, BidValue) ->
    BroadcastMsg = #{type => bid_broadcast,
                     bidder => maps:without([socket], BiddingPlayer),
                     bid_value => BidValue,
                     message => iolist_to_binary(["Player ",
                                                  maps:get(name, BiddingPlayer, "Unknown"),
                                                  " bids ",
                                                  integer_to_list(BidValue)])},
    [send_broadcast_msg(Hand, BroadcastMsg) || Hand <- Hands],
    done.

-spec broadcast_pass_to_all_players([player_bidding_data()], erlskat:player()) -> done.
broadcast_pass_to_all_players(Hands, PassingPlayer) ->
    BroadcastMsg = #{type => pass_broadcast,
                     passer => maps:without([socket], PassingPlayer),
                     message => iolist_to_binary(["Player ",
                                                  maps:get(name, PassingPlayer, "Unknown"),
                                                  " passes"])},
    [send_broadcast_msg(Hand, BroadcastMsg) || Hand <- Hands],
    done.

-spec send_broadcast_msg(player_bidding_data(), map()) -> done.
send_broadcast_msg(#{player := #{socket := Socket}}, BroadcastMsg) ->
    Socket ! BroadcastMsg,
    done.

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

-spec handle_unexpected_event(gen_statem:event_type(), term(), term()) ->
          gen_statem:event_handler_result(term()).
handle_unexpected_event(EventType, Event, Data) ->
    ?LOG_WARNING(#{module => ?MODULE,
                   line => ?LINE,
                   function => ?FUNCTION_NAME,
                   event_type => EventType,
                   event => Event,
                   state => Data,
                   action => unhandled_event}),
    keep_state_and_data.

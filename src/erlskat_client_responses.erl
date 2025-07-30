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
    awaiting_bid/1,
    game_type_prompt/1,
    multiplier_prompt/2,
    initial_choice_prompt/0,
    skat_cards/1,
    discard_prompt/1,
    bidding_complete/1,
    bidding_winner_notification/2,
    bid_broadcast/2,
    pass_broadcast/1,
    %% Connection management messages
    player_disconnected/2,
    player_timed_out/1,
    game_closed/0,
    %% Lobby messages
    lobby_waiting/1,
    lobby_matched/1,
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
    skat_cards_msg/0,
    discard_prompt_msg/0,
    bidding_complete_msg/0,
    bidding_winner_notification_msg/0,
    bid_broadcast_msg/0,
    pass_broadcast_msg/0,
    player_disconnected_msg/0,
    player_timed_out_msg/0,
    game_closed_msg/0,
    lobby_status_msg/0,
    error_msg/0
]).

%% Base message type - all client messages must have a type field
-type message_type() ::
    %% Bidding phase messages
    bid_prompt | awaiting_bid | game_type_prompt | multiplier_prompt |
    initial_choice_prompt | skat_cards | discard_prompt | bidding_complete |
    bidding_winner_notification | bid_broadcast | pass_broadcast |
    %% Connection management messages
    player_disconnected | player_timed_out | game_closed |
    %% Lobby messages
    lobby_status |
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
    message := binary()
}.

-type awaiting_bid_msg() :: #{
    type := awaiting_bid,
    bid_value := integer(),
    message := binary()
}.

-type game_type_prompt_msg() :: #{
    type := game_type_prompt,
    game_types := [binary()],
    message := binary()
}.

-type multiplier_prompt_msg() :: #{
    type := multiplier_prompt,
    multipliers := [binary()],
    game_type := binary(),
    message := binary()
}.

-type initial_choice_prompt_msg() :: #{
    type := initial_choice_prompt,
    choices := [binary()],
    message := binary()
}.

-type skat_cards_msg() :: #{
    type := skat_cards,
    cards := erlskat:cards(),
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

-type bid_broadcast_msg() :: #{
    type := bid_broadcast,
    bidder := erlskat:player(),
    bid_value := integer(),
    message := binary()
}.

-type pass_broadcast_msg() :: #{
    type := pass_broadcast,
    passer := erlskat:player(),
    message := binary()
}.

%% Connection Management Messages
-type player_disconnected_msg() :: #{
    type := player_disconnected,
    player_id := erlskat:player_id(),
    reconnection_deadline_ms := integer()
}.

-type player_timed_out_msg() :: #{
    type := player_timed_out,
    player_id := erlskat:player_id()
}.

-type game_closed_msg() :: #{
    type := game_closed
}.

%% Lobby Messages
-type lobby_status_msg() :: #{
    type := lobby_status,
    state := waiting | matched,
    players := [erlskat:player_id()]
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
    multiplier_prompt_msg() | initial_choice_prompt_msg() | skat_cards_msg() |
    discard_prompt_msg() | bidding_complete_msg() | bidding_winner_notification_msg() |
    bid_broadcast_msg() | pass_broadcast_msg() | error_msg() |
    %% Connection messages (legacy format without type field)
    player_disconnected_msg() | player_timed_out_msg() | game_closed_msg() |
    %% Lobby messages (legacy format without type field)
    lobby_status_msg().

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
                                  <<"?">>])}.

-spec awaiting_bid(integer()) -> awaiting_bid_msg().
awaiting_bid(BidValue) ->
    #{type => awaiting_bid,
      bid_value => BidValue,
      message => iolist_to_binary([<<"Waiting for opponent to bid ">>,
                                  integer_to_list(BidValue)])}.

-spec game_type_prompt([binary()]) -> game_type_prompt_msg().
game_type_prompt(GameTypes) ->
    #{type => game_type_prompt,
      game_types => GameTypes,
      message => <<"Choose your game type">>}.

-spec multiplier_prompt([binary()], binary()) -> multiplier_prompt_msg().
multiplier_prompt(Multipliers, GameType) ->
    #{type => multiplier_prompt,
      multipliers => Multipliers,
      game_type => GameType,
      message => <<"Choose additional multipliers (or skip)">>}.

-spec initial_choice_prompt() -> initial_choice_prompt_msg().
initial_choice_prompt() ->
    #{type => initial_choice_prompt,
      choices => [<<"hand">>, <<"skat">>],
      message => <<"Do you want to play hand or see the skat?">>}.

-spec skat_cards(erlskat:cards()) -> skat_cards_msg().
skat_cards(Cards) ->
    #{type => skat_cards,
      cards => Cards,
      message => <<"Here are your cards including skat">>}.

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

-spec bidding_winner_notification(erlskat:player_id(), integer()) -> bidding_winner_notification_msg().
bidding_winner_notification(WinnerId, BidValue) ->
    #{type => bidding_winner_notification,
      winner_id => WinnerId,
      bid_value => BidValue,
      message => iolist_to_binary([<<"Player ">>,
                                  WinnerId,
                                  <<" won the bidding with ">>,
                                  integer_to_list(BidValue)])}.

-spec bid_broadcast(erlskat:player(), integer()) -> bid_broadcast_msg().
bid_broadcast(BiddingPlayer, BidValue) ->
    #{type => bid_broadcast,
      bidder => maps:without([socket], BiddingPlayer),
      bid_value => BidValue,
      message => iolist_to_binary([<<"Player ">>,
                                  maps:get(name, BiddingPlayer, <<"Unknown">>),
                                  <<" bids ">>,
                                  integer_to_list(BidValue)])}.

-spec pass_broadcast(erlskat:player()) -> pass_broadcast_msg().
pass_broadcast(PassingPlayer) ->
    #{type => pass_broadcast,
      passer => maps:without([socket], PassingPlayer),
      message => iolist_to_binary([<<"Player ">>,
                                  maps:get(name, PassingPlayer, <<"Unknown">>),
                                  <<" passes">>])}.

%% Connection management message constructors
-spec player_disconnected(erlskat:player_id(), integer()) -> player_disconnected_msg().
player_disconnected(PlayerId, ReconnectionDeadlineMs) ->
    #{type => player_disconnected,
      player_id => PlayerId,
      reconnection_deadline_ms => ReconnectionDeadlineMs}.

-spec player_timed_out(erlskat:player_id()) -> player_timed_out_msg().
player_timed_out(PlayerId) ->
    #{type => player_timed_out,
      player_id => PlayerId}.

-spec game_closed() -> game_closed_msg().
game_closed() ->
    #{type => game_closed}.

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

%% Error message constructor
-spec error_message(binary(), map()) -> error_msg().
error_message(Message, ExpectedFormat) ->
    #{type => error,
      message => Message,
      expected_format => ExpectedFormat}.

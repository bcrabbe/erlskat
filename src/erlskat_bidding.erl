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
-export([start_link/1]).

-export_type([game_response/0]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3]).
-export([handle_event/4]).

-define(SERVER, ?MODULE).
-type color_game() :: erlskat:suit().
-type game_type() :: color_game() | grand | null | ramsch.
%% -type game_value() :: 18 | 20 | 22 | 23 | 24 | 27 | 30 | 33 | 35 | 36.
-type server_state() :: bidding_state() | playing_state() | finished.
-type playing_state() :: map().
-type bidding_state() :: bid | bid_response.

-type server_data() :: bidding_data() | playing_data().
-type player_data() :: player_bidding_data().

-type bidding_data() :: #{bidding_role() => player_bidding_data(), bid := number}.

-type player_bidding_data() :: #{player := erlskat:player(),
                                 initial_role := initial_bidding_role(),
                                 hand := list(erlskat:card())}.

-type initial_bidding_role() :: deals | listens | speaks.
-type bidding_role() :: initial_bidding_role() | counter_speaks | passed.


-type playing_data() :: #{players => list(map()),
                          skat => list(erlskat:card()),
                          game_type => game_type() | undefined}.

-type game_response() ::
        #{state => map(),
          players => list(erlskat:player_id())}.

-type bid_message() :: #{bid => number() | pass}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(list(erlskat:players())) ->
          {ok, Pid :: pid()} |
          ignore |
          {error, Error :: term()}.
start_link(Players) ->
    gen_statem:start_link(?MODULE, Players, []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> handle_event_function.

-spec init(list(Players :: list(erlskat:player()))) ->
          gen_statem:init_result(bid).
init(Players) ->
    process_flag(trap_exit, true),
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                players => Players,
                action => new_game_starting}),
    [erlskat_manager:update_player_proc(Player, self()) || Player <- Players],
    #{hands := Hands, skat := Skat} = deal(Players),
    InitBiddingData = lists:foldl(
                        fun
                            ({Player, Role, Cards}, Acc) ->
                                Acc#{Role => #{player => Player, initial_role => Role, hand => Cards}}
                        end,
                        #{skat => Skat},
                        Hands),
    [player_bidding_data_msg(PlayerBiddingData) || PlayerBiddingData <- maps:values(InitBiddingData)],
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                init_bidding_data => InitBiddingData}),
    {ok, bid, InitBiddingData}.

-spec handle_event(gen_statem:event_type(),
                   Msg :: erlskat_manager:player_message(),
                   State :: server_state(),
                   Data :: server_data()) ->
          gen_statem:event_handler_result(term()).
handle_event(cast,
             #{player := Player, msg := #{bid := Bid}} = Msg,
             bid,
             BiddingData) ->
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                msg => Msg,
                state => bid,
                data => BiddingData}),
    case is_bidder(Player, BiddingData) of
        true -> {bid_response, BiddingData#{bid := Bid}};
        false -> keep_state_and_data
    end.

-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->
                       any().
terminate(_Reason, _State, _Data) ->
    void.

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
    Socket ! maps:without([player], PlayerBiddingData),
    done.


%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec shuffled_deck() -> list(erlskat:card()).
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
          #{hands => list({erlskat:player(), initial_bidding_role(), list(erlskat:card())}),
            skat => list(erlskat:card())}.
deal(Players) ->
    Shuffled = shuffled_deck(),
    Hands = [lists:sublist(Shuffled, 10),
             lists:sublist(Shuffled, 11, 10),
             lists:sublist(Shuffled, 21, 10)],
    Skat = lists:sublist(Shuffled, 31, 2),
    HandsForRoles = lists:zip3(Players, [deals, listens, speaks], Hands),
    #{hands => HandsForRoles,
      skat => Skat}.

-spec is_bidder(Sender :: erlskat:player(),
                GameState :: #{bidding_role() => player_bidding_data()}) ->
          boolean().
is_bidder(#{id := SenderId}, PlayerStatesIndexedByRole) ->
    case maps:get(which_role_bids(PlayerStatesIndexedByRole), PlayerStatesIndexedByRole) of
        #{player := #{id := SenderId}} -> true;
        _ -> false
    end.

-spec index_by_bidding_role(list(player_bidding_data())) -> #{bidding_role() => player_bidding_data()}.
index_by_bidding_role(Players) ->
     lists:foldl(
      fun (#{role := Role} = Player, Acc) -> Acc#{Role => Player} end,
       #{}, Players).

-spec which_role_bids(#{bidding_role() => player_bidding_data()}) ->
          bidding_role().
which_role_bids(#{speaks := _}) ->
    speaks;
which_role_bids(#{counter_speaks := _}) ->
    counter_speaks;
which_role_bids(_) ->
    done.

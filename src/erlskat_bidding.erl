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

-type server_data() :: bidding_data().

-type bidding_data() :: #{hands := [player_bidding_data()], bid := number(), skat := erlskat:skat()}.

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
    InitBiddingData = #{hands => Hands, skat => Skat, bid => 0},
    [player_bidding_data_msg(PlayerBiddingData) ||
        PlayerBiddingData <- Hands],
    %% Send bid prompt to speaks player
    send_bid_prompt_to_speaks_player(Hands),
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
    Msg = maps:without([player], PlayerBiddingData),
    Socket ! Msg,
    done.

-spec send_bid_prompt_to_speaks_player([player_bidding_data()]) -> done.
send_bid_prompt_to_speaks_player(Hands) ->
    case lists:filter(fun(#{current_role := Role}) -> Role =:= speaks end, Hands) of
        [#{player := #{socket := Socket}}] ->
            BidPrompt = #{type => bid_prompt,
                          bid_value => 18,
                          message => <<"Do you want to bid 18?">>},
            Socket ! BidPrompt,
            ?LOG_INFO(#{module => ?MODULE,
                        line => ?LINE,
                        function => ?FUNCTION_NAME,
                        action => sent_bid_prompt,
                        bid_value => 18}),
            done;
        [] ->
            ?LOG_WARNING(#{module => ?MODULE,
                           line => ?LINE,
                           function => ?FUNCTION_NAME,
                           error => no_speaks_player_found}),
            done;
        Multiple ->
            ?LOG_ERROR(#{module => ?MODULE,
                         line => ?LINE,
                         function => ?FUNCTION_NAME,
                         error => multiple_speaks_players,
                         count => length(Multiple)}),
            done
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
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
    InitialRoles = [deals, listens, speaks],
    Hands = [#{player => Player,
               initial_role => InitialRole,
               current_role => InitialRole,
               hand => Hand} ||
             {Player, InitialRole, Hand} <- lists:zip3(Players, InitialRoles, HandCards)],
    #{hands => Hands,
      skat => Skat}.

-spec is_bidder(Sender :: erlskat:player(),
                GameState :: bidding_data()) ->
          boolean().
is_bidder(#{id := SenderId}, #{hands := Hands}) ->
    case get_current_bidder(Hands) of
        #{player := #{id := SenderId}} -> true;
        _ -> false
    end.

-spec get_current_bidder([player_bidding_data()]) ->
          player_bidding_data() | undefined.
get_current_bidder(Hands) ->
    case lists:filter(fun(#{current_role := Role}) ->
                        Role =:= speaks orelse Role =:= counter_speaks
                      end, Hands) of
        [Bidder] -> Bidder;
        [] -> undefined
    end.

-spec get_hand_by_position(hand_position(), [player_bidding_data()]) ->
          player_bidding_data() | undefined.
get_hand_by_position(Position, Hands) when Position >= 0, Position =< 2 ->
    case Position < length(Hands) of
        true -> lists:nth(Position + 1, Hands);  %% lists:nth is 1-indexed
        false -> undefined
    end;
get_hand_by_position(_, _) -> undefined.

-spec update_hand_role(hand_position(), bidding_role(), [player_bidding_data()]) ->
          [player_bidding_data()].
update_hand_role(Position, NewRole, Hands) when Position >= 0, Position =< 2 ->
    case Position < length(Hands) of
        true ->
            {Before, [Hand | After]} = lists:split(Position, Hands),
            UpdatedHand = Hand#{current_role => NewRole},
            Before ++ [UpdatedHand] ++ After;
        false -> Hands
    end;
update_hand_role(_, _, Hands) -> Hands.

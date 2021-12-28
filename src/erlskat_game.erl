%%%-------------------------------------------------------------------
%%% @author Ben Crabbe <ben.crabbe.dev@gmail.com>
%%% @copyright (C) 2019, Ben Crabbe
%%% @doc
%%%
%%% @end
%%% Created : 27 Jan 2019 by Ben Crabbe <ben.crabbe.dev@gmail.com>
%%%-------------------------------------------------------------------
-module(erlskat_game).

-behaviour(gen_statem).
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/1]).

-export_type([game_response/0]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3]).
-export([bidding/3]).

-define(SERVER, ?MODULE).
-type color_game() :: erlskat:suit().
-type game_type() :: color_game() | grand | null | ramsch.
%% -type game_value() :: 18 | 20 | 22 | 23 | 24 | 27 | 30 | 33 | 35 | 36.
-type game_phase() :: bidding | playing | finished.
-type bidding_role() :: geben | horen | sagen.
-type player_state() :: #{player => erlskat:player(),
                          role => bidding_role(),
                          hand => list(erlskat:card()),
                          playing => no | yes | undefined,
                          bidded => nothing | number()}.
-type game_state() :: #{players => list(player_state()),
                        game_phase => game_phase(),
                        skat => list(erlskat:card()),
                        game_type => game_type() | undefined}.
-type game_response() ::
        #{state => matched | waiting,
          players => list(erlskat:player_id())}.

-type bid_message() :: #{player => elskat:player(), bid => number()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(list(erlskat:players())) ->
          {ok, Pid :: pid()} |
          ignore |
          {error, Error :: term()}.
start_link(Players) ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [Players], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> state_functions.

-spec init(list(Players :: list(erlskat:player()))) ->
          gen_statem:init_result(term()).
init([Players])->
    process_flag(trap_exit, true),
    [link(Socket) || #{socket := Socket} <- Players],
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                players => Players,
                action => new_game_starting}),
    [erlskat_manager:update_player_proc(Player, self()) || Player <- Players],
    InitState = deal(Players),
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                init_game_state => InitState,
                action => cards_delt}),

    {ok, bidding, InitState}.

-spec bidding(gen_statem:event_type(),
              Msg :: bid_message(),
              Data :: game_state()) ->
          gen_statem:event_handler_result(term()).
bidding(cast,
        Msg,
        State) ->
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                msg => Msg,
                state => State}),
    keep_state_and_data.

-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->
                       any().
terminate(_Reason, _State, _Data) ->
    void.

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


-spec deal(list(erlskat:players())) -> game_state().
deal(Players) ->
    Shuffled = shuffled_deck(),
    Hands = [lists:sublist(Shuffled, 10),
             lists:sublist(Shuffled, 11, 10),
             lists:sublist(Shuffled, 21, 10)],
    Skat = lists:sublist(Shuffled, 31, 2),
    PlayerStates = lists:map(
      fun
          ({#{socket := PlayerSocket} = Player, Role, Hand}) ->
              PlayerSocket ! #{role => Role, hand => Hand},
              #{player => Player,
                role => Role,
                hand => Hand,
                playing => no,
                bidded => nothing}

      end,
      lists:zip3(Players, [geben, horen, sagen], Hands)),
    #{players => PlayerStates,
      game_phase => bidding,
      skat => Skat,
      game_type => undefined}.

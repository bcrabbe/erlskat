%%%-------------------------------------------------------------------
%%% @author Ben Crabbe <ben.crabbe.dev@gmail.com>
%%% @copyright (C) 2019, Ben Crabbe
%%% @doc
%%%
%%% @end
%%% Created : 27 Jan 2019 by Ben Crabbe <ben.crabbe.dev@gmail.com>
%%%-------------------------------------------------------------------
-module(erlskat_lobby).

-behaviour(gen_statem).
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0]).
-export([new_player/1]).
-export_type([lobby_response/0]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3]).
-export([handle_event/4]).

-define(SERVER, ?MODULE).

-type lobby_response() ::
        #{state => matched | waiting,
          players => list(erlskat:player_id())}.

-type new_player_message() :: {new_player, elskat:player()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec new_player(erlskat:player()) -> pid().
new_player(Player) ->
    gen_statem:cast(?SERVER, {new_player, Player}),
    ?SERVER.

-spec start_link() -> {ok, Pid :: pid()} |
                      ignore |
                      {error, Error :: term()}.
start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> handle_event_function.

-spec init(Args :: term()) -> gen_statem:init_result(term()).
init([]) ->
    process_flag(trap_exit, true),
    {ok, ready, #{players => []}}.

-spec handle_event(gen_statem:event_type(),
                   Msg :: new_player_message(),
                   State :: ready,
                   Data :: #{players => elskat:player()}) ->
          gen_statem:event_handler_result(term()).
handle_event(cast,
             {new_player, NewPlayer},
             ready,
             #{players := WaitingPlayers}) when length(WaitingPlayers) < 2 ->
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                player => NewPlayer,
                msg => msg}),
    NewWaitingPlayers = [NewPlayer | WaitingPlayers],
    notify_players_waiting(NewWaitingPlayers),
    {keep_state, #{players => NewWaitingPlayers}};
handle_event(cast,
             {new_player, NewPlayer},
             ready,
             #{players := WaitingPlayers}) when length(WaitingPlayers) == 2 ->
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                player => NewPlayer,
                action => starting_new_game}),
    NewGamePlayers = [NewPlayer | WaitingPlayers],
    notify_players_of_game(NewGamePlayers),
    erlskat_game_sup:new_game(NewGamePlayers),
    {keep_state, #{players => []}}.

-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->
                       any().
terminate(_Reason, _State, _Data) ->
    void.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec player_id(erlskat:player()) -> erlskat:player_id().
player_id(#{id := Id}) ->
    Id.

-spec notify_players_waiting(list(erlskat:players())) -> done.
notify_players_waiting(WaitingPlayers) ->
    lists:map(
      fun
          (#{socket := PlayerSocket}) ->
              PlayerSocket ! #{state => waiting,
                               players => lists:map(
                                            fun player_id/1,
                                            WaitingPlayers)}
      end,
      WaitingPlayers),
    done.

-spec notify_players_of_game(list(erlskat:players())) -> done.
notify_players_of_game(WaitingPlayers) ->
    lists:map(
      fun
          (#{socket := PlayerSocket}) ->
              PlayerSocket ! #{state => matched,
                               players => lists:map(
                                            fun player_id/1,
                                            WaitingPlayers)}
      end,
      WaitingPlayers),
    done.

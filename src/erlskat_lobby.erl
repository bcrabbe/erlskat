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
-export([start_link/0, stop/0]).
-export([new_player/1, return_player_to_lobby/1]).
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

-spec return_player_to_lobby(erlskat:player()) -> pid().
return_player_to_lobby(Player) ->
    gen_statem:cast(?SERVER, {new_player, Player}),
    erlskat_manager:update_player_proc(Player, ?SERVER),
    ?SERVER.

-spec new_player(erlskat:player()) -> pid().
new_player(Player) ->
    gen_statem:cast(?SERVER, {?FUNCTION_NAME, Player}),
    ?SERVER.

-spec start_link() -> {ok, Pid :: pid()} |
                      ignore |
                      {error, Error :: term()}.
start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
     gen_statem:stop(?SERVER).
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

handle_event(info,
             {'DOWN', Ref, process, DownSocket, Reason},
             ready,
             #{players := WaitingPlayers}) ->
    {Left, StillWaiting} = lists:partition(
                          fun
                              (#{socket := Socket}) when
                                    Socket =:= DownSocket -> true;
                              (_) -> false
                          end,
                          WaitingPlayers),
    erlang:demonitor(Ref),
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                still_waiting => StillWaiting,
                left => Left,
                reason => Reason}),
    notify_players_waiting(StillWaiting),
    #{id := PlayerId} = hd(Left),
    ok = erlskat_manager:clear_player_proc(PlayerId),
    {keep_state, #{players => StillWaiting}};

handle_event(cast,
             {new_player, #{socket := Socket, id := PlayerId} = NewPlayer},
             ready,
             #{players := WaitingPlayers}) when length(WaitingPlayers) < 2 ->
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                player => NewPlayer}),
    Ref = erlang:monitor(process, Socket),
    erlskat_manager:socket_response(PlayerId, erlskat_client_responses:player_joined(PlayerId)),
    NewWaitingPlayers = [NewPlayer#{ref => Ref} | WaitingPlayers],
    notify_players_waiting(NewWaitingPlayers),
    {keep_state, #{players => NewWaitingPlayers}};

handle_event(cast,
             {new_player, #{socket := Socket, id := PlayerId} = NewPlayer},
             ready,
             #{players := WaitingPlayers}) when length(WaitingPlayers) == 2 ->
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                player => NewPlayer,
                action => starting_new_game}),
    erlskat_manager:socket_response(PlayerId, erlskat_client_responses:player_joined(PlayerId)),
    NewGamePlayers = [NewPlayer | WaitingPlayers],
    notify_players_of_game(NewGamePlayers),
    erlskat_floor_manager:new_table(NewGamePlayers),
    lists:foreach(
      fun
          (#{ref := Ref}) -> erlang:demonitor(Ref);
          (_) -> ok
      end,
      NewGamePlayers),
    {keep_state, #{players => []}}.

%% handle_event(Event, Msg, _, _) ->
%%     ?LOG_INFO(#{module => ?MODULE,
%%                 line => ?LINE,
%%                 function => ?FUNCTION_NAME,
%%                 unhandled => #{event => Event, msg => Msg}}),
%%     keep_state_and_data.

-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->
                       any().
terminate(Reason, State, Data) ->
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                state => State,
                data => Data,
                reason => Reason}),
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
          (#{id := PlayerId}) ->
              erlskat_manager:socket_response(PlayerId, erlskat_client_responses:lobby_waiting(
                                lists:map(fun player_id/1, WaitingPlayers)))
      end,
      WaitingPlayers),
    done.

-spec notify_players_of_game(list(erlskat:players())) -> done.
notify_players_of_game(WaitingPlayers) ->
    lists:map(
      fun
          (#{id := PlayerId}) ->
              erlskat_manager:socket_response(PlayerId, erlskat_client_responses:lobby_matched(
                                lists:map(fun player_id/1, WaitingPlayers)))
      end,
      WaitingPlayers),
    done.

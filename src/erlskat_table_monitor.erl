%%%-------------------------------------------------------------------
%%% @author ben crabbe <bencrabbe@bens-MacBook-Pro.local>
%%% @copyright (C) 2023, ben crabbe
%%% @doc
%%% monitors the connection to the players at the table, handles
%%% reconnects, timing out and closing the table.
%%% @end
%%% Created :  9 Apr 2023 by ben crabbe <bencrabbe@bens-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(erlskat_table_monitor).

-behaviour(gen_statem).
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/1]).
-export([stop/1]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([handle_event/4]).

-define(SERVER, ?MODULE).

-define(RECONNECT_DEADLINE_MS, 4000).

-type table_monitor_state() :: connected | connecting.
-type table_monitor_data() :: #{connected => #{reference() => erlskat:player()},
                                reconnecting => [#{id => erlskat:player_id(),
                                                   prior_proc => pid()}],
                                total => number()}.


%%%===================================================================
%%% API
%%%===================================================================

-spec start_link([erlskat:player()]) ->
          {ok, Pid :: pid()} |
          ignore |
          {error, Error :: term()}.
start_link(Players) ->
    gen_statem:start_link(?MODULE, Players, []).

stop(Pid) ->
     gen_statem:stop(Pid).

%%%===================================================================
%%% responses
%%%===================================================================

player_disconnected(DisconnectedPlayerId) ->
    erlskat_client_responses:player_disconnected(DisconnectedPlayerId, ?RECONNECT_DEADLINE_MS).

player_timed_out(DisconnectedPlayerId) ->
    erlskat_client_responses:player_timed_out(DisconnectedPlayerId).

game_closed() ->
    erlskat_client_responses:game_closed().

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() -> handle_event_function.

-spec init([erlskat:player()]) ->
          gen_statem:init_result(term()).
init(Players) ->
    PlayersByRef = lists:foldl(
      fun
          (#{socket := Socket} = Player, Acc) ->
              Ref = erlang:monitor(process, Socket),
              Acc#{Ref => Player}
      end,
      #{},
      Players
     ),
    {ok, connected, #{total => length(Players),
                      connected => PlayersByRef,
                      reconnecting => []}}.

-spec handle_event(gen_statem:event_type(),
                   Msg :: term(),
                   table_monitor_state(),
                   table_monitor_data()) ->
          gen_statem:event_handler_result(term()).

handle_event(info,
             {'DOWN', Ref, process, _DownSocket, _Reason} = Msg,
             State,
             #{connected := ConnectedPlayers} = Data) ->
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                msg => Msg,
                state => State,
                data => Data,
                self => self(),
                reconnecting_deadline => ?RECONNECT_DEADLINE_MS}),
    erlang:demonitor(Ref), % not sure this is needed, but it doesn't hurt?
    #{id := DisconnectedPlayerId} = maps:get(Ref, ConnectedPlayers),
    % get the disconnected player's current process
    {ok, #{proc := PriorProc}} = erlskat_manager:get_player_proc(
                                   DisconnectedPlayerId),
    %% update the player's process to the current process so that we hear if they reconnect
    erlskat_manager:update_player_proc(#{id => DisconnectedPlayerId}, self()),
    NewReconnecting = [#{id => DisconnectedPlayerId, prior_proc => PriorProc} |
                       maps:get(reconnecting, Data, [])],
    % notify the remaining players that a player has disconnected
    RemainingConnectedPlayersByRef = maps:without([Ref], ConnectedPlayers),
    [erlskat_manager:socket_response(PlayerId, player_disconnected(DisconnectedPlayerId)) ||
        #{id := PlayerId}  <- maps:values(RemainingConnectedPlayersByRef)],
    {next_state,
     reconnecting,
     Data#{reconnecting => NewReconnecting,
           connected => RemainingConnectedPlayersByRef},
     [{{timeout, player_timeout},
       ?RECONNECT_DEADLINE_MS,
       {player_timeout, DisconnectedPlayerId}}]};

handle_event({timeout, player_timeout} = Event,
             {player_timeout, DisconnectedPlayerId} = Msg,
             connected = State,
             #{connected := Players,
               reconnecting := ReconnectingIds} = Data) ->
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                state => State,
                player_id => DisconnectedPlayerId,
                event => Event,
                msg => player_timeout_cancelled_following_reconnect}),
    keep_state_and_data;

handle_event({timeout, player_timeout},
             {player_timeout, DisconnectedPlayerId} = Msg,
             reconnecting = _State,
             #{connected := Players,
               reconnecting := ReconnectingIds} = Data) ->
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                state => _State,
                data => Data,
                self => self(),
                msg => Msg}),
    erlskat_manager:clear_player_proc(DisconnectedPlayerId),
    % remove the disconnected player from the reconnecting list
    NewReconnectingIds = lists:filter(
                           fun
                               (#{id := Id}) when Id =:= DisconnectedPlayerId -> false;
                               (_) -> true
                           end,
                           ReconnectingIds),
    % notify the remaining players that the player has timed out
    [erlskat_manager:socket_response(PlayerId, player_timed_out(DisconnectedPlayerId)) ||
        #{id := PlayerId} <- maps:values(Players)],
    {next_state,
     game_closed,
     Data#{reconnecting := NewReconnectingIds},
     [{{timeout, game_closed}, 0, game_closed}]};

handle_event({timeout, game_closed},
             _Msg,
             _State,
             #{connected := RemainingPlayersByRef,
              reconnecting := ReconnectingPlayers} = _Data) ->
    % notify the remaining players that the game is closed
    [erlskat_manager:socket_response(PlayerId, game_closed()) ||
        #{id := PlayerId} <- maps:values(RemainingPlayersByRef)],
    %% return the players to the lobby
    [erlskat_lobby:return_player_to_lobby(Player) ||
        Player <- maps:values(RemainingPlayersByRef)],
    %% disconnect any other reconnecting players
    lists:foreach(
      fun (#{id := Id}) ->
              erlskat_manager:clear_player_proc(Id)
      end,
      ReconnectingPlayers),
    {stop, player_disconnected};

 %% a msg from the reconnecting player
handle_event(cast,
             {socket_request,
              #{player := #{id := ReconnectingPlayerId} = Player, msg := Msg}},
             reconnecting = State,
             #{reconnecting := ReconnectingPlayers} = Data) ->
    case is_disconnected_player(ReconnectingPlayerId, ReconnectingPlayers) of
        true ->
            ?LOG_INFO(
               #{module => ?MODULE,
                 line => ?LINE,
                 function => ?FUNCTION_NAME,
                 state => State,
                 data => Data,
                 self => self(),
                 reconnecting_player_id => ReconnectingPlayerId,
                 msg => Msg}),
            NewReconnectingIds = lists:filter(
                                   fun
                                       (#{id := Id,
                                          prior_proc := PriorProc})
                                         when Id =:= ReconnectingPlayerId ->
                                           erlskat_manager:update_player_proc(
                                             Player,
                                             PriorProc), %% restore the prior process
                                           false;
                                       (_) -> true
                                   end,
                                   ReconnectingPlayers),
            reconnect_player(
              Player,
              Data#{reconnecting => NewReconnectingIds});
        false -> {keep_state, State, Data}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->
          any().
terminate(_Reason, _State, _Data) ->
    void.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(
        OldVsn :: term() | {down, term()},
        State :: term(), Data :: term(), Extra :: term()) ->
          {ok, NewState :: term(), NewData :: term()} |
          (Reason :: term()).
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

is_disconnected_player(Id, [#{id := Id} | _]) ->
    true;
is_disconnected_player(Id, [_ | Rest]) ->
    is_disconnected_player(Id, Rest);
is_disconnected_player(_, []) ->
    false.

reconnect_player(#{socket := Socket} = Player,
                 #{reconnecting := [], connected := ConnectedPlayers} = Data) ->
    Ref = erlang:monitor(process, Socket),
    NewData = Data#{connected => ConnectedPlayers#{Ref => Player}},
    {next_state, connected, NewData};

reconnect_player(#{socket := Socket} = Player,
                 #{reconnecting := _OtherReconnecters, connected := ConnectedPlayers} = Data) ->
    Ref = erlang:monitor(process, Socket),
    NewData = Data#{connected => ConnectedPlayers#{Ref => Player}},
    {next_state, reconnecting, NewData}.

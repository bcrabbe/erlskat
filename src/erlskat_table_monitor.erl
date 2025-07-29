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
                                reconnecting => [erlskat:player_id()],
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
    #{player_disconnected => DisconnectedPlayerId,
      reconnection_deadline_ms => ?RECONNECT_DEADLINE_MS}.

player_timed_out(DisconnectedPlayerId) ->
    #{player_timed_out => DisconnectedPlayerId}.

game_closed() ->
    game_closed.

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
                      players => PlayersByRef,
                      reconnecting => []}}.

-spec handle_event(gen_statem:event_type(),
                   Msg :: term(),
                   table_monitor_state(),
                   table_monitor_data()) ->
          gen_statem:event_handler_result(term()).

handle_event(info,
             {'DOWN', Ref, process, _DownSocket, _Reason} = Msg,
             State,
             #{players := Players} = Data) ->
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                msg => Msg,
                state => State,
                data => Data,
                self => self(),
                reconnecting_deadline => ?RECONNECT_DEADLINE_MS}),
    erlang:demonitor(Ref),
    #{id := DisconnectedPlayerId} = maps:get(Ref, Players),
    RemainingPlayersByRef = maps:without([Ref], Players),
    [Socket ! player_disconnected(DisconnectedPlayerId) ||
        #{socket := Socket}  <- maps:values(RemainingPlayersByRef)],
    {next_state, reconnecting,
     Data#{reconnecting => [DisconnectedPlayerId | maps:get(reconnecting, Data, [])]},
     [{{timeout, player_timeout}, ?RECONNECT_DEADLINE_MS, {player_timeout, DisconnectedPlayerId}}]};

handle_event({timeout, player_timeout},
             {player_timeout, DisconnectedPlayerId} = Msg,
             _State,
             #{players := Players} = Data) ->
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                state => _State,
                data => Data,
                self => self(),
                msg => Msg}),
    [Socket ! player_timed_out(DisconnectedPlayerId) ||
        #{socket := Socket} <- maps:values(Players)],
    {next_state, game_closed, Data, [{{timeout, game_closed}, 0, game_closed}]};

handle_event({timeout, game_closed},
             _Msg,
             _State,
             #{players := RemainingPlayersByRef} = _Data) ->
    [Socket ! game_closed() ||
        #{socket := Socket} <- maps:values(RemainingPlayersByRef)],
    {stop, player_disconnected};

 %% a msg from the reconnecting player
handle_event(cast,
             {socket_message,
              #{player := #{id := Id} = Player, msg := _Msg}},
             reconnecting = State,
             #{reconnecting := ReconnectingIds} = Data) ->
    case is_disconnected_player(Id, ReconnectingIds) of
        true -> reconnect_player(
                  Player,
                  maps:put(reconnecting, lists:delete(Id, ReconnectingIds), Data));
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

is_disconnected_player(Id, [Id | _]) ->
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

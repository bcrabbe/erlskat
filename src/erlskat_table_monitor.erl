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

-define(RECONNECT_DEADLINE_MS, 5000).

-type table_monitor_state() :: connected | connecting.
-type table_monitor_data() :: #{connected => #{reference() => erlskat:player()},
                                reconnecting => [erlskat:player_id()],
                                total => number()}.


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link([erlskat:player()]) ->
          {ok, Pid :: pid()} |
          ignore |
          {error, Error :: term()}.
start_link(Players) ->
    gen_statem:start_link(?MODULE, Players, []).

stop(Pid) ->
     gen_statem:stop(Pid).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() -> handle_event_function.

-spec init([erlskat:player()]) ->
          gen_statem:init_result(term()).
init(Players) ->
    process_flag(trap_exit, true),
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
                player_disconected => Msg,
                state => State,
                data => Data,
                reconnecting_deadline => ?RECONNECT_DEADLINE_MS}),
    erlang:demonitor(Ref),
    #{id := DisconnectedPlayerId} = maps:get(Ref, ConnectedPlayers),
    RemainingPlayersByRef = maps:without([Ref], ConnectedPlayers),
    [Socket ! #{player_disconected => DisconnectedPlayerId} ||
        #{socket := Socket}  <- maps:values(RemainingPlayersByRef)],
    {next_state, reconnecting,
     Data#{reconnecting => [DisconnectedPlayerId | maps:get(reconnecting, Data, [])]},
     [{{timeout, dead}, ?RECONNECT_DEADLINE_MS, {disconnected, DisconnectedPlayerId}}]};

handle_event({timeout, dead},
             {disconnected, DisconnectedPlayerId} = Msg,
             State,
             #{connected := RemainingPlayersByRef} = Data) ->
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                state => State,
                data => Data,
                table_disconnected => Msg}),
    [Socket ! #{player_timed_out => DisconnectedPlayerId} ||
        #{socket := Socket} <- maps:values(RemainingPlayersByRef)],
    {stop, State, Data};

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
    end;

handle_event(Event, Msg, State, Data) ->
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                ignored_event => Event,
                ignored_msg => Msg,
                state => State,
                data  => Data}),
    {keep_state, State, Data}.

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
        OldVsn :: term() | {down,term()},
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

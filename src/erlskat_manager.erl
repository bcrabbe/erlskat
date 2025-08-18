%%%-------------------------------------------------------------------
%%% @Author Ben Crabbe <ben.crabbe.dev@gmail.com>
%%% @copyright (C) 2019, Ben Crabbe
%%% @doc
%%% knows where everyone is
%%% @end
%%% Created : 27 Jan 2019 by Ben Crabbe <ben.crabbe.dev@gmail.com>
%%%-------------------------------------------------------------------
-module(erlskat_manager).

-behaviour(gen_statem).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0]).
-export([socket_request/2,
         socket_response/2,
         update_player_proc/2,
         clear_player_proc/1,
         get_player_proc/1]).
-export_type([player_message/0]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3]).
-export([handle_event/4]).

-define(SERVER, ?MODULE).
%%%===================================================================
%%% API
%%%===================================================================
-type player_message() :: #{player => erlskat:player(), msg => map() | binary()}.

%% requests coming from the player
-spec socket_request(#{id := erlskat:player_id(), socket := pid()},
                     Msg :: map() | binary() | atom()) -> ok.
socket_request(Player, Msg) ->
    gen_statem:cast(?SERVER, {socket_request, Player, Msg}),
    ok.

%% responses going to the player
-spec socket_response(erlskat:player_id(), Response :: map() | binary()) -> ok.
socket_response(PlayerId, Response) ->
    gen_statem:cast(?SERVER, {socket_response, PlayerId, Response}),
    ok.

-spec update_player_proc(erlskat:player(), pid()) -> ok.
update_player_proc(Player, Proc) ->
    gen_statem:cast(?SERVER, {update_player_proc, Player, Proc}),
    ok.

-spec clear_player_proc(erlskat:player_id()) -> ok.
clear_player_proc(PlayerId) ->
    gen_statem:cast(?SERVER, {clear_player_proc, PlayerId}),
    ok.

-spec get_player_proc(erlskat:player_id()) ->
          {error, no_proc} |
          {ok, #{id => erlskat:player_id(), socket => pid(), proc => pid()}}.
get_player_proc(PlayerId) ->
    gen_statem:call(?SERVER, {get_player_proc, PlayerId}).

-spec start_link() ->
          {ok, Pid :: pid()} |
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
    PlayersTid = ets:new(players, player_table_opts()),
    {ok, ready, #{players => PlayersTid, player_histories => #{}}}.

-spec handle_event(gen_statem:event_type(),
                   Msg :: term(),
                   State :: term(),
                   Data :: term()) ->
          gen_statem:event_handler_result(term()).

 %% request from a player socket
handle_event(cast,
             {socket_request,
              #{id := PlayerId, socket := Socket} = Player,
              Msg},
             ready,
             #{players := PlayersTid, player_histories := PlayerHistories} = _Data) ->
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                player => Player,
                msg => Msg}),
    case ets:lookup(PlayersTid, PlayerId) of
        [] ->
            NewData = new_player(Player, PlayersTid, PlayerHistories),
            {keep_state, NewData};
        [{PlayerId, Socket, Proc}] ->
            gen_statem:cast(
              Proc,
              {socket_request, #{player => Player, msg => Msg}}),
            keep_state_and_data;
        [{PlayerId, _OldSocket, Proc}] ->
            ?LOG_INFO(#{module => ?MODULE,
                        line => ?LINE,
                        function => ?FUNCTION_NAME,
                        player_id => PlayerId,
                        new_socket => Socket,
                        msg => "Player reconnected, resyncing history"}),
            true = ets:insert(
                     PlayersTid,
                     {PlayerId, Socket, Proc}),
            resync_player_history(PlayerId, PlayerHistories),
            gen_statem:cast(
              Proc,
              {socket_request, #{player => Player, msg => Msg}}),
            keep_state_and_data
    end;

%% response to a player socket
handle_event(cast,
             {socket_response, PlayerId, Response},
             ready,
             #{players := PlayersTid, player_histories := PlayerHistories}) ->
    case ets:lookup(PlayersTid, PlayerId) of
        [] ->
            ?LOG_WARNING(#{module => ?MODULE,
                          line => ?LINE,
                          function => ?FUNCTION_NAME,
                          player_id => PlayerId,
                          reason => no_socket_found}),
            ok;
        [{PlayerId, Socket, _Proc}] ->
            store_response_in_history(PlayerId, Response, PlayerHistories),
            Socket ! Response
    end,
    keep_state_and_data;

handle_event(cast,
             {update_player_proc,
              #{id := PlayerId} = Player,
              NewProc},
             ready,
             #{players := PlayersTid} = _Data) ->
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                player => Player,
                new_proc => NewProc}),
    true = ets:update_element(
             PlayersTid,
             PlayerId,
             {3, NewProc}),
    keep_state_and_data;

handle_event(cast,
             {clear_player_proc, PlayerId},
             ready,
             #{players := PlayersTid, player_histories := PlayerHistories} = Data) ->
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                player_id => PlayerId}),
    true = ets:delete(PlayersTid, PlayerId),
    NewData = clear_player_history(PlayerId, PlayerHistories, Data),
    {keep_state, NewData};

handle_event({call, From},
             {get_player_proc, PlayerId},
             ready,
             #{players := PlayersTid} = _Data) ->
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => call_get_player_proc,
                player_id => PlayerId}),
    Reply = case ets:lookup(PlayersTid, PlayerId) of
        [] -> {error, no_proc};
        [{PlayerId, Socket, Proc}] ->
                   {ok, #{id => PlayerId,
                          socket => Socket,
                          proc => Proc}}
    end,
    {keep_state_and_data, [{reply, From, Reply}]}.

-spec terminate(Reason :: term(), State :: term(), Data :: term()) -> any().

terminate(_Reason, _State, _Data) ->
    void.

new_player(#{id := PlayerId, socket := Socket} = Player, PlayersTid, PlayerHistories) ->
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                new_player => PlayerId}),
    Proc = erlskat_lobby:new_player(Player),
    true = ets:insert(
             PlayersTid,
             {PlayerId, Socket, Proc}),
    PlayerHistoryTid = ets:new(player_history, player_history_table_opts()),
    NewPlayerHistories = PlayerHistories#{PlayerId => PlayerHistoryTid},
    #{players => PlayersTid, player_histories => NewPlayerHistories}.
%%%===================================================================
%%% Internal functions
%%%===================================================================

player_table_opts() ->
    default_table_opts().

player_history_table_opts() ->
    default_table_opts().

default_table_opts() ->
    [ordered_set,
     protected,
     {keypos, 1},
     {heir, none},
     {write_concurrency, false},
     {read_concurrency, false},
     {decentralized_counters, false}].

store_response_in_history(PlayerId, Response, PlayerHistories) ->
    case maps:get(PlayerId, PlayerHistories, undefined) of
        undefined ->
            ?LOG_WARNING(#{module => ?MODULE,
                          line => ?LINE,
                          function => ?FUNCTION_NAME,
                          player_id => PlayerId,
                          reason => no_history_table}),
            ok;
        HistoryTid ->
            ResponseType = extract_response_type(Response),
            Timestamp = erlang:system_time(microsecond),
            true = ets:insert(HistoryTid, {Timestamp, ResponseType, Response}),
            ok
    end.

extract_response_type(Response) when is_map(Response) ->
    maps:get(type, Response, unknown);
extract_response_type(_Response) ->
    unknown.

resync_player_history(PlayerId, PlayerHistories) ->
    case maps:get(PlayerId, PlayerHistories, undefined) of
        undefined ->
            ?LOG_WARNING(#{module => ?MODULE,
                          line => ?LINE,
                          function => ?FUNCTION_NAME,
                          player_id => PlayerId,
                          reason => no_history_table_for_resync}),
            ok;
        HistoryTid ->
            ?LOG_INFO(#{module => ?MODULE,
                       line => ?LINE,
                       function => ?FUNCTION_NAME,
                       player_id => PlayerId,
                       msg => "Resyncing player history"}),
            Messages = ets:tab2list(HistoryTid),
            SortedMessages = lists:sort(Messages),
            lists:foreach(
              fun({_Timestamp, _Type, Response}) ->
                  socket_response(PlayerId, Response)
              end,
              SortedMessages),
            ok
    end.

clear_player_history(PlayerId, PlayerHistories, #{players := _PlayersTid} = Data) ->
    case maps:get(PlayerId, PlayerHistories, undefined) of
        undefined ->
            Data;
        HistoryTid ->
            true = ets:delete(HistoryTid),
            NewPlayerHistories = maps:remove(PlayerId, PlayerHistories),
            Data#{player_histories => NewPlayerHistories}
    end.

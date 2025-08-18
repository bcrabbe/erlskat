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
    {ok, ready, #{players => PlayersTid}}.

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
             #{players := PlayersTid}) ->
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                player => Player,
                msg => Msg}),
    case ets:lookup(PlayersTid, PlayerId) of
        [] -> new_player(Player, PlayersTid);
        [{PlayerId, Socket, Proc}] ->
            gen_statem:cast(
              Proc,
              {socket_request, #{player => Player, msg => Msg}});
        [{PlayerId, NewSocket, Proc}] ->
            %% new socket - this can happen if the player reconnects
            %% TODO: need to think about how to handle this since the original socket pid
            %% is held in the controlling processes
            ?LOG_INFO(#{module => ?MODULE,
                        line => ?LINE,
                        function => ?FUNCTION_NAME,
                        player_id => PlayerId,
                        new_socket => NewSocket}),
            true = ets:insert(
                     PlayersTid,
                     {PlayerId, NewSocket, Proc}),
            gen_statem:cast(
              Proc,
              {socket_request, #{player => Player, msg => Msg}})
    end,
    keep_state_and_data;

%% response to a player socket
handle_event(cast,
             {socket_response, PlayerId, Response},
             ready,
             #{players := PlayersTid}) ->
    case ets:lookup(PlayersTid, PlayerId) of
        [] ->
            ?LOG_WARNING(#{module => ?MODULE,
                          line => ?LINE,
                          function => ?FUNCTION_NAME,
                          player_id => PlayerId,
                          reason => no_socket_found}),
            ok;
        [{PlayerId, Socket, _Proc}] ->
            Socket ! Response
    end,
    keep_state_and_data;

handle_event(cast,
             {update_player_proc,
              #{id := PlayerId} = Player,
              NewProc},
             ready,
             #{players := PlayersTid}) ->
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
             #{players := PlayersTid}) ->
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                player_id => PlayerId}),
    true = ets:delete(PlayersTid, PlayerId),
    keep_state_and_data;

handle_event({call, From},
             {get_player_proc, PlayerId},
             ready,
             #{players := PlayersTid}) ->
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

new_player(#{id := PlayerId, socket := Socket} = Player, PlayersTid) ->
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                new_player => PlayerId}),
    Proc = erlskat_lobby:new_player(Player),
    true = ets:insert(
             PlayersTid,
             {PlayerId, Socket, Proc}).
%%%===================================================================
%%% Internal functions
%%%===================================================================

player_table_opts() ->
    [ordered_set,
     protected,
     {keypos, 1},
     {heir, none},
     {write_concurrency, false},
     {read_concurrency, false},
     {decentralized_counters, false}].

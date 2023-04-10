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
-export([socket_message/2, update_player_proc/2]).
-export_type([player_message/0]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3]).
-export([handle_event/4]).

-define(SERVER, ?MODULE).
%%%===================================================================
%%% API
%%%===================================================================
-type player_message() :: #{player => elskat:player(), msg => map()}.

-spec socket_message(erlskat:player(), Msg :: map()) -> ok.
socket_message(Player, Msg) ->
    gen_statem:cast(?SERVER, {socket_message, Player, Msg}),
    ok.

-spec update_player_proc(erlskat:player(), pid()) -> ok.
update_player_proc(Player, Proc) ->
    gen_statem:cast(?SERVER, {update_player_proc, Player, Proc}),
    ok.

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

 %% message from a player socket
handle_event(cast,
             {socket_message, #{id := PlayerId} = Player, Msg},
             ready,
             #{players := PlayersTid}) ->
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                player => Player,
                msg => Msg}),
    case ets:lookup(PlayersTid, PlayerId) of
        [] -> new_player(Player, PlayersTid);
        [{PlayerId, _Socket, Proc}] -> gen_statem:cast(
                                         Proc,
                                         {socket_message,
                                          #{player => Player, msg => Msg}})
    end,
    keep_state_and_data;

%% message from a process controlling a socket
handle_event(cast,
             {update_player_proc,
              #{id := PlayerId, socket := Socket} = Player,
              NewProc},
             ready,
             #{players := PlayersTid}) ->
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                player => Player,
                new_proc => NewProc}),
    true = ets:insert(
             PlayersTid,
             {PlayerId, Socket, NewProc}),
    keep_state_and_data.

-spec terminate(Reason :: term(), State :: term(), Data :: term()) -> any().

terminate(_Reason, _State, _Data) ->
    void.

new_player(#{id := PlayerId, socket := Socket} = Player, PlayersTid) ->
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

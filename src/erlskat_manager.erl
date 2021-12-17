%%%-------------------------------------------------------------------
%%% @Author Ben Crabbe <ben.crabbe.dev@gmail.com>
%%% @copyright (C) 2019, Ben Crabbe
%%% @doc
%%%
%%% @end
%%% Created : 27 Jan 2019 by Ben Crabbe <ben.crabbe.dev@gmail.com>
%%%-------------------------------------------------------------------
-module(erlskat_manager).

-behaviour(gen_statem).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0]).
-export([handle/2]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3]).
-export([handle_event/4]).

-define(SERVER, ?MODULE).

-type response() :: map().
-type error() :: map().

%%%===================================================================
%%% API
%%%===================================================================
-spec handle(Session :: binary(), Msg :: map()) ->
          {ok, Response :: response()} |
          {error, Error :: error()}.
handle(Session, Msg) ->
    gen_statem:call(?SERVER, {Session, Msg}).

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
handle_event({call, Socket},
             {SessionId, Msg},
             ready = _State,
             #{players := PlayersTid}) ->
    ?LOG_INFO(#{call_from => Socket}),
    Reply = case ets:lookup(PlayersTid, SessionId) of
                [] -> new_player(Socket, PlayersTid, SessionId);
                [{SessionId, Socket, Proc}] -> gen_statem:call(Pid, {SessionId, Msg})
            end,
    {keep_state_and_data, [{reply, Socket, Reply}]}.

%% handle_event(
%%   {call, self},
%%   {new_player, Session, #{ name := Name }},
%%   _State,
%%   #{players := PlayersTid}) ->
%%     {keep_state_and_data, [{reply, From, erlskat_util:trie_to_map(Trie)}]}.


-spec terminate(Reason :: term(), State :: term(), Data :: term()) -> any().

terminate(_Reason, _State, _Data) ->
    void.

new_player(Socket, PlayersTid, PlayerId) ->
    erlskat_lobby:new_player(),
    true = ets:insert(
             PlayersTid,
             [{PlayerId, Socket, Proc}]).
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

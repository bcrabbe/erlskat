%%%-------------------------------------------------------------------
%%% @author Ben Crabbe <ben.crabbe.dev@gmail.com>
%%% @copyright (C) 2019, Ben Crabbe
%%% @doc
%%%
%%% @end
%%% Created : 27 Jan 2019 by Ben Crabbe <ben.crabbe.dev@gmail.com>
%%%-------------------------------------------------------------------
-module(erlskat_trie_manager).

-behaviour(gen_statem).

%% API
-export([start_link/0]).
-export([add_words/1]).
-export([trie_to_map/0]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3]).
-export([handle_event/4]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API
%%%===================================================================
trie_to_map() ->
    gen_statem:call(?SERVER, to_map).

add_words(Words) ->
    gen_statem:cast(?SERVER, {add, Words}).

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
    {ok, ready, #{trie => erlskat_builder:init()}}.

-spec handle_event(gen_statem:event_type(),
                   Msg :: term(),
                   State :: term(),
                   Data :: term()) ->
                          gen_statem:event_handler_result(term()).

handle_event(cast,
             {add, NewWords},
             ready = _State,
             #{trie := Trie}) ->
    erlskat_builder:add_words(Trie, NewWords),
    keep_state_and_data;

handle_event({call, From}, to_map, _State, #{trie := Trie}) ->
    {keep_state_and_data, [{reply, From, erlskat_util:trie_to_map(Trie)}]};

handle_event({call, From}, _Msg, State, Data) ->
    {next_state, State, Data, [{reply, From, ok}]}.

-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->
                       any().
terminate(_Reason, _State, _Data) ->
    void.

%%%===================================================================
%%% Internal functions
%%%===================================================================

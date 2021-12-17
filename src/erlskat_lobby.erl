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

%% API
-export([start_link/0]).
-export([new_player/1]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3]).
-export([handle_event/4]).

-define(SERVER, ?MODULE).

-type player() ::
   #{ id => binary() }.

-type new_player_response() ::
        #{ msg => binary(),
           players => list(player()) }.
%%%===================================================================
%%% API
%%%===================================================================

-spec new_player(PlayerId :: binary()) -> new_player_response().
new_player(PlayerId) ->
    gen_statem:call(?SERVER, {new_player, PlayerId}).

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
                   Msg :: term(),
                   State :: term(),
                   Data :: term()) ->
          gen_statem:event_handler_result(term()).
handle_event({call, From},
             {new_player, PlayerId},
             _State,
             #{players := WaitingPlayers}) when length(WaitingPlayers) < 2 ->
    {keep_state,
     #{players => [PlayerId | WaitingPlayers]},
     [{reply,
       From,
       #{ msg => <<"waiting for more players">>,
          players => [lists:map(
                        fun (P) -> #{ id => P } end,
                        WaitingPlayers)] }}]};
handle_event({call, From},
             {new_player, PlayerId},
             _State,
             #{players := WaitingPlayers}) when length(WaitingPlayers) == 2 ->

    {keep_state, #{ players => [] }, [{reply, From, erlskat_util:trie_to_map(Trie)}]};

handle_event({call, From}, _Msg, State, Data) ->
    {next_state, State, Data, [{reply, From, ok}]}.

-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->
                       any().
terminate(_Reason, _State, _Data) ->
    void.

%%%===================================================================
%%% Internal functions
%%%===================================================================

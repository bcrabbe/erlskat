%%%-------------------------------------------------------------------
%%% @author Ben Crabbe <ben.crabbe.dev@gmail.com>
%%% @copyright (C) 2019, Ben Crabbe
%%% @doc
%%% simple_one_for_one supervisor, starts tables for 3 players.
%%% @end
%%% Created : 27 Jan 2019 by Ben Crabbe <ben.crabbe.dev@gmail.com>
%%%-------------------------------------------------------------------
-module(erlskat_floor_manager).

-behaviour(supervisor).
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0]).
-export([new_table/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec new_table([erlskat:player()]) -> pid().
new_table(Players) ->
    Tables = supervisor:count_children(?SERVER),
    CurrentTableCount = proplists:get_value(specs, Tables, 0),
    NewTableNumber = CurrentTableCount + 1,
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                players => Players,
                tables => Tables,
                action => starting_new_table,
                table_number => NewTableNumber}),
    {ok, Game} = supervisor:start_child(
                   ?SERVER,
                   [NewTableNumber, Players]),
    Game.

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
                      {error, {already_started, Pid :: pid()}} |
                      {error, {shutdown, term()}} |
                      {error, term()} |
                      ignore.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% simple_one_for_one supervisor is similar to writing any other type of
%%  supervisor, except for one thing. The argument list in the {M,F,A}
%% tuple is not the whole thing, but is going to be appended to what you
%% call it with when you do supervisor:start_child(Sup, Args).
%%  That's right, supervisor:start_child/2 changes API. So instead of
%% doing supervisor:start_child(Sup, Spec), which would call
%%  erlang:apply(M,F,A), we now have supervisor:start_child(Sup, Args),
%%  which calls erlang:apply(M,F,A++Args).
%% @end
%%--------------------------------------------------------------------
-spec init(list()) ->
          {ok, {SupFlags :: supervisor:sup_flags(),
                [ChildSpec :: supervisor:child_spec()]}} |
          ignore.
init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 1,
                 period => 5},
    TableSpec = #{id => erlskat_table,
      start => {erlskat_table, start_link, []},
      restart => transient,
      shutdown => 5000,
      type => supervisor,
      modules => [erlskat_table]},
    {ok, {SupFlags, [TableSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

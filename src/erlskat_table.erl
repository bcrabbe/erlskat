%%%-------------------------------------------------------------------
%%% @author Ben Crabbe <ben.crabbe.dev@gmail.com>
%%% @copyright (C) 2019, Ben Crabbe
%%% @doc
%%% a table is for 3 players. it remembers the score and starts hands
%%% @end
%%% Created : 27 Jan 2019 by Ben Crabbe <ben.crabbe.dev@gmail.com>
%%%-------------------------------------------------------------------
-module(erlskat_table).

-behaviour(supervisor).
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/2]).


%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link(number(), [erlskat:player()]) -> {ok, Pid :: pid()} |
          {error, {already_started, Pid :: pid()}} |
          {error, {shutdown, term()}} |
          {error, term()} |
          ignore.
start_link(TableNumber, Players) ->
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                players => Players,
                action => new_table_started,
                table_number => TableNumber}),
    supervisor:start_link({local, ?SERVER}, ?MODULE, Players).

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
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
          {ok, {SupFlags :: supervisor:sup_flags(),
                [ChildSpec :: supervisor:child_spec()]}} |
          ignore.
init(Players) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5,
                 auto_shutdown => all_significant},
    Monitor = #{id => erlskat_table_monitor,
                start => {erlskat_table_monitor, start_link, [Players]},
                restart => transient,
                shutdown => 5000,
                significant => true,
                type => worker,
                modules => [erlskat_table_monitor]},
    Scorecard = #{id => erlskat_scorecard,
                  start => {erlskat_scorecard, start_link, [Players]},
                  restart => permanent,
                  shutdown => 5000,
                  type => worker,
                  modules => [erlskat_scorecard]},
    Hand = #{id => erlskat_hand,
             start => {erlskat_hand, start_link, [Players]},
             restart => permanent,
             shutdown => 5000,
             type => supervisor,
             modules => [erlskat_hand]},
    {ok, {SupFlags, [Scorecard, Hand, Monitor]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

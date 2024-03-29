%%%-------------------------------------------------------------------
%%% @author Ben Crabbe <ben.crabbe.dev@gmail.com>
%%% @copyright (C) 2019, Ben Crabbe
%%% @doc
%%% simple_one_for_one game supervisor. Starts games between 3
%%% players.
%%% @end
%%% Created : 27 Jan 2019 by Ben Crabbe <ben.crabbe.dev@gmail.com>
%%%-------------------------------------------------------------------
-module(erlskat_game_sup).

-behaviour(supervisor).
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0]).
-export([new_game/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec new_game(list(erlskat:player())) -> pid().
new_game(Players) ->
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                players => Players,
                action => starting_new_bidding}),
    {ok, Game} = supervisor:start_child(
                   ?SERVER,
                   [Players]),
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
    GameSpec = #{id => erlskat_bidding,
      start => {erlskat_bidding, start_link, []},
      restart => transient,
      shutdown => 5000,
      type => worker,
      modules => [erlskat_bidding]},
    {ok, {SupFlags, [GameSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

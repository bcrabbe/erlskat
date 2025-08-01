%%%-------------------------------------------------------------------
%%% @author Ben Crabbe <ben.crabbe.dev@gmail.com>
%%% @copyright (C) 2019, Ben Crabbe
%%% @doc
%%% coordinates a hand of skat: manages bidding -> game workflow
%%% @end
%%% Created : 27 Jan 2019 by Ben Crabbe <ben.crabbe.dev@gmail.com>
%%%-------------------------------------------------------------------
-module(erlskat_hand).

-behaviour(gen_server).
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

% State record for the coordinator
-record(state, {
    players :: [erlskat:player()],
    current_phase :: bidding | game,
    current_pid :: pid() | undefined,
    current_monitor_ref :: reference() | undefined,
    bidding_result :: term() | undefined
}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Players) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Players, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Players) ->
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                players => Players,
                action => hand_coordinator_started}),

    % Start the bidding phase immediately
    {ok, BiddingPid} = erlskat_bidding:start_link(self(), Players),
    MonitorRef = erlang:monitor(process, BiddingPid),

    {ok, #state{
        players = Players,
        current_phase = bidding,
        current_pid = BiddingPid,
        current_monitor_ref = MonitorRef
    }}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

% Handle bidding completion message
handle_info({bidding_complete, BiddingPid, BiddingResult},
            #state{current_phase = bidding, current_pid = BiddingPid} = State) ->

    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                bidding_result => BiddingResult,
                action => bidding_phase_complete}),

    % Clean up the bidding process monitor
    erlang:demonitor(State#state.current_monitor_ref, [flush]),

    % Extract winner and game info from bidding result
    Winner = maps:get(winner, BiddingResult),
    ChosenGame = maps:get(chosen_game, BiddingResult),
    
    % Start the game phase with the new unified game server
    case start_game_phase(Winner, ChosenGame, BiddingResult, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        {stop, Reason, NewState} ->
            {stop, Reason, NewState}
    end;

% Handle game completion message
handle_info({game_complete, GamePid, GameResult},
            #state{current_phase = game, current_pid = GamePid} = State) ->

    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                game_result => GameResult,
                action => game_phase_complete}),

    % Clean up the game process monitor
    erlang:demonitor(State#state.current_monitor_ref, [flush]),

    % Hand is complete, TODO: notify scorecard
    % notify_scorecard(GameResult),

    {stop, normal, State};

% Handle child process crash
handle_info({'DOWN', MonitorRef, process, Pid, Reason},
            #state{current_monitor_ref = MonitorRef, current_pid = Pid} = State) ->

    ?LOG_ERROR(#{module => ?MODULE,
                 line => ?LINE,
                 function => ?FUNCTION_NAME,
                 crashed_pid => Pid,
                 crash_reason => Reason,
                 phase => State#state.current_phase,
                 action => child_process_crashed}),

    % Handle the crash appropriately
    {stop, {child_crashed, Reason}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_game_phase(Declarer, GameType, BiddingResult, State) ->
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                declarer => Declarer,
                game_type => GameType,
                action => starting_game_phase}),

    case erlskat_game:start_link(self(), Declarer, GameType, BiddingResult, State#state.players) of
        {ok, GamePid} ->
            MonitorRef = erlang:monitor(process, GamePid),
            {noreply, State#state{
                current_phase = game,
                current_pid = GamePid,
                current_monitor_ref = MonitorRef,
                bidding_result = BiddingResult
            }};
        {error, Reason} ->
            ?LOG_ERROR(#{module => ?MODULE,
                         line => ?LINE,
                         error_reason => Reason,
                         action => failed_to_start_game}),
            {stop, {game_start_failed, Reason}, State}
    end.




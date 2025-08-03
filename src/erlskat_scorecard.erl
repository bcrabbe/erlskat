%%%-------------------------------------------------------------------
%%% @author ben crabbe <bencrabbe@bens-MacBook-Pro.local>
%%% @copyright (C) 2023, ben crabbe
%%% @doc
%%%
%%% @end
%%% Created :  8 Apr 2023 by ben crabbe <bencrabbe@bens-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(erlskat_scorecard).

-behaviour(gen_statem).
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/1]).
-export([record_result/2]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([handle_event/4]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API
%%%===================================================================

-spec record_result(pid(), erlskat_hand:game_result()) -> ok | {error, term()}.
record_result(TableSupPid, HandResult) ->
    {ok,
     {_Id, ScorecardPid, _Type, _Modules}} = supervisor:which_child(
                                               TableSupPid,
                                               erlskat_scorecard),
    gen_statem:call(ScorecardPid, {record_result, HandResult}).

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link([erlskat:player()]) ->
          {ok, Pid :: pid()} |
          ignore |
          {error, Error :: term()}.
start_link(Players) ->
    gen_statem:start_link(?MODULE, Players, []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Define the callback_mode() for this callback module.
%% @end
%%--------------------------------------------------------------------
-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> handle_event_function.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
          gen_statem:init_result(term()).
init(Players) ->
    process_flag(trap_exit, true),
    Scores = new_scorecard(Players),
    ?LOG_INFO(#{module => ?MODULE,
                line => ?LINE,
                function => ?FUNCTION_NAME,
                scores => Scores}),
    {ok, ready, Scores}.


handle_event({call, From}, get_scores, _State, Data) ->
    {keep_state, Data, [{reply, From, Data}]};

handle_event({call, From}, {record_result, GameResult}, _State, Data) ->
    % Update scores based on game result
    UpdatedData = update_scores_with_game_result(GameResult, Data),

    % Broadcast updated scores to all players
    broadcast_scores_update(UpdatedData),

    % Broadcast next hand starting message
    broadcast_next_hand_starting(UpdatedData),

    {keep_state, UpdatedData, [{reply, From, ok}]};

handle_event(_, _, State, Data) ->
    %% Ignore all other events
    {next_state, State, Data}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->
          any().
terminate(_Reason, _State, _Data) ->
    void.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(
        OldVsn :: term() | {down, term()},
        State :: term(), Data :: term(), Extra :: term()) ->
          {ok, NewState :: term(), NewData :: term()} |
          (Reason :: term()).
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
new_scorecard(Players) ->
    ScorecardForPlayer =
        fun
            (#{id := Id, socket := Socket}, Acc) ->
                Acc#{Id => #{id => Id, socket => Socket, score => 0}}
        end,
    lists:foldl(
      ScorecardForPlayer,
      #{hands => [], hand_count => 0},
      Players).

% Update scores with game result
-spec update_scores_with_game_result(erlskat_hand:game_result(), map()) -> map().
update_scores_with_game_result(GameResult, Data) ->
    Declarer = maps:get(declarer, GameResult),
    ActualGameValue = maps:get(actual_game_value, GameResult),
    HandCount = maps:get(hand_count, Data, 0),

    % Update declarer's score
    UpdatedData = case maps:get(Declarer, Data, undefined) of
        undefined ->
            ?LOG_WARNING(#{module => ?MODULE,
                          line => ?LINE,
                          declarer => Declarer,
                          action => declarer_not_found_in_scorecard}),
            Data;
        DeclarerData ->
            CurrentScore = maps:get(score, DeclarerData, 0),
            NewScore = CurrentScore + ActualGameValue,
            UpdatedDeclarerData = DeclarerData#{score => NewScore},
            Data#{Declarer => UpdatedDeclarerData}
    end,

    % Add this hand to the hands list and increment hand count
    HandResult = #{
        hand_number => HandCount + 1,
        declarer => Declarer,
        declarer_won => maps:get(declarer_won, GameResult),
        game_type => maps:get(game_type, GameResult),
        actual_game_value => ActualGameValue
    },
    Hands = maps:get(hands, UpdatedData, []),
    UpdatedData#{
        hands => [HandResult | Hands],
        hand_count => HandCount + 1
    }.

% Broadcast updated scores to all players
-spec broadcast_scores_update(map()) -> ok.
broadcast_scores_update(Data) ->
    % Extract player scores
    PlayerScores = maps:fold(fun(Key, Value, Acc) ->
        case Key of
            hands -> Acc;
            hand_count -> Acc;
            PlayerId -> [#{player_id => PlayerId, score => maps:get(score, Value, 0)} | Acc]
        end
    end, [], Data),

    % Get all players from the scores data to broadcast to
    AllPlayers = [PlayerId || #{player_id := PlayerId} <- PlayerScores],

    lists:foreach(
      fun(PlayerId) ->
              PlayerProc = maps:get(socket, maps:get(PlayerId, Data)),
              PlayerProc ! erlskat_client_responses:scores_update_broadcast(PlayerScores)
         end,
      AllPlayers).

% Broadcast next hand starting message
-spec broadcast_next_hand_starting(map()) -> ok.
broadcast_next_hand_starting(Data) ->
    NextHandNumber = maps:get(hand_count, Data, 0) + 1,

    % Get all players from the scores data
    AllPlayers = maps:fold(fun(Key, _Value, Acc) ->
        case Key of
            hands -> Acc;
            hand_count -> Acc;
            PlayerId -> [PlayerId | Acc]
        end
    end, [], Data),

    % Broadcast to all players
    lists:foreach(
      fun(PlayerId) ->
              PlayerProc = maps:get(socket, maps:get(PlayerId, Data)),
              PlayerProc ! erlskat_client_responses:next_hand_starting_broadcast(NextHandNumber)
      end,
      AllPlayers).

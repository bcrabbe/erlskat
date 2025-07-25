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
-export([get_scores/0]).
-export([record_result/1]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([handle_event/4]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API
%%%===================================================================
get_scores() ->
    gen_statem:call(?SERVER, get_scores).

record_result(HandResult) ->
    gen_statem:call(?SERVER, {record_result, HandResult}).

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
            (#{id := Id}, Acc) ->
                Acc#{Id => #{id => Id, score => 0}}
        end,
    lists:foldl(
      ScorecardForPlayer,
      #{hands => []},
      Players).

-module(erlskat_table_monitor_tests).

-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
start_test_() ->
    [{"Can start",
     {setup, fun start/0, fun stop/1, fun has_started/1}},
    {"Can start 2 at once",
      {setup, fun start_two/0, fun stop/1, fun has_started/1}}
    ].

one_leaver_test_() ->
    [{"notifies remaining players of disconnect",
      {setup, fun start_with_leaver/0, fun stop/1, fun receive_disconnect_messages/1}},
     {"notifies remaining players of timeout",
      {setup, fun start_with_leaver/0, fun stop/1, fun receive_timeout_messages/1}}
    ].
%% player_disconnects_test_() ->
%%     [{"when player leaves he should be removed from the waiting players",
%%      {setup, fun start/0, fun stop/1}},
%%      {"Once a game is started disconnects should not be monitored",
%%       {setup,
%%        fun () -> start(), mock_game_sup() end,
%%        fun (_) -> unload_mocks(), stop() end}}].

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
has_started(Pids) ->
    lists:map(
      fun (Pid) -> ?_assert(erlang:is_process_alive(Pid)) end,
      Pids).

-define(LEAVING_PLAYER_ID, 3).

receive_disconnect_messages(Pid) ->
    Resp1 = receive
                Msg1 -> Msg1
           after 2000 -> received_nothing
           end,
    Resp2 = receive
               Msg2 -> Msg2
           after 2000 -> received_nothing
           end,
    [?_assert(erlang:is_process_alive(Pid)),
     ?_assertMatch(#{player_disconnected := ?LEAVING_PLAYER_ID}, Resp1),
     ?_assertMatch(#{player_disconnected := ?LEAVING_PLAYER_ID}, Resp2)].


receive_timeout_messages(Pid) ->
    Resps = receive_n(4),
    %% io:format("~n~ngot ~p~n", Resps),
    lists:map(
      fun
          ({1, Resp1}) ->
              ?_assertMatch(#{player_disconnected := ?LEAVING_PLAYER_ID}, Resp1);
          ({2, Resp2}) ->
              ?_assertMatch(#{player_disconnected := ?LEAVING_PLAYER_ID}, Resp2);
          ({3, Resp3}) ->
              ?_assertMatch(#{player_timed_out := ?LEAVING_PLAYER_ID}, Resp3);
          ({4, Resp4}) ->
              ?_assertMatch(#{player_timed_out := ?LEAVING_PLAYER_ID}, Resp4)
      end,
      Resps).

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

-define(PLAYERS_PER_TEST, 3).

start_two() ->
    {ok, Pid} = erlskat_table_monitor:start_link(players(?PLAYERS_PER_TEST)),
    {ok, Pid2} = erlskat_table_monitor:start_link(players(?PLAYERS_PER_TEST)),
    [Pid, Pid2].

start() ->
    {ok, Pid} = erlskat_table_monitor:start_link(players(?PLAYERS_PER_TEST)),
    [Pid].

start_with_leaver() ->
    LeaverPid = spawn(
      fun() ->
              timer:sleep(100),
              exit(expected_mock_player_disconnect)
      end),
    {ok, Pid} = erlskat_table_monitor:start_link([player(?LEAVING_PLAYER_ID, LeaverPid) | players(2)]),
    Pid.


stop([Pid | []]) ->
    stop(Pid);
stop([Pid | Pids]) ->
    stop(Pid),
    stop(Pids);
stop(Pid) ->
    erlskat_table_monitor:stop(Pid).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

receive_n(N) ->
    lists:map(
              fun (Index) ->
                      Received = receive
                                     Msg -> Msg
                                 after 2000 -> received_nothing
                                 end,
                      {Index, Received}
              end,
              lists:seq(1,N)).
flush() ->
    receive
        M ->
            io:format("~n~ngot ~p~n",[M]),
            flush()
    after 0 ->
        ok
    end.

players(N) ->
    players(N, []).

players(0, Acc) ->
    Acc;
players(N, Acc) ->
    players(N-1, [player(N) | Acc]).

player(N, Pid) ->
    #{id => N, socket => Pid}.

player(N) ->
    #{id => N, socket => self()}.

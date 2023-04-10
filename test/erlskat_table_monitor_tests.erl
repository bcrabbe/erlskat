-module(erlskat_table_monitor_tests).

-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
happy_path_test_() ->
    [{"Can start",
     {setup, fun start/0, fun stop/1, fun can_start/1}},
    {"Can start 2 at once",
      {setup, fun start_two/0, fun stop/1, fun can_start/1}}
     %% {"A player can join",
     %%  {setup, fun start/0, fun stop/1, fun}},
     %% {"When 3 players join, a game is started",
     %%  {setup,
     %%   fun () -> start(), mock_game_sup() end,
     %%   fun (_) -> unload_mocks(), stop() end}}
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
can_start(Pids) ->
    lists:map(
      fun (Pid) -> ?_assert(erlang:is_process_alive(Pid)) end,
      Pids).

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
%% mock_game_sup() ->
%%     meck:new(erlskat_game_sup, [unstick, passthrough]),
%%     meck:expect(
%%       erlskat_game_sup,
%%       new_game,
%%       fun
%%           (Players) when length(Players) =:= 3 -> noice
%%       end).

%% unload_mocks() ->
%%     ok = meck:unload(erlskat_game_sup).

start_two() ->
    {ok, Pid} = erlskat_table_monitor:start_link(players(3)),
    {ok, Pid2} = erlskat_table_monitor:start_link(players(3)),
    [Pid, Pid2].

start() ->
    {ok, Pid} = erlskat_table_monitor:start_link(players(3)),
    [Pid].

stop(Pids) ->
    lists:map(
      fun (Pid) -> erlskat_table_monitor:stop(Pid) end,
      Pids).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
%% nothing here yet

%% flush() ->
%%     receive
%%         _M ->
%%             %% io:format("got ~p~n",[M]),
%%             flush()
%%     after 0 ->
%%         ok
%%     end.

players(N) ->
    players(N, []).

players(0, Acc) ->
    Acc;
players(N, Acc) ->
    players(N-1, [player() | Acc]).

player() ->
    #{id => uuid:get_v4(), socket => self()}.

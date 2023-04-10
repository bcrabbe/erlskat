-module(erlskat_bidding_tests).

-include_lib("eunit/include/eunit.hrl").

%% %%%%%%%%%%%%%%%%%%%%%%%%%%
%% %%% TESTS DESCRIPTIONS %%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%
%% happy_path_test_() ->
%%     [{"Can start",
%%      {setup, fun start/0, fun stop/1, fun can_start/1}},
%%      {"A player can join",
%%       {setup, fun start/0, fun stop/1}},
%%      {"When 3 players join, a game is started",
%%       {setup,
%%        fun () -> start(), mocks() end,
%%        fun (Arg) -> unload_mocks(), stop(Arg) end}}].

%% player_disconnects_test_() ->
%%     [{"when player leaves he should be removed from the waiting players",
%%      {setup, fun start/0, fun stop/1}},
%%      {"Once a game is started disconnects should not be monitored",
%%       {setup,
%%        fun () -> start(), mocks() end,
%%        fun (Arg) -> unload_mocks(), stop(Arg) end}}].

%% %%%%%%%%%%%%%%%%%%%%
%% %%% ACTUAL TESTS %%%
%% %%%%%%%%%%%%%%%%%%%%
%% can_start(#{game := Pid}) ->
%%     [?_assert(erlang:is_process_alive(Pid)),
%%      ?_assertEqual(Pid, whereis(erlskat_game))].

%% %%%%%%%%%%%%%%%%%%%%%%%
%% %%% SETUP FUNCTIONS %%%
%% %%%%%%%%%%%%%%%%%%%%%%%
%% mocks() ->
%%     meck:new(erlskat_game_sup, [unstick, passthrough]),
%%     meck:expect(
%%       erlskat_game_sup,
%%       new_game,
%%       fun
%%           (Players) when length(Players) =:= 3 -> noice
%%       end).

%% unload_mocks() ->
%%     ok = meck:unload(erlskat_game_sup).

%% start() ->
%%     {ok, Pid} = erlskat_game_sup:start_link(),
%%     {ok, Pid} = erlskat_game_sup:new_game(),
%%     flush(),
%%     #{game => Pid}.

%% stop(_) ->
%%     erlskat_game:stop().

%% %%%%%%%%%%%%%%%%%%%%%%%%
%% %%% HELPER FUNCTIONS %%%
%% %%%%%%%%%%%%%%%%%%%%%%%%
%% %% nothing here yet

%% flush() ->
%%     receive
%%         _M ->
%%             %% io:format("got ~p~n",[M]),
%%             flush()
%%     after 0 ->
%%         ok
%%     end.

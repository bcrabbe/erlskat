-module(erlskat_lobby_tests).

-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
happy_path_test_() ->
    [{"Can start",
     {setup, fun start/0, fun stop/1, fun can_start/1}},
     {"A player can join",
      {setup, fun start/0, fun stop/1, fun test_new_player/1}},
     {"When 3 players join, a game is started",
      {setup, fun start/0, fun stop/1, fun game_starts/1}}].


%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
can_start(#{lobby := Pid}) ->
    [?_assert(erlang:is_process_alive(Pid)),
     ?_assertEqual(Pid, whereis(erlskat_lobby))].

test_new_player(_) ->
    new_player(1),
    Resp = receive
              #{state := waiting,
                players := Players} when
                    length(Players) == 1 -> true
          after 2000 -> false
          end,
    [?_assert(Resp)].


game_starts(_) ->
    new_players(3),
    Resp = receive
              #{state := matched,
                players := Players} when
                    length(Players) == 3 -> true
          after 2000 -> false
          end,
    [?_assert(Resp)].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    {ok, GamePid} = erlskat_game_sup:start_link(),
    {ok, Pid} = erlskat_lobby:start_link(),
    #{game_sup => GamePid, lobby => Pid}.

stop(#{game_sup := GamePid, lobby := Pid}) ->
    gen_statem:stop(Pid),
    gen_statem:stop(GamePid).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
%% nothing here yet
new_players(N) -> [new_player(I) || I <- lists:seq(1, N)].

new_player(N) ->
    erlskat_lobby:new_player(#{id => N, socket => self()}).

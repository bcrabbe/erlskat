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

player_disconnects_test_() ->
    [{"when player leaves he should be removed from the waiting players",
     {setup, fun start/0, fun stop/1, fun test_player_leaves/1}}].

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
    meck:new(erlskat_game_sup, [unstick, passthrough]),
    meck:expect(
      erlskat_game_sup,
      new_game,
      fun
          (Players) when length(Players) =:= 3 -> noice
      end),
    new_players(3),
    Resp = receive
              #{state := matched,
                players := Players} when
                    length(Players) == 3 -> true
          after 2000 -> false
          end,
    [?_assert(Resp)].

test_player_leaves(_) ->
    spawn_link(
      fun() ->
              new_player(leaver),
              true = receive
                         #{state := waiting,
                           players := Players} when
                               length(Players) == 1 -> true
                     after 2000 -> false
                     end,
              timer:sleep(1000)
      end),
    timer:sleep(10),
    new_player(1),
    BeforeLeaving = receive
               #{state := waiting,
                 players := PlayersBeforeLeaving} when
                     length(PlayersBeforeLeaving) == 2 -> true
           after 2000 -> false
           end,
    timer:sleep(2000),
    new_player(2),
    AfterLeaving = receive
               #{state := waiting,
                 players := PlayersAfterLeaving} when
                     length(PlayersAfterLeaving) == 2 -> true
           after 2000 -> false
           end,
    [?_assert(BeforeLeaving),
     ?_assert(AfterLeaving)].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    {ok, Pid} = erlskat_lobby:start_link(),
    #{lobby => Pid}.

stop(_) ->
    erlskat_lobby:stop().

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
%% nothing here yet

new_players(N) -> [new_player(I) || I <- lists:seq(1, N)].

new_player(N) ->
    erlskat_lobby:new_player(#{id => N, socket => self()}).

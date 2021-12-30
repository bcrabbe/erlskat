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
      {setup,
       fun () -> start(), mock_game_sup() end,
       fun (Arg) -> unload_mocks(), stop(Arg) end,
       fun game_starts/1}}].

player_disconnects_test_() ->
    [{"when player leaves he should be removed from the waiting players",
     {setup, fun start/0, fun stop/1, fun player_leaves/1}},
     {"Once a game is started disconnects should not be monitored",
      {setup,
       fun () -> start(), mock_game_sup() end,
       fun (Arg) -> unload_mocks(), stop(Arg) end,
       fun player_leaves_after_game_started/1}}].

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

player_leaves(_) ->
    spawn(
      fun() ->
              new_player(leaver),
              true = receive
                  #{state := waiting,
                    players := [leaver]} ->
                             true
              after 2000 -> false
              end,
              timer:sleep(1600),
              exit(normal)
      end),
    timer:sleep(500),
    new_player(1),
    BeforeLeaving = receive
               #{state := waiting,
                 players := [1]}
                               ->
                            true
           after 2000 -> false
           end,
    timer:sleep(2000),
    new_player(2),
    AfterLeaving = receive
               #{state := waiting,
                 players := PlayersAfterLeaving} when
                     length(PlayersAfterLeaving) == 2 ->
                            lists:any(
                              fun
                                  (leaver) -> false;
                                  (_) -> true
                              end,
                              PlayersAfterLeaving)
           after 2000 -> false
           end,
    [?_assert(BeforeLeaving),
     ?_assert(AfterLeaving)].

player_leaves_after_game_started(_) ->
    new_players(2),
    timer:sleep(5),
    Resp1 = receive
                Msg1 -> Msg1
           after 2000 -> received_nothing
           end,
    Resp2 = receive
               Msg2 -> Msg2
           after 2000 -> received_nothing
           end,
    Resp3 = receive
               Msg3 -> Msg3
           after 2000 -> received_nothing
           end,
    spawn(
      fun() ->
              new_player(leaver),
              true = receive
                         #{players := [leaver, 2, 1]} -> true
                     after 2000 -> false
                     end,
              timer:sleep(100),
              exit(normal)
      end),
    timer:sleep(5),
    Resp4 = receive
                Msg4 -> Msg4
            after 2000 -> received_nothing
            end,
    Resp5 = receive
                Msg5 -> Msg5
            after 2000 -> received_nothing
            end,
    timer:sleep(500),
    Resp6 = receive
                Msg6 -> Msg6
            after 2000 -> received_nothing
            end,
    [?_assertEqual(#{state => waiting,
                    players => [1]}, Resp1),
     ?_assertEqual(#{state => waiting,
                     players => [2, 1]}, Resp2),
     ?_assertEqual(#{state => waiting,%two players receive this msg
                     players => [2, 1]}, Resp3),
     ?_assertEqual(#{state => matched,
                     players => [leaver, 2, 1]}, Resp4),
     ?_assertEqual(#{state => matched,
                     players => [leaver, 2, 1]}, Resp5),
     ?_assertEqual(received_nothing, Resp6)].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
mock_game_sup() ->
    meck:new(erlskat_game_sup, [unstick, passthrough]),
    meck:expect(
      erlskat_game_sup,
      new_game,
      fun
          (Players) when length(Players) =:= 3 -> noice
      end).

unload_mocks() ->
    ok = meck:unload(erlskat_game_sup).

start() ->
    {ok, Pid} = erlskat_lobby:start_link(),
    flush(),
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

flush() ->
    receive
        _M ->
            %% io:format("got ~p~n",[M]),
            flush()
    after 0 ->
        ok
    end.

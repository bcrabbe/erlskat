-module(erlskat_lobby_tests).

-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
happy_path_test_() ->
    [{"Can start",
     {setup, fun start/0, fun stop/1, fun can_start/1}},
     {"A player can join",
      {setup,
       fun () -> start(), simple_manager_mock() end,
       fun (Arg) -> unload_simple_manager_mock(), stop(Arg) end,
       fun test_new_player/1}},
     {"When 3 players join, a game is started",
      {setup,
       fun () -> start(), mocks() end,
       fun (Arg) -> unload_mocks(), stop(Arg) end,
       fun game_starts/1}}].

player_disconnects_test_() ->
    [{"when player leaves he should be removed from the waiting players",
     {setup,
      fun () -> start(), simple_manager_mock() end,
      fun (Arg) -> unload_simple_manager_mock(), stop(Arg) end,
      fun player_leaves/1}},
     {"Once a game is started disconnects should not be monitored",
      {setup,
       fun () -> start(), mocks() end,
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
              #{type := lobby_status,
                state := waiting,
                players := Players} when
                    length(Players) == 1 -> true
          after 2000 -> false
          end,
    [?_assert(Resp)].


game_starts(_) ->
    new_players(3),
    Resp = receive
              #{type := lobby_status,
                state := matched,
                players := Players} when
                    length(Players) == 3 -> true
          after 2000 -> false
          end,
    [?_assert(Resp)].

player_leaves(_) ->
    spawn(
      fun() ->
              new_player(leaver),
              timer:sleep(1600),
              exit(normal)
      end),
    true = receive
               #{type := lobby_status,
                 state := waiting,
                 players := [leaver]} ->
                   true
           after
               2000 -> false
           end,
    timer:sleep(500),
    new_player(1),
    BeforeLeaving = receive
               #{type := lobby_status,
                 state := waiting,
                 players := [1]}
                               ->
                            true
           after 2000 -> false
           end,
    timer:sleep(2000),
    new_player(2),
    AfterLeaving = receive
               #{type := lobby_status,
                 state := waiting,
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
                #{type := lobby_status} = Msg1 -> Msg1
           after 2000 -> received_nothing
           end,
    Resp2 = receive
               #{type := lobby_status} = Msg2 -> Msg2
           after 2000 -> received_nothing
           end,
    Resp3 = receive
               #{type := lobby_status} = Msg3 -> Msg3
           after 2000 -> received_nothing
           end,
    spawn(
      fun() ->
              new_player(leaver),
              timer:sleep(100),
              exit(normal)
      end),
    true = receive
               #{type := lobby_status,
                 players := [leaver, 2, 1]} -> true
           after 2000 -> false
           end,
    timer:sleep(5),
    Resp4 = receive
                #{type := lobby_status} = Msg4 -> Msg4
            after 2000 -> received_nothing
            end,
    Resp5 = receive
                #{type := lobby_status} = Msg5 -> Msg5
            after 2000 -> received_nothing
            end,
    timer:sleep(500),
    Resp6 = receive
                #{type := lobby_status} = Msg6 -> Msg6
            after 2000 -> received_nothing
            end,
    [?_assertEqual(#{type => lobby_status,
                    state => waiting,
                    players => [1]}, Resp1),
     ?_assertEqual(#{type => lobby_status,
                     state => waiting,
                     players => [2, 1]}, Resp2),
     ?_assertEqual(#{type => lobby_status,
                     state => waiting, % two players receive this msg
                     players => [2, 1]}, Resp3),
     ?_assertEqual(#{type => lobby_status,
                     state => matched,
                     players => [leaver, 2, 1]}, Resp4),
     ?_assertEqual(#{type => lobby_status,
                     state => matched,
                     players => [leaver, 2, 1]}, Resp5),
     ?_assertEqual(received_nothing, Resp6)].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
mocks() ->
    TestPid = self(),
    meck:new(erlskat_floor_manager, [unstick, passthrough]),
    meck:expect(
      erlskat_floor_manager,
      new_table,
      fun
          (Players) when length(Players) =:= 3 -> ok
      end),
    try meck:validate(erlskat_manager) of
        true -> meck:unload(erlskat_manager);
        false -> ok
    catch
        error:_ -> ok
    end,
    meck:new(erlskat_manager, [unstick, passthrough]),
    meck:expect(
      erlskat_manager,
      socket_response,
      fun(_PlayerId, Response) ->
          TestPid ! Response,
          ok
      end).

unload_mocks() ->
    ok = meck:unload(erlskat_floor_manager),
    ok = meck:unload(erlskat_manager).

simple_manager_mock() ->
    TestPid = self(),
    try meck:validate(erlskat_manager) of
        true -> meck:unload(erlskat_manager);
        false -> ok
    catch
        error:_ -> ok
    end,
    meck:new(erlskat_manager, [unstick, passthrough]),
    meck:expect(
      erlskat_manager,
      socket_response,
      fun(_PlayerId, Response) ->
          TestPid ! Response,
          ok
      end).

unload_simple_manager_mock() ->
    ok = meck:unload(erlskat_manager).

start() ->
    {ok, Pid} = erlskat_lobby:start_link(),
    flush(),
    #{lobby => Pid}.

stop(_) ->
    erlskat_lobby:stop().

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

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

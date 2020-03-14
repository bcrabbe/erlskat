-module(erlskat_trie_manager_tests).

-include_lib("eunit/include/eunit.hrl").

builder_test() ->
%    application:ensure_started(erlskat),
    erlskat:start(),
    ?assert(
       ok =:= erlskat_trie_manager:add_words(
                [{<<"testA">>, #{word => <<"testA">>}},
                 {<<"testB">>, #{word => <<"testB">>}}])),
    error_logger:info_report(
      [{module, ?MODULE},
       {line, ?LINE},
       {function, ?FUNCTION_NAME},
       {map, erlskat_trie_manager:trie_to_map()}]).

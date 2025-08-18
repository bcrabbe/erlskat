-module(erlskat_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2,
         all/0]).

-spec all() -> [atom()].
all() -> [].

-spec init_per_suite(list()) -> list().
init_per_suite(Config) ->
    application:start(erlskat),
    Config.

-spec end_per_suite(list()) -> ok.
end_per_suite(_Config) ->
    application:stop(erlskat),
    ok.

-spec init_per_testcase(atom(), list()) -> list().
init_per_testcase(_, Config) ->
    Config.

-spec end_per_testcase(atom(), list()) -> ok.
end_per_testcase(_, _Config) ->
    ok.

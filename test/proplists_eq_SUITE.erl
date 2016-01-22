-module(proplists_eq_SUITE).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-include("proplists_eq.hrl").

all() ->
    [proplists_eq_test].

proplists_eq_test(_) ->
    ?proplists_eq([1,2,3], [3,1,2]),
    try
        ?proplists_eq([1,2,3], [a,1,2]),
        ct:fail("assertion passed, but shouldn't")
    catch
        error:{assertEqual, _} -> ok
    end.

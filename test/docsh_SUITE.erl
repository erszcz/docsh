-module(docsh_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TESTED, example).

-define(eq(Expected, Actual), ?assertEqual(Expected, Actual)).

all() ->
    [edoc_example,
     recon].

edoc_example(C) -> test(C, edoc_example).

recon(C) -> test(C, recon).

test(_, Module) ->
    code:ensure_loaded(Module),
    ?eq(true, erlang:function_exported(Module, h, 0)),
    ?eq(ok, Module:h()).

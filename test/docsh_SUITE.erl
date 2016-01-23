-module(docsh_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TESTED, example).

-define(eq(Expected, Actual), ?assertEqual(Expected, Actual)).

all() ->
    [edoc_example,
     recon].

%%
%% Tests
%%

edoc_example(C) ->
    h0(C, edoc_example),
    h2(C, edoc_example).

recon(C) ->
    h0(C, recon),
    h2(C, recon).

%%
%% Helpers
%%

h0(_, Module) ->
    code:ensure_loaded(Module),
    ?eq(true, erlang:function_exported(Module, h, 0)),
    ?eq(ok, Module:h()).

h2(_, Module) ->
    code:ensure_loaded(Module),
    ?eq(true, erlang:function_exported(Module, h, 2)),
    {Fun, Arity} = hd(recon:module_info(exports) -- [{h,0}, {h,2}]),
    ?eq(ok, Module:h(Fun, Arity)).

-module(docsh_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TESTED, example).

-define(eq(Expected, Actual), ?assertEqual(Expected, Actual)).

all() ->
    [edoc_example_has_helpers,
     edoc_example_has_docs_from_debug_info,
     recon_has_helpers,
     recon_has_docs_from_debug_info].

%%
%% Tests
%%

edoc_example_has_helpers(C) ->
    h0(C, edoc_example),
    h2(C, edoc_example).

edoc_example_has_docs_from_debug_info(C) ->
    ?assert(has_docs(docsh_transform_in_memory(edoc_example))).

recon_has_helpers(C) ->
    h0(C, recon),
    h2(C, recon).

recon_has_docs_from_debug_info(C) ->
    ?assert(has_docs(docsh_transform_in_memory(recon))).

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
    {Fun, Arity} = hd(Module:module_info(exports) -- [{h,0}, {h,2}]),
    ?eq(ok, Module:h(Fun, Arity)).

docsh_transform_in_memory(Mod) ->
    {module, Mod} = code:ensure_loaded(Mod),
    File = code:which(Mod),
    {ok, NewBEAM} = docsh_lib:process_beam(File),
    NewBEAM.

has_docs(Mod) ->
    docsh_lib:has_exdc(Mod).

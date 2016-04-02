-module(docsh_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TESTED, example).

-define(eq(Expected, Actual), ?assertEqual(Expected, Actual)).

all() ->
    [edoc_example_has_helpers,
     edoc_example_has_docs_from_debug_info,
     edoc_example_has_docs_from_source,
     recon_has_helpers,
     recon_has_docs_from_debug_info,
     recon_has_docs_from_source].

%%
%% Tests
%%

edoc_example_has_helpers(C) ->
    h0(C, edoc_example),
    h2(C, edoc_example).

edoc_example_has_docs_from_debug_info(C) ->
    module_has_docs_from_debug_info(C, edoc_example).

edoc_example_has_docs_from_source(C) ->
    module_has_docs_from_source(C, edoc_example).

recon_has_helpers(C) ->
    h0(C, recon),
    h2(C, recon).

recon_has_docs_from_debug_info(C) ->
    module_has_docs_from_debug_info(C, recon).

recon_has_docs_from_source(C) ->
    module_has_docs_from_source(C, recon).

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

module_has_docs_from_debug_info(_, Mod) ->
    %% given
    File = code:which(Mod),
    ?assert(has_debug_info(File)),
    %% when
    BMod = docsh_transform_in_memory(File),
    %% then
    ?assert(has_docs(BMod)).

module_has_docs_from_source(_, Mod) ->
    %% given
    File = code:which(Mod),
    {ok, NoDebugInfoBMod} = strip_debug_info(File),
    ?assert(not has_debug_info(NoDebugInfoBMod)),
    %% when
    BMod = docsh_transform_in_memory(NoDebugInfoBMod),
    %% then
    ?assert(has_docs(BMod)).

docsh_transform_in_memory(File) ->
    {ok, NewBEAM} = docsh_lib:process_beam(File),
    NewBEAM.

has_docs(Mod) ->
    docsh_lib:has_exdc(Mod).

has_debug_info(Mod) ->
    case docsh_lib:get_debug_info(Mod) of
        {ok, _Abst} -> true;
        false -> false
    end.

has_source(Mod) ->
    case docsh_lib:get_source_file(Mod) of
        {ok, _} -> true;
        false -> false
    end.

strip_debug_info(BEAMFile) ->
    {ok, _, Chunks} = beam_lib:all_chunks(BEAMFile),
    NewChunks = lists:keystore("Abst", 1, Chunks, {"Abst", <<>>}),
    {ok, _NewBEAM} = beam_lib:build_module(NewChunks).

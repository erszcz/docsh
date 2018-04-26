-module(docsh_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TESTED, example).

-define(eq(Expected, Actual), ?assertEqual(Expected, Actual)).

all() ->
    [edoc_example_has_docs_from_debug_info,
     edoc_example_has_docs_from_source,
     recon_has_docs_from_debug_info,
     recon_has_docs_from_source,
     simple_lookup_should_just_work,
     simple_lookup_should_not_work_if_no_doc_is_available].

%%
%% Tests
%%

edoc_example_has_docs_from_debug_info(C) ->
    module_has_docs_from_debug_info(C, edoc_example).

edoc_example_has_docs_from_source(C) ->
    module_has_docs_from_source(C, edoc_example).

recon_has_docs_from_debug_info(C) ->
    module_has_docs_from_debug_info(C, recon).

recon_has_docs_from_source(C) ->
    module_has_docs_from_source(C, recon).

simple_lookup_should_just_work(_) ->
    {ok, Beam} = docsh_lib:get_beam(proplists),
    Doc = docsh_format:lookup(Beam, proplists, [moduledoc]),
    ct:pal("doc: ~p", [Doc]),
    ?assertMatch({ok, _}, Doc).

simple_lookup_should_not_work_if_no_doc_is_available(_) ->
    {ok, Beam} = docsh_lib:get_beam(lists),
    %% TODO: This is cheating, should return not_found, but we store a placeholder.
    {ok, Doc} = docsh_format:lookup(Beam, lists, [moduledoc]),
    ct:pal("doc: ~p", [Doc]),
    ?assertMatch({_,_}, binary:match(Doc, <<"is not available">>)).

%%
%% Helpers
%%

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
    {ok, NewBEAM, _} = docsh_lib:process_beam(File),
    NewBEAM.

has_docs(Mod) ->
    docsh_lib:has_docs(Mod).

has_debug_info(Mod) ->
    case docsh_lib:get_abstract_code(Mod) of
        {ok, _Abst} -> true;
        false -> false
    end.

has_source(Mod) ->
    case docsh_lib:get_source_file(Mod) of
        {ok, _} -> true;
        false -> false
    end.

strip_debug_info(BEAMFile) ->
    {ok, _, Chunks0} = beam_lib:all_chunks(BEAMFile),
    Chunks1 = lists:keydelete("Abst", 1, Chunks0),
    Chunks2 = lists:keydelete("Dbgi", 1, Chunks1),
    {ok, _NewBEAM} = beam_lib:build_module(Chunks2).

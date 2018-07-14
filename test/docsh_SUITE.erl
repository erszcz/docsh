-module(docsh_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TESTED, example).

-define(eq(Expected, Actual), ?assertEqual(Expected, Actual)).

all() ->
    [
     {group, docs_v1}
    ].

groups() ->
    [{docs_v1, [], tests()}].

tests() ->
    [edoc_example_has_docs_from_debug_info,
     edoc_example_has_docs_from_source,
     recon_has_docs_from_debug_info,
     recon_has_docs_from_source,
     simple_lookup_should_just_work,
     simple_lookup_should_not_work_if_no_doc_is_available].

init_per_suite(Config) ->
    ok = application:load(docsh),
    ok = application:set_env(docsh, enable_cache, false),
    Config.

end_per_suite(Config) -> Config.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(CaseName, Config) ->
    ct_helper:test_specific_init(?MODULE, CaseName, Config).

end_per_testcase(CaseName, Config) ->
    ct_helper:test_specific_end(?MODULE, CaseName, Config).

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
    {ok, Beam} = docsh_lib:get_docs(proplists),
    Doc = docsh_format:lookup(Beam, proplists, [moduledoc]),
    ct:pal("doc: ~p", [Doc]),
    ?assertMatch({ok, _}, Doc).

simple_lookup_should_not_work_if_no_doc_is_available(_) ->
    {ok, Beam} = docsh_lib:get_docs(lists),
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
    {ok, Beam} = docsh_beam:from_beam_file(File),
    %% when / then
    ?assertMatch({ok, _Docs, []}, docsh_lib:make_docs(Beam)).

module_has_docs_from_source(C, Mod) ->
    %% given
    File = code:which(Mod),
    {ok, NoDebugInfoBMod} = strip_debug_info(File),
    ?assert(not has_debug_info(NoDebugInfoBMod)),
    PrivDir = ?config(priv_dir, C),
    NewFile = filename:join([PrivDir, filename:basename(File)]),
    ok = file:write_file(NewFile, NoDebugInfoBMod),
    {ok, Beam} = docsh_beam:from_beam_file(NewFile),
    %% when / then
    ?assertMatch({ok, _Docs, [no_debug_info]}, docsh_lib:make_docs(Beam)).

docsh_transform_in_memory(File) ->
    File.

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
    Chunks3 = lists:keydelete("Docs", 1, Chunks2),
    {ok, _NewBEAM} = beam_lib:build_module(Chunks3).

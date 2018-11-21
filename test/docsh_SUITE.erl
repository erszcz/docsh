-module(docsh_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
     {group, docs_v1}
    ].

groups() ->
    [{docs_v1, [], tests()}].

tests() ->
    [
     edoc_example,
     proplists,
     recon,
     simple_lookup_should_not_work_if_no_doc_is_available
    ].

init_per_suite(Config) ->
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

edoc_example(C) ->
    simple_lookup(C, edoc_example).

proplists(C) ->
    simple_lookup(C, proplists).

recon(C) ->
    simple_lookup(C, recon).

simple_lookup_should_not_work_if_no_doc_is_available(_) ->
    {ok, Beam} = docsh_lib:get_docs(lists),
    {not_found, Doc} = docsh_format:lookup(Beam, lists, [moduledoc]),
    ct:pal("doc: ~p", [Doc]),
    ?assertMatch({_,_}, binary:match(Doc, <<"is not available">>)).

%%
%% Helpers
%%

simple_lookup(_, Module) ->
    {ok, Beam} = docsh_lib:get_docs(Module),
    Doc = docsh_format:lookup(Beam, Module, [moduledoc]),
    ct:pal("doc: ~p", [Doc]),
    ?assertMatch({ok, _}, Doc).

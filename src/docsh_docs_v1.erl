%% @doc EEP-0048 (http://erlang.org/eeps/eep-0048.html) docs_v1 documentation format support.
-module(docsh_docs_v1).

-behaviour(docsh_format).
-export([lookup/3]).

-behaviour(docsh_writer).
-export([from_internal/1]).

-export_type([t/0]).

-record(docs_v1, {anno,
                  beam_language,
                  format,
                  module_doc,
                  metadata,
                  docs}).

-opaque t() :: #docs_v1{anno :: erl_anno:anno(),
                        beam_language :: atom(),
                        format :: mime_type(),
                        module_doc :: i18n_doc() | none | hidden,
                        metadata :: map(),
                        docs :: [item()]}.

-type mime_type() :: binary().

-type item() :: {KNA :: docsh_format:kna(),
                 Anno :: erl_anno:anno(),
                 Signature :: [binary()],
                 Doc :: i18n_doc() | none | hidden,
                 Metadata :: map()}.

-ifndef(no_map_specs).
-type i18n_doc() :: #{language() := string_()}.
-type language() :: binary().
-type string_() :: binary().
-else.
-type i18n_doc() :: #{}.
-endif.

-spec lookup(docsh_format:t(), docsh_format:key(), docsh_format:kinds()) -> {ok, binary()}
                                                                          | {not_found, binary()}.
lookup(#docs_v1{} = _Docs, _Key, _Kinds) ->
    error(not_implemented),
    {not_found, <<"docsh: docs_v1 lookup not implemented">>}.

-spec from_internal(docsh_internal:t()) -> t().
from_internal(Internal) ->
    Grouped = docsh_internal:grouped(Internal),
    %% TODO: remove
    docsh_lib:print("~p\n", [Grouped]),
    %% TODO: it would be nice to get source location for the annotation here
    ModuleInfo = maps:get({module, 0}, Grouped),
    {DocsV1, DocsMap} = maps:fold(mk_step(ModuleInfo),
                                  {docs_v1_default(), #{}},
                                  Grouped),
    docsh_lib:print("DocsV1 : ~p\n", [DocsV1]),
    docsh_lib:print("DocsMap: ~p\n", [DocsMap]),
    DocsV1#docs_v1{docs = maps:values(DocsMap)}.

docs_v1_default() ->
    #docs_v1{anno = erl_anno:new({0, 1}),
             beam_language = erlang,
             format = <<"text/plain">>,
             module_doc = #{<<"en">> => <<"Documentation for the module is not available.\n">>},
             metadata = #{},
             docs = []}.

mk_step(ModuleInfo) ->
    fun (NameArity, Info, Acc) -> step(ModuleInfo, NameArity, Info, Acc) end.

%step(_ModuleInfo, {Name, Arity}, Info0, { #docs_v1{} = DocsV1, DocsMap }) ->
%    Info = flatten_info(Info0),
%    %docsh_lib:print("item: ~p\n", [Item]),
%    ct:pal("item: ~p\n", [Info]),
%    case make_item_doc(Info) of
%        skip ->
%            {DocsV1, DocsMap};
%        {KNA, _, _, _, _} = NewDoc ->
%            {DocsV1, DocsMap#{KNA => NewDoc}}
%    end.

step(_ModuleInfo, {module, 0}, _Info, { DocsV1, DocsMap }) ->
    { DocsV1, DocsMap };
step(_ModuleInfo, {Name, Arity}, Info0, { #docs_v1{} = DocsV1, DocsMap }) ->
    Info = flatten_info(Info0),
    %docsh_lib:print("item: ~p\n", [Item]),
    ct:pal("item: ~p\n", [Info]),
    %% TODO: discover item kind
    Kind = function,
    KNA = {Kind, Name, Arity},
    Entry = { {Kind, Name, Arity},
              erl_anno:new({0, 1}),
              signature(Name, Arity, Info),
              #{<<"en">> => description(Name, Arity, Info)},
              #{} },
    {DocsV1, DocsMap#{KNA => Entry}}.

flatten_info(Info) -> lists:foldl(fun flatten_info/2, [], Info).

flatten_info({{spec, _}, {description, BSpec}}, Acc) ->
    [{spec, BSpec} | Acc];
flatten_info({{type, _}, {description, BType}}, Acc) ->
    [{type, BType} | Acc];
flatten_info({{function, _}, FunctionInfo}, Acc) ->
    tuple_to_list(FunctionInfo) ++  Acc;
flatten_info({module, Info}, Acc) ->
    [{module, docsh_lib:get(name, Info)} | Acc].

%make_item_doc({module, _Info})               -> skip;
%make_item_doc({{type, _NameArity}, _})       -> skip;
%make_item_doc({{spec, _NameArity}, _})       -> skip;
%make_item_doc({{function, _}, _} = Function) ->
%    { kind_name_arity(Function),
%      erl_anno:new({0, 1}),
%      signature(Function),
%      #{<<"en">> => description(Function)},
%      #{} }.

signature(Name, Arity, Info) ->
    case docsh_lib:get(spec, Info, not_found) of
        not_found ->
            SName = atom_to_list(Name),
            SArity = integer_to_list(Arity),
            [iolist_to_binary([SName, "/", SArity])];
        Spec ->
            [Spec]
    end.

description(_Name, _Arity, Info) ->
    Default = <<"Documentation for the entry is not available.\n">>,
    case docsh_lib:get(description, Info, not_found) of
        not_found ->
            Default;
        undefined ->
            Default;
        Desc when is_binary(Desc) ->
            Desc
    end.

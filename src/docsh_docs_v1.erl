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

-define(a2b(A), atom_to_binary(A, utf8)).
-define(i2b(I), integer_to_binary(I)).
-define(il2b(IOList), iolist_to_binary(IOList)).

-spec lookup(docsh_format:t(), docsh_format:key(), docsh_format:kinds()) -> {ok, binary()}
                                                                          | {not_found, binary()}.
lookup(#docs_v1{} = Docs, Key, Kinds0) ->
    %docsh_lib:print("lookup ~p ~p in\n  ~p\n", [Key, Kinds0, Docs]),
    %% TODO: switch on environment language
    Lang = <<"en">>,
    %% This way we'll never get [spec, doc], only [doc, spec].
    Kinds = lists:sort(Kinds0),
    dispatch_lookup(Docs, Key, Kinds, Lang).

dispatch_lookup(Docs, Mod, [moduledoc], Lang) ->
    case Docs#docs_v1.module_doc of
        none    -> {ok, module_doc_not_available()};
        hidden  -> {ok, <<"Module documentation is hidden.\n">>};
        Doc     -> {ok, format_module_doc(Mod, maps:get(Lang, Doc))}
    end;
dispatch_lookup(Docs, {Mod, Name, Arity}, [type], Lang) ->
    Items = fetch_items(Docs#docs_v1.docs, type, Name, Arity),
    {ok, format_types(Mod, Items, Lang)};
dispatch_lookup(Docs, Mod, [type], Lang) ->
    Items = fetch_items(Docs#docs_v1.docs, type, any, any),
    {ok, format_types(Mod, Items, Lang)};
dispatch_lookup(Docs, {Mod, Name, Arity}, Kinds, Lang)
  when Kinds =:= [doc, spec];
       Kinds =:= [spec] ->
    Items = fetch_items(Docs#docs_v1.docs, function, Name, Arity),
    {ok, format_functions(Mod, Items, Kinds, Lang)}.

fetch_items(AllItems, Kind, Name, Arity) ->
    lists:filter(mk_select(Kind, Name, Arity), AllItems).

mk_select(Kind, Name, Arity) ->
    fun ({ItemKNA, _, _, _, _}) -> select(Kind, Name, Arity, ItemKNA) end.

select(Kind, Name, Arity, {Kind, ItemName, ItemArity})
  when Name =:= ItemName orelse Name =:= any,
       Arity =:= ItemArity orelse Arity =:= any ->
    true;
select(_, _, _, _) ->
    false.

format_module_doc(Mod, Doc) ->
    ?il2b(["\n# ", ?a2b(Mod), "\n\n", Doc, "\n"]).

format_functions(Mod, Items, Kinds, Lang) ->
    ?il2b([ ["\n", ?il2b([?a2b(Mod), ":", ?a2b(Name), "/", ?i2b(Arity), "\n\n",
                          Signature, DocIfRequested]), "\n"]
            || {{_, Name, Arity}, _, Signature, MaybeDoc, _Metadata} <- Items,
               DocIfRequested <- [ "" ++ [ ["\n", format_maybe_doc(MaybeDoc, Lang)]
                                           || lists:member(doc, Kinds) ] ] ]).

format_types(_Mod, Items, _Lang) ->
    ?il2b([ [?il2b([Signature])]
            || {{_, _Name, _Arity}, _, Signature, _, _Metadata} <- Items ]).

format_maybe_doc(none, _)   -> item_doc_not_available();
format_maybe_doc(hidden, _) -> <<"Documentation for the entry is hidden.\n">>;
format_maybe_doc(Doc, Lang) -> maps:get(Lang, Doc).

-spec from_internal(docsh_internal:t()) -> t().
from_internal(Internal) ->
    Grouped = docsh_internal:grouped(Internal),
    %% TODO: remove
    %docsh_lib:print("from_internal grouped ~p\n", [Grouped]),
    %% TODO: it would be nice to get source location for the annotation here
    ModuleInfo = maps:get({module, 0}, Grouped),
    {DocsV1, DocsMap} = maps:fold(mk_step(ModuleInfo),
                                  {docs_v1_default(), #{}},
                                  Grouped),
    %docsh_lib:print("DocsV1 : ~p\n", [DocsV1]),
    %docsh_lib:print("DocsMap: ~p\n", [DocsMap]),
    DocsV1#docs_v1{docs = maps:values(DocsMap)}.

docs_v1_default() ->
    #docs_v1{anno = erl_anno:new({0, 1}),
             beam_language = erlang,
             format = <<"text/plain">>,
             module_doc = #{<<"en">> => module_doc_not_available()},
             metadata = #{},
             docs = []}.

module_doc_not_available() ->
    <<"Documentation for the module is not available.\n">>.

item_doc_not_available() ->
    <<"Documentation for the entry is not available.\n">>.

mk_step(ModuleInfo) ->
    fun (NameArity, Info, Acc) -> step(ModuleInfo, NameArity, Info, Acc) end.

step(_ModuleInfo, {module, 0}, Info, { DocsV1, DocsMap }) ->
    [{module, InfoItems}] = Info,
    NewDocsV1 = DocsV1#docs_v1{module_doc = #{<<"en">> => module_doc(InfoItems)}},
    { NewDocsV1, DocsMap };
step(_ModuleInfo, {Name, Arity}, Info0, { #docs_v1{} = DocsV1, DocsMap }) ->
    Info = flatten_info(Info0),
    %docsh_lib:print("item: ~p\n", [Info]),
    %ct:pal("item: ~p\n", [Info]),
    Kind = infer_item_kind(Info),
    KNA = {Kind, Name, Arity},
    Entry = { {Kind, Name, Arity},
              erl_anno:new({0, 1}),
              signature(Kind, Name, Arity, Info),
              #{<<"en">> => description(Name, Arity, Info)},
              #{} },
    {DocsV1, DocsMap#{KNA => Entry}}.

flatten_info(Info) -> lists:foldl(fun flatten_info/2, [], Info).

flatten_info({{spec, _}, {description, BSpec}}, Acc) ->
    [{spec, BSpec} | Acc];
flatten_info({{type, _}, {description, BType}}, Acc) ->
    [{type, BType} | Acc];
flatten_info({{function, _}, FunctionInfo}, Acc) ->
    tuple_to_list(FunctionInfo) ++  Acc.

infer_item_kind(Info) ->
    case lists:keyfind(type, 1, Info) of
        false -> function;
        _ -> type
    end.

signature(Kind, Name, Arity, Info) ->
    InfoKey = case Kind of
                  function -> spec;
                  type -> type
              end,
    case docsh_lib:get(InfoKey, Info, not_found) of
        not_found ->
            SName = atom_to_list(Name),
            SArity = integer_to_list(Arity),
            [iolist_to_binary([SName, "/", SArity])];
        InfoItem ->
            [InfoItem]
    end.

module_doc(ModuleInfo) ->
    case docsh_lib:get(description, ModuleInfo, not_found) of
        not_found ->
            module_doc_not_available();
        Desc when is_binary(Desc) ->
            Desc
    end.

description(_Name, _Arity, Info) ->
    case docsh_lib:get(description, Info, not_found) of
        not_found ->
            item_doc_not_available();
        undefined ->
            item_doc_not_available();
        Desc when is_binary(Desc) ->
            Desc
    end.

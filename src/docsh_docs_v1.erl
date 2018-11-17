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
    ?il2b(["\n# ", ?a2b(Mod), "\n\n", docsh_edoc:format_edoc(Doc), "\n\n"]).

format_functions(Mod, Items, Kinds, Lang) ->
    ?il2b([ ["\n", ?il2b([?a2b(Mod), ":", ?a2b(Name), "/", ?i2b(Arity), "\n\n",
                          Signature, DocIfRequested]), "\n"]
            || {{_, Name, Arity}, _, Signature, MaybeDoc, _Metadata} <- Items,
               DocIfRequested <- [ "" ++ [ ["\n", format_maybe_doc(MaybeDoc, Lang), "\n"]
                                           || lists:member(doc, Kinds) ] ] ]).

format_types(_Mod, Items, _Lang) ->
    ?il2b([ [?il2b([Signature])]
            || {{_, _Name, _Arity}, _, Signature, _, _Metadata} <- Items ]).

format_maybe_doc(none, _)   -> item_doc_not_available();
format_maybe_doc(hidden, _) -> <<"Documentation for the entry is hidden.\n">>;
format_maybe_doc(Doc, Lang) -> docsh_edoc:format_edoc(maps:get(Lang, Doc)).

-spec from_internal(docsh_internal:t()) -> t().
from_internal(Internal) ->
    %% TODO: remove
    %docsh_lib:print("internal ~p\n", [Internal]),
    #{name := ModuleName,
      description := Description} = Internal,
    %% TODO: this contains some assumptions, e.g. English language
    Lang = <<"en">>,
    ModuleInfo = #{name => ModuleName,
                   lang => Lang},
    Docs0 =
        (docs_v1_default())#docs_v1{format = <<"text/erlang-edoc">>,
                                    module_doc = module_doc(Lang, Description)},
    %% TODO: it would be nice to get source location for the annotation here
    {Docs, DocsMap} = lists:foldl(mk_step(ModuleInfo),
                                  {Docs0, #{}},
                                  maps:get(items, Internal)),
    %docsh_lib:print("Docs : ~p\n", [Docs]),
    %docsh_lib:print("DocsMap: ~p\n", [DocsMap]),
    Docs#docs_v1{docs = maps:values(DocsMap)}.

docs_v1_default() ->
    #docs_v1{anno = erl_anno:new({0, 1}),
             beam_language = erlang,
             format = <<"text/plain">>,
             module_doc = none,
             metadata = #{},
             docs = []}.

module_doc(_Lang, none) -> none;
module_doc( Lang, D) -> #{Lang => D}.

module_doc_not_available() ->
    <<"Documentation for the module is not available.\n">>.

item_doc_not_available() ->
    <<"Documentation for the entry is not available.\n">>.

mk_step(ModuleInfo) ->
    fun (Info, Acc) -> step(ModuleInfo, Info, Acc) end.

step(ModuleInfo, Info, { #docs_v1{} = DocsV1, DocsMap }) ->
    %docsh_lib:print("item: ~p\n", [Info]),
    %ct:pal("item: ~p\n", [Info]),
    {_, Name, Arity} = KNA = docsh_internal:kna(Info),
    Entry = { KNA,
              erl_anno:new({0, 1}),
              signature(ModuleInfo, Info),
              description(Name, Arity, Info, ModuleInfo),
              #{} },
    {DocsV1, DocsMap#{KNA => Entry}}.

signature(ModuleInfo, Info) ->
    case maps:get(signature, Info, no_signature) of
        no_signature ->
            %% TODO: this is kind of lame
            %% as with the current format function it displays the same text twice
            kna_signature(ModuleInfo, Info);
        Signature ->
            Signature
    end.

kna_signature(ModuleInfo, Info) ->
    #{name := Mod} = ModuleInfo,
    {_, Name, Arity} = docsh_internal:kna(Info),
    ?il2b(io_lib:format("~s:~s/~p\n", [Mod, Name, Arity])).

description(_Name, _Arity, Info, #{lang := Lang}) ->
    case maps:get(description, Info, none) of
        none -> none;
        D -> #{Lang => D}
    end.

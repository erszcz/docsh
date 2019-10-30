%% @doc Documentation lookup functions exported in the Erlang shell.

-module(docsh_erl).

-export([h/1, h/2, h/3,
         s/1, s/2, s/3,
         t/1, t/2, t/3]).

-import(docsh_lib, [print/2]).

-include_lib("docsh/include/docsh_exdoc.hrl").

-define(a2b(A), atom_to_binary(A, utf8)).
-define(i2b(I), integer_to_binary(I)).
-define(il2b(IOList), iolist_to_binary(IOList)).
-define(il2l(IOList), binary_to_list(iolist_to_binary(IOList))).

%% Function or type name.
-type name() :: docsh_internal:name().

%% @doc When invoked with a module as an argument like `h(lists)',
%% then look up the module documentation.
%% When invoked with a fun as the argument like `h(fun lists:keyfind/3)',
%% then equivalent to `h(M, F, A)' with the the fun's deduced
%% `{M :: module(), F :: name(), A :: arity()}'.
-spec h(fun() | module()) -> ok.
h(Fun) when is_function(Fun) ->
    {M, F, A} = erlang:fun_info_mfa(Fun),
    h(M, F, A);

h(M) when is_atom(M) ->
    lookup(M, [moduledoc]).

%% @doc Look up function documentation. Equivalent to `h(M, F, any)'.
-spec h(module(), name()) -> ok.
h(M, F) -> h(M, F, any).

%% @doc Look up function documentation.
%% If arity is 'any', then all matching functions' docs will be printed.
%% The complete info is function name/arity, spec and description.
%% Use `s/{1,2,3}' to print just the spec.
-spec h(module(), name(), arity() | 'any') -> ok.
h(M, F, Arity) when is_atom(M), is_atom(F),
                    is_integer(Arity) orelse Arity =:= any ->
    lookup({M, F, Arity}, [doc, spec]).

%% @doc Look up a function spec.
%% Similar to `h/1' used with a fun, but prints just the spec.
-spec s(fun()) -> ok.
s(Fun) when is_function(Fun) ->
    {M, F, A} = erlang:fun_info_mfa(Fun),
    s(M, F, A).

%% @doc Look up function spec. Equivalent to `s(M, F, any)'.
-spec s(module(), name()) -> ok.
s(M, F) -> s(M, F, any).

%% @doc Look up function spec.
%% If arity is 'any' print specs for all matching functions.
-spec s(module(), name(), arity() | 'any') -> ok.
s(M, F, Arity) when is_atom(M), is_atom(F),
                    is_integer(Arity) orelse Arity =:= any ->
    lookup({M, F, Arity}, [spec]).

%% @doc Look up all types defined in module `M'.
-spec t(module()) -> ok.
t(M) when is_atom(M) ->
    lookup(M, [type]).

%% @doc Look up type definition. Equivalent to `t(M, T, any)'.
-spec t(module(), name()) -> ok.
t(M, T) -> t(M, T, any).

%% @doc Look up type definition.
%% If arity is 'any' print definitions for all matching types.
-spec t(module(), name(), arity() | 'any') -> ok.
t(M, T, Arity) when is_atom(M), is_atom(T),
                    is_integer(Arity) orelse Arity =:= any ->
    lookup({M, T, Arity}, [doc, type]).

-spec lookup(Key, Kinds) -> 'ok' when
      Key :: docsh_internal:key(),
      Kinds :: [docsh_internal:item_kind()].
lookup(Key, Kinds) ->
    %% TODO: switch on environment language
    Lang = <<"en">>,
    case docsh_lib:get_docs(key_to_module(Key)) of
        {error, R} -> erlang:error(R, Key);
        {ok, Docs} ->
            case docsh_format:lookup(Docs, Key, Kinds) of
                {not_found, Message} ->
                    print("~ts", [[string:strip(?il2l([Message]), right, $\n), "\n\n"]]);
                {ok, DocItems} ->
                    print("~ts", [format(DocItems, Key, Kinds, Lang)])
            end
    end.

format([DocItem], Mod, [moduledoc], Lang)  -> format_module_doc(Mod, maps:get(Lang, DocItem));
format(DocItems, {Mod, _, _}, Kinds, Lang) -> format_items(Mod, DocItems, Kinds, Lang);
format(DocItems, Mod, Kinds, Lang)         -> format_items(Mod, DocItems, Kinds, Lang).

format_module_doc(Mod, Doc) ->
    RenderingContext = #{},
    ?il2b(["\n# ", ?a2b(Mod), "\n\n", docsh_edoc:format_edoc(Doc, RenderingContext)]).

format_items(Mod, Items, Kinds, Lang) ->
    ?il2b([string:strip(?il2l([ format_item(Mod, Item, Kinds, Lang)
                                || Item <- Items ]), right, $\n),
           "\n\n"]).

format_item(Mod, Item, Kinds, Lang) ->
    {{_, Name, Arity}, _, Signature, MaybeDoc, _Metadata} = Item,
    [
     [ ["\n", ?a2b(Mod), ":", ?a2b(Name), "/", ?i2b(Arity), "\n\n"]
       || lists:member(doc, Kinds) orelse lists:member(spec, Kinds) ],
     [Signature,
      [ ["\n", format_maybe_doc(MaybeDoc, Lang)]
        || lists:member(doc, Kinds) ]]
    ].

format_maybe_doc(none, _)   -> docsh_format:item_doc_not_available();
format_maybe_doc(hidden, _) -> docsh_format:item_doc_hidden();
format_maybe_doc(Doc, Lang) ->
    RenderingContext = #{},
    docsh_edoc:format_edoc(maps:get(Lang, Doc), RenderingContext).

-spec key_to_module(docsh_internal:key()) -> module().
key_to_module(M) when is_atom(M) -> M;
key_to_module({M,_,_}) -> M.

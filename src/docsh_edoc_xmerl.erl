-module(docsh_edoc_xmerl).

-export(['#root#'/4,
         '#element#'/5,
         '#text#'/1,
         '#xml-inheritance#'/0]).

%-export([dd/4,
%         dl/4,
%         fullDescription/4,
%         li/4]).

-include_lib("xmerl/include/xmerl.hrl").

-define(il2b(IOList), iolist_to_binary(IOList)).
-define(l2b(L), list_to_binary(L)).
-define(l2i(L), list_to_integer(L)).
-define(l2ea(L), list_to_existing_atom(L)).

%%% Unused.
-spec '#xml-inheritance#'() -> list().
'#xml-inheritance#'() -> [].

%% The '#root#' tag is called when the entire structure has been
%% exported. It does not appear in the structure itself.
-spec '#root#'(any(), any(), any(), any()) -> #{name        := atom(),
                                                description := binary(),
                                                functions   := [],
                                                types       := []}.
'#root#'([#xmlElement{name = module} = Module], _, _, _) ->
    #{name => get_module_name(Module),
      description => get_module_description(Module),
      functions => get_functions(Module),
      types => get_types(Module)}.

-spec '#element#'(any(), any(), any(), any(), any()) -> any().
'#element#'(_, _, _, _, E) -> E.

%% The '#text#' function is called for every text segment.
-spec '#text#'(any()) -> any().
'#text#'(Text) -> ?il2b(Text).

get_module_name(#xmlElement{attributes = Attrs}) ->
    ?l2ea('find_attribute!'(name, Attrs)).

get_module_description(#xmlElement{name = module} = M) ->
    get_description(M).

get_functions(#xmlElement{name = module} = M) ->
    get_content(functions, [], fun get_functions/1, M);
get_functions(#xmlElement{name = functions, content = Content}) ->
    [ get_function(Function) || #xmlElement{name = function} = Function <- Content ].

-spec get_function(#xmlElement{}) -> #{kind        := 'function',
                                       name        := atom(),
                                       arity       := arity(),
                                       exported    := boolean(),
                                       description := binary()}.
get_function(#xmlElement{attributes = Attrs} = Function) ->
    #{kind        => 'function',
      name        => ?l2ea('find_attribute!'(name, Attrs)),
      arity       => ?l2i('find_attribute!'(arity, Attrs)),
      exported    => list_to_boolean('find_attribute!'(exported, Attrs)),
      description => get_function_description(Function)}.

get_types(#xmlElement{name = module} = M) ->
    get_content(typedecls, [], fun get_types/1, M);
get_types(#xmlElement{name = typedecls, content = Content}) ->
    [ get_type(Type) || #xmlElement{name = typedecl} = Type <- Content ].

-spec get_type(#xmlElement{}) -> #{kind        := 'type',
                                   name        := atom(),
                                   arity       := arity(),
                                   description := binary()}.
get_type(#xmlElement{name = typedecl} = Type) ->
    #{kind        => 'type',
      name        => get_type_name(Type),
      arity       => get_type_arity(Type),
      description => get_type_description(Type)}.

get_function_description(#xmlElement{name = function} = Function) ->
    get_description(Function).

get_type_name(#xmlElement{name = typedecl} = Type) ->
    get_type_def(fun get_type_name/1, Type);
get_type_name(#xmlElement{name = typedef} = TypeDef) ->
    case get_content(erlangName, {error, no_erlang_name}, fun get_type_name/1, TypeDef) of
        {error, no_erlang_name} -> erlang:error({not_found, erlangName});
        TypeName -> TypeName
    end;
get_type_name(#xmlElement{name = erlangName, attributes = Attrs}) ->
    ?l2ea('find_attribute!'(name, Attrs)).

get_type_arity(#xmlElement{name = typedecl} = Type) ->
    get_type_def(fun get_type_arity/1, Type);
get_type_arity(#xmlElement{name = typedef} = TypeDef) ->
    case get_content(argtypes, {error, no_argtypes}, fun get_type_arity/1, TypeDef) of
        {error, no_argtypes} -> erlang:error({not_found, argtypes});
        TypeArity -> TypeArity
    end;
get_type_arity(#xmlElement{name = argtypes, content = Content}) ->
    count_args(Content).

count_args(Args) ->
    length([ Arg || #xmlElement{name = type} = Arg <- Args ]).

get_type_description(#xmlElement{name = typedecl} = Type) ->
    get_description(Type).

%% Intended only for tracing.
debug(_, _) -> ok.

get_content(Name, Default, ContinueFun, #xmlElement{content = Content} = Element) ->
    case lists:keyfind(Name, #xmlElement.name, Content) of
        false -> debug({not_found, Name}, Element),
                 Default;
        #xmlElement{} = Found -> ContinueFun(Found)
    end.

get_description(#xmlElement{} = Element) ->
    get_content(description, <<>>, fun get_full_description/1, Element).

get_full_description(#xmlElement{name = description} = D) ->
    get_content(fullDescription, <<>>, fun get_full_description/1, D);
get_full_description(#xmlElement{name = fullDescription, content = Content}) ->
    format_text(Content).

%% TODO: this might need extending
format_text([#xmlText{value = Text}]) ->
    ?il2b(Text).

get_type_def(ContinueFun, #xmlElement{name = typedecl} = Type) ->
    case get_content(typedef, {error, no_typedef}, ContinueFun, Type) of
        {error, no_typedef} -> erlang:error({not_found, typedef, Type});
        ContinuationResult -> ContinuationResult
    end.

list_to_boolean("yes") -> true;
list_to_boolean("no")  -> false.

'find_attribute!'(Attr, Attrs) ->
    case xmerl_lib:find_attribute(Attr, Attrs) of
        false -> erlang:error({no_attribute, Attr, Attrs});
        {value, Value} -> Value
    end.

%%% The '#element#' function is the default handler for XML elements.
%-spec '#element#'(any(), any(), any(), any(), any()) -> any().
%'#element#'(function, Data, Attrs, _Parents, _E) ->
%    F1 = function_details_from_attrs(Attrs, #function{}),
%    F2 = function_details_from_data(lists:flatten(Data), F1),
%    NameArity = {F2#function.name, F2#function.arity},
%    [{{function, NameArity},
%      debug(function, {{name,        F2#function.name},
%                       {arity,       F2#function.arity},
%                       {exported,    F2#function.exported},
%                       {label,       F2#function.label},
%                       {description, F2#function.description}})}];
%'#element#'(see, Data, _Attrs, _Parents, _E) ->
%    {fmt, debug(see, ["See ", Data, "\n"])};
%'#element#'(equiv, Data, _Attrs, _Parents, _E) ->
%    Desc = case collect_loose_text(Data) of
%               [{fmt, Eq}, {fmt, See}] ->
%                   {description, ?il2b(["Equivalent to ", Eq, See])};
%               [{fmt, Eq}] ->
%                   {description, ?il2b(["Equivalent to ", Eq, "\n"])}
%           end,
%    debug(equiv, Desc);
%'#element#'(functions, Data, _Attrs, _Parents, _E) ->
%    %% Functions are already extracted.
%    [{functions, Data}];
%'#element#'(module, Data, Attrs, _Parents, _E) ->
%    Details = module_details_from_data(Data),
%    {value, {functions, Functions}, Rest} = lists:keytake(functions, 1, Details),
%    [{module, module_details_from_attrs(Attrs) ++ Rest}
%     | Functions];
%'#element#'(briefDescription, _Data, _Attrs, _Parents, _E) ->
%    [];
%'#element#'(description, Data, _Attrs, _Parents, _E) ->
%    [{description, debug(description, ?il2b(Data))}];
%'#element#'(Tag, Data, _Attrs, _Parents, _E)
%        when Tag =:= h1;
%             Tag =:= h2;
%             Tag =:= h3 ->
%    {fmt, debug(inline, header(Tag, unwrap_inline(Data)))};
%'#element#'(Tag, Data, _Attrs, _Parents, _E)
%        when Tag =:= a;
%             Tag =:= code;
%             Tag =:= em;
%             Tag =:= expr;
%             Tag =:= h4;
%             Tag =:= h5;
%             Tag =:= h6;
%             Tag =:= tt ->
%    debug('inline:before', Data),
%    After = [ unwrap_inline(E) || E <- Data ],
%    debug('inline:after', After),
%    debug(Tag, After),
%    {inline, After};
%'#element#'(Tag, Data, _Attrs, _Parents, _E) when
%        Tag =:= dt ->
%    {dt, debug(Tag, Data)};
%'#element#'(Tag, Data, _Attrs, _Parents, _E) when
%        Tag =:= p ->
%    {fmt, debug(Tag, cleanup_lines(Data))};
%'#element#'(Tag, Data, _Attrs, _Parents, _E) when
%        Tag =:= pre ->
%    {fmt, debug(Tag, [Data, "\n"])};
%'#element#'(Tag, Data, _Attrs, _Parents, _E) when
%        Tag =:= ol;
%        Tag =:= ul ->
%    list(Tag, Data);
%'#element#'(Tag, Data, _Attrs, _Parents, _E) ->
%    debug(discarded, {Tag, Data}),
%    [].

%unwrap_inline([]) -> [];
%unwrap_inline([{inline, Elements}]) when is_list(Elements) -> Elements;
%unwrap_inline({inline, Elements}) when is_list(Elements) -> Elements;
%unwrap_inline([{fmt, Elements}]) when is_list(Elements) -> Elements;
%unwrap_inline({fmt, Elements}) when is_list(Elements) -> Elements;
%unwrap_inline([BString]) when is_binary(BString) -> BString;
%unwrap_inline(BString) when is_binary(BString) -> BString.

%debug(Tag, Content) ->
%    docsh_lib:debug(Tag, "~s: ~p~n", [Tag, Content]),
%    Content.

%cleanup_lines(BString) when is_binary(BString) ->
%    Lines = re:replace(BString, <<"\s*\n\s*">>, <<"\n">>, [global, {return, list}]),
%    S = string:strip(lists:flatten(Lines), both, $\n),
%    [ [T, "\n"] || T <- string:tokens(S, "\n") ];
%cleanup_lines(IOList) ->
%    BString = ?il2b([ unwrap_inline(E) || E <- IOList ]),
%    cleanup_lines(BString).

%-spec fullDescription(any(), any(), any(), any()) -> binary().
%fullDescription(Data, _Attrs, _Parents, _E) ->
%    [{fmt, H} | T] = collect_loose_text(Data),
%    ?il2b([H] ++ [ ["\n", E] || {fmt, E} <- T ]).

%-spec li(any(), any(), any(), any()) -> {atom(), [any()]}.
%li(Data, _Attrs, _Parents, _E) ->
%    item(li, fmt, Data).

%-spec dd(any(), any(), any(), any()) -> {atom(), [any()]}.
%dd(Data, _Attrs, _Parents, _E) ->
%    item(dd, dd, Data).

%-spec item(atom(), atom(), any()) -> {atom(), [any()]}.
%item(Type, Out, Data) ->
%    {Out, debug(Type, lists:flatmap(fun unwrap_fmt/1, collect_loose_text(Data)))}.

%-spec dl(any(), any(), any(), any()) -> {atom(), [any()]}.
%dl(Data, _Attrs, _Parents, _E) ->
%    debug('dl:in', Data),
%    {fmt, debug(dl, [ itemize_dl(Type, Content)
%                      || E <- Data,
%                         {Type, Content} <- unwrap_dl_content(E) ])}.

%unwrap_dl_content({dt, BString}) when is_binary(BString) -> [{dt, BString}];
%unwrap_dl_content({dt, [BString]}) when is_binary(BString) -> [{dt, BString}];
%unwrap_dl_content({dd, Lines}) when is_list(Lines) ->
%    Length = length(Lines),
%    [ {dd, append_dd_trailing_newline(Nth, Length, L)}
%      || {Nth, L} <- enumerate(Lines) ];
%unwrap_dl_content(C) ->
%    debug('unwrap_dl_content:discard', C),
%    [].

%append_dd_trailing_newline(Length, Length, L) -> [L, "\n"];
%append_dd_trailing_newline(_Nth, _Length, L) -> L.

%list(Type, Data) ->
%    %% Two passes to only enumerate items that get through the first pass.
%    %% Whitespace is filtered out, the remainings are the list items.
%    Items = enumerate([ Unwrapped
%                        || E <- Data,
%                           [_|_] = Unwrapped <- [unwrap_fmt(E)] ]),
%    {fmt, debug(Type, [ debug(itemize, itemize(Type, N, Line, Item))
%                        || {N, E} <- Items,
%                           {Line, Item} <- enumerate(E) ])}.

%-spec itemize_dl(dt | dd, iolist()) -> iolist().
%itemize_dl(dt, Content) -> ["  ", Content, "\n\n"];
%itemize_dl(dd, Content) -> ["      ", Content].

%-spec itemize(Type, Nth, Line, Content) -> iolist() when
%      Type :: ol | ul,
%      Nth :: pos_integer(),
%      Line :: pos_integer(),
%      Content :: iolist().
%itemize(ol,  Nth, 1, Content) -> [io_lib:format("  ~b. ", [Nth]), Content];
%itemize(ol, _Nth, _, Content) -> ["    ", Content];
%itemize(ul, _Nth, 1, Content) -> ["  - ", Content];
%itemize(ul, _Nth, _, Content) -> ["    ", Content].

%enumerate(List) ->
%    lists:zip(lists:seq(1, length(List)), List).

%unwrap_fmt({fmt, Lines}) -> Lines;
%unwrap_fmt(_) -> [].

%collect_loose_text(Data) ->
%    debug(loose, collect_loose_text(Data, [], [])).

%collect_loose_text([], [], Fmt) -> Fmt;
%collect_loose_text([], Data, Fmt) ->
%    [{fmt, cleanup_lines(?il2b(lists:reverse(Data)))} | Fmt];
%collect_loose_text([{inline, Element} | T], [], Fmt) ->
%    collect_loose_text(T, [Element], Fmt);
%collect_loose_text([{fmt, Element} | T], [], Fmt) ->
%    [{fmt, Element} | collect_loose_text(T, [], Fmt)];
%collect_loose_text([{inline, Element} | T], Data, Fmt) ->
%    collect_loose_text(T, [Element | Data], Fmt);
%collect_loose_text([{fmt, Element} | T], Data, Fmt) ->
%    Clean = cleanup_lines(?il2b(lists:reverse(Data))),
%    case Clean of
%        [] -> [{fmt, Element}];
%        _ -> [{fmt, Clean}, {fmt, Element}]
%    end ++ collect_loose_text(T, [], Fmt);
%collect_loose_text([LooseText | T], Data, Fmt) when is_binary(LooseText);
%                                                    is_list(LooseText) ->
%    collect_loose_text(T, [LooseText | Data], Fmt).

%header(Level, Data) ->
%    ["\n", header_prefix(Level), Data, "\n"].

%header_prefix(h1) -> <<"# ">>;
%header_prefix(h2) -> <<"## ">>;
%header_prefix(h3) -> <<"### ">>.

%module_details_from_attrs(Attrs) ->
%    [ D || At <- Attrs, D <- [module_detail_from_attr(At)], D /= ignore ].

%module_detail_from_attr(#xmlAttribute{name = name} = At) ->
%    {name, ?l2ea(value(At))};
%module_detail_from_attr(#xmlAttribute{}) ->
%    ignore.

%module_details_from_data(Data) ->
%    lists:flatmap(fun module_detail_from_data/1, Data).

%module_detail_from_data([{description, _}] = Desc) ->
%    Desc;
%module_detail_from_data([{functions, _}] = Functions) ->
%    Functions;
%module_detail_from_data(_) -> [].

%function_details_from_attrs(Attrs, F) ->
%    lists:foldl(fun fda/2, F, Attrs).

%fda(#xmlAttribute{name = name} = At, F)     -> F#function{name = ?l2ea(value(At))};
%fda(#xmlAttribute{name = arity} = At, F)    -> F#function{arity = ?l2i(value(At))};
%fda(#xmlAttribute{name = exported} = At, F) -> F#function{exported = string_to_boolean(value(At))};
%fda(#xmlAttribute{name = label} = At, F)    -> F#function{label = ?l2b(value(At))}.

%function_details_from_data(Data, F) ->
%    lists:foldl(fun fdd/2, F, Data).

%fdd({description, Desc}, F) -> F#function{description = Desc};
%fdd(_, F)                   -> F.

%value(#xmlAttribute{value = V}) -> V.

%string_to_boolean("yes") -> true;
%string_to_boolean("no") -> false.

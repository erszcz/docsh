-module(docsh_edoc_xmerl).

-export(['#root#'/4,
         '#element#'/5,
         '#text#'/1,
         '#xml-inheritance#'/0]).

-include_lib("xmerl/include/xmerl.hrl").

-define(il2b(IOList), iolist_to_binary(IOList)).
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
      items => [ {kna(Item), Item}
                 || Item <- get_functions(Module) ++ get_types(Module) ]}.

kna(#{kind := K, name := N, arity := A}) -> {K, N, A}.

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
      %% TODO: really always true? anyway, we want the structure for functions and types
      %% to be the same
      exported    => true,
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

format_text(TextSubtree) ->
    %% Just return the EDoc subtree for storage or later processing.
    TextSubtree.

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

%% Intended only for tracing.
debug(_, _) -> ok.

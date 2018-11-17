-module(docsh_edoc_xmerl).

%% xmerl:simple_export/2 API
-export(['#root#'/4,
         '#element#'/5,
         '#text#'/1,
         '#xml-inheritance#'/0]).

%% EDoc formatter
-export([format_content/2]).

-export_type([xml_element_content/0]).

-include_lib("xmerl/include/xmerl.hrl").

%% @type xml_element_content(). `#xmlElement.content' as defined by `xmerl.hrl'.
-type xml_element_content() :: [#xmlElement{} | #xmlText{} | #xmlPI{} | #xmlComment{} | #xmlDecl{}].

-define(il2b(IOList), iolist_to_binary(IOList)).
-define(l2i(L), list_to_integer(L)).
-define(l2ea(L), list_to_existing_atom(L)).

%%
%%' xmerl:simple_export/2 API
%%

-spec '#xml-inheritance#'() -> list().
'#xml-inheritance#'() -> [].

%% The '#root#' tag is called when the entire structure has been
%% exported. It does not appear in the structure itself.
-spec '#root#'(any(), any(), any(), any()) -> docsh_internal:t().
'#root#'([#xmlElement{name = module} = Module], _, _, _) ->
    #{name => get_module_name(Module),
      description => get_module_description(Module),
      items => get_functions(Module) ++ get_types(Module)}.

-spec '#element#'(any(), any(), any(), any(), any()) -> any().
'#element#'(_, _, _, _, E) -> E.

%% The '#text#' function is called for every text segment.
-spec '#text#'(any()) -> any().
'#text#'(Text) -> ?il2b(Text).

%%.
%%' xmerl:simple_export/2 helpers
%%

get_module_name(#xmlElement{attributes = Attrs}) ->
    ?l2ea('find_attribute!'(name, Attrs)).

get_module_description(#xmlElement{name = module} = M) ->
    get_description(M).

-spec get_functions(#xmlElement{}) -> [docsh_internal:item()].
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

-spec get_types(#xmlElement{}) -> [docsh_internal:item()].
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
    get_content(description, none, fun get_full_description/1, Element).

get_full_description(#xmlElement{name = description} = D) ->
    get_content(fullDescription, none, fun get_full_description/1, D);
get_full_description(#xmlElement{name = fullDescription, content = XmlElementContent}) ->
    %% See xmerl.hrl for the definition of #xmlElement.content:
    %%   content = [#xmlElement()|#xmlText()|#xmlPI()|#xmlComment()|#xmlDecl()]
    format_text(XmlElementContent).

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

%%.
%%' EDoc formatter
%%

-spec format_content(xml_element_content(), any()) -> iolist().
format_content(Content, Ctx) ->
    [ format_content_(C, Ctx) || C <- Content ].

format_content_(#xmlPI{}, _Ctx)      -> [];
format_content_(#xmlComment{}, _Ctx) -> [];
format_content_(#xmlDecl{}, _Ctx)    -> [];

format_content_(#xmlText{} = T, Ctx) ->
    Text = T#xmlText.value,
    case edoc_lib:is_space(Text) of
        true -> [];
        false ->
            case is_preformatted_text(T) of
                true  -> cleanup_preformatted_text(Text, Ctx);
                false -> cleanup_text(Text, Ctx)
            end
    end;

%% Useful for structure preview
%format_content_(#xmlElement{name = Name, content = Content}) ->
%    ["\n", io_lib:format("~s", [Name]), "> ",  format_content(Content)].

format_content_(#xmlElement{name = Name, content = Content} = E, Ctx) ->
    case layout_type(Name) of
        header -> format_header(E, Ctx);
        inline -> format_content(Content, Ctx);
        block  -> format_block_element(E, format_content(Content, Ctx))
    end.

format_header(#xmlElement{name = Name, parents = Parents, content = Content} = E, Ctx) ->
    has_non_header_parents(E) andalso erlang:error({non_header_parents, Parents}, [E]),
    Headers = #{h1 => "# ",
                h2 => "## ",
                h3 => "### ",
                h4 => "#### ",
                h5 => "##### ",
                h6 => "###### "},
    case Name of
        hgroup -> [];
        _ -> ["\n", maps:get(Name, Headers), format_content(Content, Ctx), "\n"]
    end.

has_non_header_parents(#xmlElement{parents = Parents}) ->
    lists:any(fun
                  ({hgroup, _}) -> false;
                  (_) -> true
              end, Parents).

format_block_element(#xmlElement{name = Name}, Formatted) ->
    ["\n", io_lib:format("~s", [Name]), "> ",  Formatted].

layout_type(_) -> inline.

cleanup_text(Text, _Ctx) ->
    %% split on newlines, discard empty results, end each line with a single newline character
    [ [Line, "\n"] || [_|_] = Line <- re:split(Text, "\s*\n\s*", [trim, {return, list}]) ].

cleanup_preformatted_text(Text, _Ctx) ->
    Text.

is_preformatted_text(#xmlText{parents = Parents}) ->
    lists:any(fun
                  ({pre, _}) -> true;
                  (_) -> false
              end, Parents).

%%. vim: foldmethod=marker foldmarker=%%',%%.

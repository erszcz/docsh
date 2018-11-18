-module(docsh_edoc_xmerl).

%% xmerl:simple_export/2 API
-export(['#root#'/4,
         '#element#'/5,
         '#text#'/1,
         '#xml-inheritance#'/0]).

%% EDoc formatter
-export([format_edoc/2]).

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

-spec format_edoc(xml_element_content(), any()) -> iolist().
format_edoc(Content, Ctx) ->
    lists:map(fun
                  ({br})        -> "\n";
                  ({l, Line})   -> [Line, "\n"];
                  ({i, Inline}) -> [Inline]
                  %({l, Line})   -> ["<l>", Line, "</l>\n"];
                  %({i, Inline}) -> ["<i>", Inline, "</i>"]
              end, format_content(Content, Ctx)).

format_content(Content, Ctx) ->
    lists:flatten([ format_content_(C, Ctx) || C <- Content ]).

format_content_(#xmlPI{}, _Ctx)      -> [];
format_content_(#xmlComment{}, _Ctx) -> [];
format_content_(#xmlDecl{}, _Ctx)    -> [];

format_content_(#xmlText{} = T, Ctx) ->
    Text = T#xmlText.value,
    case edoc_lib:is_space(Text) of
        true -> [];
        false ->
            case is_preformatted(T#xmlText.parents) of
                true  -> cleanup_preformatted_text(Text, Ctx);
                false -> cleanup_text(Text, Ctx)
            end
    end;

format_content_(#xmlElement{} = E, Ctx) ->
    format_element(E, Ctx).

format_element(#xmlElement{name = Name, content = Content} = E, Ctx) ->
    case layout_type(Name) of
        header -> format_header(E, Ctx);
        %inline -> format_inline_element(E, Ctx);
        %block  -> format_block_element(E, Ctx)
        _ -> format_element_(Name, E, format_content(Content, Ctx), Ctx)
    end.

format_element_(code, #xmlElement{} = E, Lines, _Ctx) ->
    case is_preformatted(E#xmlElement.parents) of
        true  -> Lines;
        false -> [ Symbol || Symbol <- Lines, Symbol /= {br} ]
    end;
format_element_(dl, #xmlElement{}, Lines, Ctx) ->
    Lines;
format_element_(dt, #xmlElement{name = Name} = E, Lines, Ctx) ->
    Lines;
format_element_(dd, #xmlElement{name = Name} = E, Lines, Ctx) ->
    Lines;
format_element_(_, #xmlElement{name = Name} = E, Lines, Ctx) ->
    Lines.

format_inline_element(#xmlElement{name = Name, pos = Pos, content = Content}, Ctx) ->
    %% Structure preview
    %Prefix = io_lib:format("~s ~p> ", [Name, Pos]),
    %WSSpan = [ " " || _ <- lists:seq(1, length(Prefix) + 1) ],
    %[{l, First} | Rest] = format_content(Content, Ctx),
    %[{l, [Prefix, First]} | [ {l, [WSSpan, L]} || {l, L} <- Rest ]].
    %%
    %Lines = format_content(Content, Ctx),
    %merge_lines(Lines, Ctx).
    %%
    format_content(Content, Ctx).

merge_lines(Lines, Ctx) ->
    W = maps:get(width, Ctx, default_width()),
    #{lines := NewLines, current := {_, Current}} =
        lists:foldl(fun (L, Acc) -> merge_lines_(W, L, Acc) end,
                    #{lines => [], current => {0, []}}, Lines),
    lists:reverse([{l, lists:flatten(Current)} | NewLines]).

merge_lines_(W, {l, L}, #{lines := Lines, current := {Len, Current}} = Acc) ->
    NewLen = Len + length(L),
    if
        NewLen =< W -> Acc#{current := {NewLen, [Current, L]}};
        NewLen  > W -> Acc#{lines := append_line(Current, Lines),
                            current := {length(L), [L]}}
    end.

append_line(Current, Lines) ->
    [{l, lists:flatten(Current)} | Lines].

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
        _ ->
            [{l, Text}] = format_content(Content, Ctx),
            [{l, []},
             {l, [maps:get(Name, Headers), Text]}]
    end.

has_non_header_parents(#xmlElement{parents = Parents}) ->
    %% 5 is the level from which function descriptions are extracted in an EDoc document.
    Drop = 5,
    lists:any(fun
                  ({hgroup, _}) -> false;
                  (_) -> true
              end, lists:nthtail(Drop, Parents)).

format_block_element(#xmlElement{content = Content}, Ctx) ->
    format_content(Content, Ctx).

layout_type(Tag) ->
    case Tag of
        hgroup -> header;
        h1     -> header;
        h2     -> header;
        h3     -> header;
        h4     -> header;
        h5     -> header;
        h6     -> header;
        dl     -> block;
        ol     -> block;
        ul     -> block;
        _      -> inline
    end.

cleanup_text(Text, _Ctx) ->
    lists:flatmap(fun
                      ("\n") -> [{br}];
                      ([]) -> [];
                      (T) -> [{i, string:trim(T, leading)}]
                  end,
                  re:split(Text, "(\n)", [notempty, trim, {return, list}])).

cleanup_preformatted_text(Text, _Ctx) ->
    [ {l, Line} || Line <- string:tokens(Text, "\n") ].

is_preformatted(Parents) ->
    lists:any(fun
                  ({pre, _}) -> true;
                  (_) -> false
              end, Parents).

default_width() -> 80.

%%. vim: foldmethod=marker foldmarker=%%',%%.

-module(docsh_syntax).

-behaviour(docsh_reader).
-export([available/1,
         to_internal/1]).

-define(il2b(IOList), iolist_to_binary(IOList)).
-define(l(Args), fun () -> Args end).

-spec available(docsh_beam:t()) -> [docsh_reader:t()].
available(Beam) ->
    [ ?MODULE || docsh_beam:abstract_code(Beam) /= false ].

-spec to_internal(docsh_beam:t()) -> R when
      R :: {ok, docsh_internal:t()}
         | {error, any(), [erlang:stack_item()]}.
to_internal(Beam) ->
    try
        Forms = case {docsh_beam:abstract_code(Beam), docsh_beam:source_file(Beam)} of
                    {false, false} ->
                        erlang:error(no_debug_info_no_src, [Beam]);
                    {Abst, false} ->
                        Abst;
                    {_, Source} ->
                        {ok, Fs} = epp:parse_file(Source, []),
                        Fs
                end,
        Internal = #{name => get_module_name(Forms),
                     items => get_functions(Forms) ++ get_types(Forms)},
        {ok, Internal}
    catch
        _:R -> {error, R, erlang:get_stacktrace()}
    end.

get_module_name(Forms) ->
    case lists:keyfind(module, 3, Forms) of
        false -> erlang:error({not_found, module, Forms}, [Forms]);
        {_, _, module, Mod} -> Mod
    end.

-spec get_functions([erl_parse:abstract_form()]) -> [docsh_internal:item()].
get_functions(Forms) ->
    [ function(Spec) || {attribute, _, spec, _} = Spec <- Forms ].

function({attribute, _, spec, _} = Spec) ->
    #{kind      => 'function',
      name      => function_name(Spec),
      arity     => function_arity(Spec),
      signature => function_signature(Spec)}.

function_name({attribute, _, spec, Data}) ->
    {{Name, _Arity}, _} = Data,
    Name.

function_arity({attribute, _, spec, Data}) ->
    {{_Name, Arity}, _} = Data,
    Arity.

function_signature({attribute, _, spec, _} = Spec) ->
    ?il2b(format(Spec)).

-spec get_types([erl_parse:abstract_form()]) -> [docsh_internal:item()].
get_types(Forms) ->
    [ type(Type) || {attribute, _, type, _} = Type <- Forms ].

type({attribute, _, type, _} = Type) ->
    #{kind      => 'type',
      name      => type_name(Type),
      arity     => type_arity(Type),
      signature => type_signature(Type)}.

type_name({attribute, _, type, Data}) ->
    {Name, _, _Args} = Data,
    Name.

type_arity({attribute, _, type, Data}) ->
    {_Name, _, Args} = Data,
    length(Args).

type_signature({attribute, _, type, _} = Type) ->
    ?il2b(format(Type)).

-ifdef(erl_prettypr_no_specs).

format(Attr) ->
    erl_pp:form(Attr).

-else.

format(Attr) ->
    [erl_prettypr:format(Attr, [{ribbon, 80}]), $\n].

-endif.

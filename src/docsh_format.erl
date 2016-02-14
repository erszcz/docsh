-module(docsh_format).

-export([type/1, type/2,
         type_attr/1, type_attr/2]).

-import(erl_types_prettypr_fmt, [text/1]).

-define(a2l(A), atom_to_list(A)).

-type format_opt() :: {format, 'flat' | 'pretty'}.

-spec type_attr(Attr) -> iodata() when
      Attr :: erl_parse:abstract_form().
type_attr(Attr) ->
    type_attr(Attr, [{format, pretty}]).

-spec type_attr(Attr, [Opt]) -> iodata() when
      Attr :: erl_parse:abstract_form(),
      Opt :: format_opt().
type_attr({attribute, _Loc, type, Data}, Opts) ->
    {TypeName, Form, TypeArgs} = Data,
    Doc = type(Form, lists:keystore(return, 1, Opts, {return, prettypr})),
    NamedType = named_type(TypeName, TypeArgs, Doc),
    io_lib:format("~s", [prettypr:format(NamedType)]).

named_type(Name, Args, Doc) ->
    Text = "-type " ++ ?a2l(Name) ++ named_type_args(Args) ++ " ::",
    Prefix = text(Text),
    prettypr:follow(Prefix, Doc, length(Text)).

named_type_args(Args) when length(Args) == 0 -> "()";
named_type_args(Args) when length(Args) >  0 -> "(...)".

-spec type(Type) -> iodata() when
      Type :: erl_parse:abstract_form().
type(Form) ->
    type(Form, [{format, pretty}]).

-spec type(Type, [Opt]) -> iodata() when
      Type :: erl_parse:abstract_form(),
      Opt :: format_opt().
type(Form, Opts) ->
    ExpTypes = sets:new(),
    Site = {type, {m, t, 0}},
    RecDict = dict:new(),
    %% TODO: all the arguments are stubs - figure out what would make more sense
    CRecord = erl_types_fork:t_from_form(Form, ExpTypes, Site, RecDict),
    debug('type:c_record', CRecord),
    Doc = erl_types_fork:t_to_string(format(Opts), CRecord, RecDict),
    debug('type:prettypr', Doc),
    debug('type', case proplists:get_value(return, Opts, iodata) of
                      prettypr -> Doc;
                      iodata ->
                          io_lib:format("~s", [prettypr:format(Doc)])
                  end).

debug(Tag, Content) ->
    docsh_lib:debug(Tag, "~s: ~p~n", [Tag, Content]),
    Content.

format(Opts) ->
    case docsh_lib:get(format, Opts) of
        flat -> erl_types_flat_fmt;
        pretty -> erl_types_prettypr_fmt
    end.

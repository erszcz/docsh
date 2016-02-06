-module(docsh_prettypr).

-export([type/1,
         type_attr/1]).

-spec type_attr(Attr) -> iodata() when
      Attr :: erl_parse:abstract_form().
type_attr({attribute, _Loc, type, Data}) ->
    {_TypeName, Form, _} = Data,
    type(Form).

-spec type(Type) -> iodata() when
      Type :: erl_parse:abstract_form().
type(Form) ->
    ExpTypes = sets:new(),
    Site = {type, {m, t, 0}},
    RecDict = dict:new(),
    CRecord = erl_types:t_from_form(Form, ExpTypes, Site, RecDict),
    erl_types:t_to_string(CRecord).

debug(Tag, Content) ->
    docsh_lib:debug(Tag, "~s: ~p~n", [Tag, Content]),
    Content.

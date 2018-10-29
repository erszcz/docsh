-module(docsh_internal).

-export([grouped/1,
         merge/1]).

-type t() :: flat() | grouped().

%% Internal documentation format used for merging module info
%% fetched from different sources (edoc, debug info, etc).
%% This format is flat hence its suitability for merging.
-type flat() :: [{atom(), any()}].

%% This format contains all info about an entitiy under a `{name(), arity()}' key.
%% Entity kind has to be inferred from the entry's content.
-type grouped() :: #{{name(), arity()} => any()}.

-type name() :: atom().

-define(a2b(A), atom_to_binary(A, utf8)).
-define(a2l(A), atom_to_list(A)).
-define(i2b(I), integer_to_binary(I)).
-define(il2b(IOList), iolist_to_binary(IOList)).

%%
%%' Public
%%

-spec merge([Info]) -> MergedInfo when
      Info :: t(),
      MergedInfo :: t().
merge([]) -> [];
merge([Info]) -> Info;
merge([Info1, Info2 | Rest]) ->
    case are_disjoint(Info1, Info2) of
        false -> erlang:error(not_disjoint, [Info1, Info2 | Rest]);
        true ->
            (module(Info1) =:= module(Info2)
             orelse erlang:error(different_modules, [Info1, Info2 | Rest])),
            merge([merge2(Info1, Info2) | Rest])
    end.

-spec grouped(t()) -> t().
grouped(#{} = Grouped) -> Grouped;
grouped([_|_] = Internal) ->
    Dict = docsh_lib:group_by(fun item_name_and_arity/1, Internal),
    maps:from_list(dict:to_list(Dict)).

%%.
%%' Internal
%%

merge2(#{items := Items1} = Info1, #{items := Items2}) ->
    %% TODO: this might discard valuable module info from Info2
    Info1#{items := Items1 ++ Items2}.

are_disjoint(Info1, Info2) ->
    Module = maps:get(name, Info1),
    Module = maps:get(name, Info2),
    #{items := {Items1, _}} = Info1,
    #{items := {Items2, _}} = Info2,
    Items1 -- Items2 == Items1.

module(Info) ->
    {_, Props} = lists:keyfind(module, 1, Info),
    {_, Mod} = lists:keyfind(name, 1, Props),
    Mod.

item_name_and_arity({module, _Info})            -> {module, 0};
item_name_and_arity({{type, NameArity}, _})     -> NameArity;
item_name_and_arity({{spec, NameArity}, _})     -> NameArity;
item_name_and_arity({{function, NameArity}, _}) -> NameArity.

%%. vim: foldmethod=marker foldmarker=%%',%%.

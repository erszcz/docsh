-module(docsh_internal).

-export([merge/1]).

%% Internal documentation format used for merging module info
%% fetched from different sources (edoc, debug info, etc).
%% This format is flat hence its suitability for merging.
-type t() :: [{atom(), any()}].

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
        false -> error(not_disjoint, [Info1, Info2 | Rest]);
        true ->
            (module(Info1) =:= module(Info2)
             orelse error(different_modules, [Info1, Info2 | Rest])),
            merge([merge2(Info1, Info2) | Rest])
    end.

%%.
%%' Internal
%%

merge2(Info1, Info2) ->
    %% TODO: this might discard valuable module info from Info2
    Info1 ++ lists:keydelete(module, 1, Info2).

are_disjoint(Info1, Info2) ->
    Keys1 = proplists:get_keys(Info1) -- [module],
    Keys2 = proplists:get_keys(Info2) -- [module],
    Keys1 -- Keys2 == Keys1.

module(Info) ->
    {_, Props} = lists:keyfind(module, 1, Info),
    {_, Mod} = lists:keyfind(name, 1, Props),
    Mod.

%%. vim: foldmethod=marker foldmarker=%%',%%.

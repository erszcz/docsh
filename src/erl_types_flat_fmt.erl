-module(erl_types_flat_fmt).

-compile([export_all]).

comma_sequence(Types, RecDict) ->
    List = [case T =:= any of
                true -> text("_");
                false -> erl_types:t_to_string(?MODULE, T, RecDict)
            end || T <- Types],
    join(List, text(",")).

union_sequence(Types, RecDict) ->
    List = [erl_types:t_to_string(?MODULE, T, RecDict) || T <- Types],
    join(List, text(" | ")).

flat_format(F, S) ->
    lists:flatten(io_lib:format(F, S)).

join(List, Sep) -> string:join(List, Sep).

text(T) -> T.

concat(A, B) -> A ++ B.
concat(A, B, C) -> A ++ B ++ C.
concat(A, B, C, D) -> A ++ B ++ C ++ D.

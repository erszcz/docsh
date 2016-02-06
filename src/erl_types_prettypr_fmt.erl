-module(erl_types_prettypr_fmt).

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
    text(lists:flatten(io_lib:format(F, S))).

join([], _Sep) -> prettypr:empty();
join([H], _Sep) -> H;
join([H|T], Sep) -> prettypr:sep([prettypr:beside(H, text(Sep)), join(T, Sep)]).

text({text, _} = T) -> T;
text([]) -> prettypr:empty();
text(T) when is_list(T) ->
    case is_string(T) of
        true -> prettypr:text(T);
        false -> error(badarg, [T])
    end.

is_string(String) -> eunit_lib:is_string(String).

concat(A, B) -> prettypr:beside(A, B).
concat(A, B, C) -> prettypr:beside(A, prettypr:beside(B, C)).
concat(A, B, C, D) -> prettypr:beside(A, prettypr:beside(B, prettypr:beside(C, D))).

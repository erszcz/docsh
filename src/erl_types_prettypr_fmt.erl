-module(erl_types_prettypr_fmt).

-compile([export_all]).

comma_sequence(Types, RecDict) ->
    List = [case T =:= any of
                true -> text("_");
                false -> erl_types_fork:t_to_string(?MODULE, T, RecDict)
            end || T <- Types],
    join(List, text(","), trailing).

union_sequence(Types, RecDict) ->
    List = [erl_types_fork:t_to_string(?MODULE, T, RecDict) || T <- Types],
    join(List, text("|"), leading).

flat_format(F, S) ->
    text(lists:flatten(io_lib:format(F, S))).

join(List, Sep) -> join_t(List, Sep).

join(List, Sep, SepPosition) ->
    case SepPosition of
        trailing -> join_t(List, Sep);
        leading  -> join_l(List, Sep)
    end.

join_t([], _Sep) -> prettypr:empty();
join_t([H], _Sep) -> H;
join_t([H|T], Sep) -> prettypr:sep([prettypr:beside(H, Sep), join_t(T, Sep)]).

join_l(List, Sep) ->
    {_, {beside, {sep,[_, Text], X, Y}, Rest}} = lists:foldr(fun do_join_l/2,
                                                          {text(Sep), prettypr:empty()}, List),
    Separated = {beside, Text, Rest},
    docsh_lib:debug(join_l, "join_l: ~p~n", [Separated]),
    Separated.

do_join_l(E, {Sep, Acc}) ->
    {Sep, prettypr:follow(Sep, prettypr:follow(E, Acc, 0), 0)}.

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

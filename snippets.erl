P = "/home/erszcz/apps/elixir/1.2.1/bin/../lib/elixir/ebin/Elixir.Agent.beam".
{ok, {_, C}} = beam_lib:chunks(P, ["ExDc"]).
[{_, B}] = C.
Docs = binary_to_term(B).
{_ModLineNo, ModDoc} = proplists:get_value(moduledoc, element(2, Docs)).
FunDocs = proplists:get_value(docs, element(2, Docs)).
{{update,3}, _FunLineNo, _Type, _Something, FunDoc} = lists:keyfind({update,3}, 1, FunDocs).
ModDoc.
FunDoc.

S3 = "/home/erszcz/work/lavrin/docsh/test/edoc_example.erl".
docsh_edoc:to_internal(S3).

S3 = "/home/erszcz/work/lavrin/docsh/test/edoc_example.erl".
{ok, BData} = file:read_file(S3).
{ok, Tokens, _} = erl_scan:string(binary_to_list(BData)).
%erl_parse:parse_exprs(Tokens).

%erl_scan:string(Str)
%Dot = {dot, erl_anno:new(1)},
%erl_parse:parse_term(Tokens ++ [Dot])

Spec = {attribute,13,spec,
        {{f,0},
         [{type,13,'fun',
           [{type,13,product,[]},{user_type,13,r,[]}]}]}}.
iolist_to_binary(erl_pp:form(Spec)).
% <<"-spec f() -> r().\n">>

f().
Spec = {attribute,17,spec,
        {{g,0},
         [{type,17,bounded_fun,
           [{type,17,'fun',[{type,17,product,[]},{var,17,'R'}]},
            [{type,18,constraint,
              [{atom,18,is_subtype},
               [{var,18,'R'},{user_type,18,r,[]}]]}]]}]}}.
iolist_to_binary(erl_pp:form(Spec)).
% <<"-spec g() -> R when is_subtype(R, r()).\n">>

RTBeam = code:where_is_file("docsh_rt.beam"),
beam_lib:chunks(RTBeam, ["Abst"]),
{ok, {_, [{"Abst", Abst}]}} = beam_lib:chunks(RTBeam, ["Abst"]),
io:format("~p~n", [binary_to_term(Abst)]).

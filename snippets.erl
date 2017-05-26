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

%% Get abstract code from a .beam file.
RTBeam = code:where_is_file("docsh_rt.beam"),
beam_lib:chunks(RTBeam, ["Abst"]),
{ok, {_, [{"Abst", Abst}]}} = beam_lib:chunks(RTBeam, ["Abst"]),
io:format("~p~n", [binary_to_term(Abst)]).

%% Compile file with debug_info / inlining / to Core Erlang and get the result.
compile:file("test/edoc_example.erl", [debug_info]).
compile:file("test/edoc_example.erl", [inline]).
{ok, Mod, Core} = compile:file("test/edoc_example.erl", [binary, to_core]).

%% Compile Core Erlang to .beam.
{ok, [], BEAM} = compile:forms(Core, [from_core]).

%% Print Core Erlang.
io:format("~s~n", [core_pp:format(Core)]).

%% Compound attribute
CA = {attribute,98,type, {proc_attrs, {type,98,tuple, [{type,98,pid,[]}, {ann_type,99,[{var,99,'Attr'},{var,99,'_'}]}, {type,100,nonempty_list, [{ann_type,100, [{var,100,'Name'}, {type,100,union, [{type,100,atom,[]}, {type,101,tuple,[{atom,101,current_function},{type,101,mfa,[]}]}, {type,102,tuple, [{atom,102,initial_call},{type,102,mfa,[]}]}]}]}]}]}, []}}.
Pid = {type,98,pid,[]}.
erl_types:from_form({type,98,pid,[]}).
erl_types:t_from_form({type,98,pid,[]}).
erl_types:t_from_form({type,98,pid,[]}, [], [], []).
erl_types:t_from_form({type,98,pid,[]}, sets:empty(), [], []).
erl_types:t_from_form({type,98,pid,[]}, sets:new(), [], []).
erl_types:t_from_form({type,98,pid,[]}, sets:new(), {type, {m, t, 0}}, dict:new()).
CPid = erl_types:t_from_form({type,98,pid,[]}, sets:new(), {type, {m, t, 0}}, dict:new()).
erl_types:t_to_string(CPid).
CAtom = erl_types:t_from_form({type,100,atom,[]}, sets:new(), {type, {m, t, 0}}, dict:new()).
erl_types:t_to_string(CAtom).
erl_types:t_from_form({type,101,mfa,[]}, sets:new(), {type, {m, t, 0}}, dict:new()).
CMFA = erl_types:t_from_form({type,101,mfa,[]}, sets:new(), {type, {m, t, 0}}, dict:new()).
erl_types:t_to_string(CMFA).

%% Using prettypr.
f(PParts),
PParts = [ prettypr:text("| " ++ S)
           || S1 <- string:tokens("int() | string() | f() | non_neg_integer() | asd() | qwe() | zxc() | some_other_type_with_a_tremendously_long_name()", "|"),
              S <- [string:strip(S1)] ].
io:format(prettypr:format(prettypr:par([prettypr:text("-type a() :: ")] ++ PParts, 11))).

CList = {c,list,
         [{c,union,
           [{c,atom,any,unknown},
            none,none,none,none,none,
            {c,tuple_set,
             [{2,
               [{c,tuple,
                 [{c,atom,[current_function],unknown},
                  {c,tuple,
                   [{c,atom,any,unknown},
                    {c,atom,any,unknown},
                    {c,number,{int_rng,0,255},integer}],
                   {3,any}}],
                 {2,{c,atom,[current_function],unknown}}},
                {c,tuple,
                 [{c,atom,[initial_call],unknown},
                  {c,tuple,
                   [{c,atom,any,unknown},
                    {c,atom,any,unknown},
                    {c,number,{int_rng,0,255},integer}],
                   {3,any}}],
                 {2,{c,atom,[initial_call],unknown}}}]}],
             unknown},
            none,none,none,none],
           unknown},
          {c,nil,[],unknown}],
         nonempty}.
CTuple = {c,tuple,
          [{c,identifier,[pid],unknown},
           any,
           {c,list,
            [{c,union,
              [{c,atom,any,unknown},
               none,none,none,none,none,
               {c,tuple_set,
                [{2,
                  [{c,tuple,
                    [{c,atom,[current_function],unknown},
                     {c,tuple,
                      [{c,atom,any,unknown},
                       {c,atom,any,unknown},
                       {c,number,{int_rng,0,255},integer}],
                      {3,any}}],
                    {2,{c,atom,[current_function],unknown}}},
                   {c,tuple,
                    [{c,atom,[initial_call],unknown},
                     {c,tuple,
                      [{c,atom,any,unknown},
                       {c,atom,any,unknown},
                       {c,number,{int_rng,0,255},integer}],
                      {3,any}}],
                    {2,{c,atom,[initial_call],unknown}}}]}],
                unknown},
               none,none,none,none],
              unknown},
             {c,nil,[],unknown}],
            nonempty}],
          {3,any}}.
RecDict = dict:new().
prettypr:format(erl_types:t_to_string(erl_types_prettypr_fmt, CList, RecDict)).

ExpTypes = sets:new(), Site = {type, {m, t, 0}}, RecDict = dict:new().
{attribute, _, type, {proc_attrs, Inner, []}} = CA.
erl_types:t_from_form(Inner, ExpTypes, Site, RecDict).
io:format(docsh_prettypr:pretty_type(Inner)).

dbg:tracer().
dbg:p(all, [call, arity]).
DMods = [ct_docsh,
         docsh,
         docsh_beam,
         docsh_edoc,
         docsh_edoc_xmerl,
         docsh_edoc_xmerl_flat,
         docsh_elixir_docs_v1,
         docsh_embeddable,
         docsh_internal,
         docsh_lib,
         docsh_reader,
         docsh_shell,
         docsh_syntax,
         docsh_writer,
         rebar3_docsh,
         rebar3_docsh_prv].
[ dbg:tpl(M, []) || M <- DMods ].

code:add_path("/Users/erszcz/work/erszcz/tracer").
my_tracer:start().
dbg:p(all, [call, timestamp]).
dbg:tpl(docsh_shell, unchecked_lookup, x).
dbg:tpl(docsh_shell, get_beam, 1, x).

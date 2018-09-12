-module(syntax_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").
-include("proplists_eq.hrl").

-define(eq(Expected, Actual), ?proplists_eq(Expected, Actual)).
-define(TESTED, docsh_syntax).

all() ->
    [syntax_to_internal].

syntax_to_internal(_) ->
    {ok, DBeam} = docsh_beam:from_loaded_module(edoc_example),
    ct:pal("~p", [DBeam]),
    ?eq([{module, [{name, edoc_example}]},
         {{spec, {f,0}}, {description, <<"-spec f() -> r().\n">>}},
         {{type, {l,0}}, {description, <<"-type l() :: list().\n">>}},
         {{type, {l,1}}, {description, <<"-type l(A) :: [A].\n">>}},
         {{type, {r,0}}, {description, <<"-type r() :: ok.\n">>}}],
        unwrap(?TESTED:to_internal(DBeam))).

source_file(Mod) ->
    proplists:get_value(source, Mod:module_info(compile)).

unwrap({ok, V}) -> V;
unwrap(Else) -> erlang:error(not_ok, [Else]).

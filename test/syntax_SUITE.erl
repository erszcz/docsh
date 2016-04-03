-module(syntax_SUITE).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("proplists_eq.hrl").

-define(eq(Expected, Actual), ?proplists_eq(Expected, Actual)).
-define(TESTED, docsh_syntax).

all() ->
    [syntax_to_internal].

syntax_to_internal(_) ->
    File = source_file(edoc_example),
    ct:pal("~p", [File]),
    ?eq([{module, [{name, edoc_example}]},
         {{spec, {f,0}}, {description, <<"-spec f() -> r().\n">>}},
         {{type, {l,0}}, {description, <<"-type l() :: list().\n">>}},
         {{type, {l,1}}, {description, <<"-type l(A) :: [A].\n">>}},
         {{type, {r,0}}, {description, <<"-type r() :: ok.\n">>}}],
        unwrap(?TESTED:to_internal(File))).

source_file(Mod) ->
    proplists:get_value(source, Mod:module_info(compile)).

unwrap({ok, V}) -> V;
unwrap(Else) -> error(not_ok, [Else]).

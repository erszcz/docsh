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
         {{type, {l,1}}, {description, <<"[A]\n">>}},
         {{type, {l,0}}, {description, <<"[any()]\n">>}},
         {{type, {r,0}}, {description, <<"'ok'\n">>}}],
        ?TESTED:to_internal(File)).

source_file(Mod) ->
    proplists:get_value(source, Mod:module_info(compile)).

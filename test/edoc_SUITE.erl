-module(edoc_SUITE).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("proplists_eq.hrl").

-define(eq(Expected, Actual), ?proplists_eq(Expected, Actual)).
-define(TESTED, docsh_edoc).
-define(EXAMPLE, edoc_example).

all() ->
    [edoc_to_internal].

edoc_to_internal(_) ->
    File = source_file(?EXAMPLE),
    ct:pal("~p", [File]),
    ?eq([{module, [{name, edoc_example},
                   {description, <<"Top-level module doc.">>}]},
         {function, [{name, f},
                     {arity, 0},
                     {exported, true},
                     {label, <<"f-0">>},
                     {description, <<"Doc for f/0.">>}]}],
        ?TESTED:to_internal(File)).

source_file(Mod) ->
    proplists:get_value(source, Mod:module_info(compile)).

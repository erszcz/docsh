-module(edoc_SUITE).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("proplists_eq.hrl").

-define(eq(Expected, Actual), ?proplists_eq(Expected, Actual)).
-define(TESTED, docsh_edoc).

all() ->
    [edoc_to_internal,
     edoc_format,
     edoc_format_pre].

edoc_to_internal(_) ->
    File = source_file(edoc_example),
    ct:pal("~p", [File]),
    ?eq([{module, [{name, edoc_example},
                   {description, <<"Top-level module doc.">>}]},
         {function, [{name, f},
                     {arity, 0},
                     {exported, true},
                     {label, <<"f-0">>},
                     {description, <<"Doc for f/0.">>}]}],
        ?TESTED:to_internal(File)).

edoc_format(_) ->
    File = source_file(edoc_example2),
    D = function_description({g,0}, ?TESTED:to_internal(File)),
    ct:pal("~s", [D]),
    ?eq([<<"g() returns a more complex value,\n"
           "while its documentation uses more complex markup.\n"
           "\n"
           "  Why?\n"
           "  To test the documentation extraction and formatting process.\n"
           "\n"
           "  Any other reason?\n"
           "  Not really.\n"
           "\n"
           "  1. Some\n"
           "  2. Random\n"
           "  3. Items\n"
           "\n"
           "  - One\n"
           "  - Two\n"
           "  - Three:\n"
           "      - a\n"
           "      - b\n"
           "      - c\n">>],
        [D]).

edoc_format_pre(_) ->
    File = source_file(edoc_example2),
    D = function_description({pre,0}, ?TESTED:to_internal(File)),
    ct:pal("~s", [D]),
    ?eq([<<"  pre\n"
           "    formatted\n"
           "      text\n">>],
        [D]).

function_description(F, Docs) ->
    %% TODO: grrr... the internal format sucks
    Functions = proplists:get_all_values(function, Docs),
    {F,_,_,Description} = lists:keyfind(F, 1, Functions),
    Description.

source_file(Mod) ->
    proplists:get_value(source, Mod:module_info(compile)).

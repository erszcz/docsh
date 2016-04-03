-module(edoc_SUITE).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("proplists_eq.hrl").

-define(eq(Expected, Actual), ?proplists_eq(Expected, Actual)).
-define(TESTED, docsh_edoc).

all() ->
    [edoc_to_internal,
     edoc_format,
     edoc_format_code_in_loose_text,
     edoc_format_dl,
     edoc_format_p,
     edoc_format_pre,
     edoc_format_text1,
     edoc_format_text2,
     edoc_format_ul].

edoc_to_internal(_) ->
    File = source_file(edoc_example),
    ct:pal("~p", [File]),
    ?eq([{module, [{name, edoc_example},
                   {description, <<"Top-level module doc.\n">>}]},
         {{function, {f,0}}, {{name, f},
                              {arity, 0},
                              {exported, true},
                              {label, <<"f-0">>},
                              {description, <<"Doc for f/0.\n">>}}}],
        unwrap(?TESTED:to_internal(File))).

edoc_format(_) ->
    File = source_file(edoc_example2),
    D = function_description({g,0}, unwrap(?TESTED:to_internal(File))),
    ct:pal("~s", [D]),
    ?eq([<<"g() returns a more complex value,\n"
           "while its documentation uses more complex markup.\n"
           "\n"
           "  Why?\n"
           "\n"
           "      To test the documentation extraction and formatting process.\n"
           "\n"
           "  Any other reason?\n"
           "\n"
           "      Not really.\n"
           "\n"
           "\n"
           "  - Some\n"
           "  - Random\n"
           "  - Items\n"
           "\n"
           "  1. One\n"
           "  2. Two\n"
           "  3. Three:\n"
           "      - a\n"
           "      - b\n"
           "      - c\n">>],
        [D]).

edoc_format_code_in_loose_text(C) ->
    edoc_format(C, code_in_loose_text, <<"Fetch the internal state of an OTP process.\n"
                                         "Calls sys:get_state/2 directly in R16B01+, and fetches\n"
                                         "it dynamically on older versions of OTP.\n">>).

edoc_format_dl(C) ->
    edoc_format(C, dl, <<"  sys_alloc\n"
                         "\n"
                         "      System allocator, usually just malloc\n"
                         "\n"
                         "  mseg_alloc\n"
                         "\n"
                         "      Used by other allocators, can do mmap. Caches allocations\n"
                         "\n">>).

edoc_format_p(C) ->
    edoc_format(C, p, <<"Just\n"
                        "a paragraph.\n">>).

edoc_format_pre(C) ->
    edoc_format(C, pre, <<"    pre\n"
                          "      formatted\n"
                          "        text\n">>).

edoc_format_text1(C) ->
    edoc_format(C, text1, <<"Some\n"
                            "loose text,\n"
                            "not a paragraph.\n">>).

edoc_format_text2(C) ->
    edoc_format(C, text2, <<"Some\n"
                            "loose text,\n"
                            "not a paragraph.\n"
                            "\n"
                            "A paragraph.\n">>).

edoc_format_ul(C) ->
    edoc_format(C, ul, <<"  - Module is any atom representing a module\n"
                         "  - Function is any atom representing a function, or the wildcard\n"
                         "    '_'\n"
                         "  - Args is either the arity of a function (0..255), a wildcard\n"
                         "    pattern ('_'), a\n"
                         "    match specification,\n"
                         "    or a function from a shell session that can be transformed into\n"
                         "    a match specification\n">>).

edoc_format(_, Element, Expected) ->
    File = source_file(edoc_example2),
    D = function_description({Element, 0}, unwrap(?TESTED:to_internal(File))),
    ct:pal("~s", [D]),
    ?eq([Expected], [D]).

function_description(F, Docs) ->
    {{function, F},
     {_,_,_,_,{description, D}}} = lists:keyfind({function, F}, 1, Docs),
    D.

source_file(Mod) ->
    proplists:get_value(source, Mod:module_info(compile)).

unwrap({ok, V}) -> V;
unwrap(Else) -> error(not_ok, [Else]).

-module(edoc_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").
-include("proplists_eq.hrl").

-define(eq(Expected, Actual), ?proplists_eq(Expected, Actual)).
-define(TESTED, docsh_edoc).

all() ->
    [edoc_to_internal,
     edoc_format_function_g,
     edoc_format_code_in_loose_text,
     edoc_format_non_p_paragraphs,
     edoc_format_dl,
     edoc_format_p,
     edoc_format_pre,
     edoc_format_text1,
     edoc_format_text2,
     edoc_format_ul].

edoc_to_internal(_) ->
    {ok, DBeam} = docsh_beam:from_loaded_module(edoc_example),
    ct:pal("~p", [DBeam]),
    ?assertEqual( #{name => edoc_example,
                    description => <<"Top-level module doc.\n\n">>,
                    items => [ #{kind => function,
                                 name => f,
                                 arity => 0,
                                 exported => true,
                                 description => <<"Doc for f/0.\n\n">>},
                               #{kind => type,
                                 name => r,
                                 arity => 0,
                                 exported => true,
                                 description => <<"Doc for type r().\n\n">>},
                               #{kind => type,
                                 name => s,
                                 arity => 0,
                                 exported => true,
                                 description => <<"Example opaque type s().\n\n">>},
                               #{kind => type,
                                 name => t,
                                 arity => 1,
                                 exported => true,
                                 description => <<"Unary type t/1.\n\n">>} ]},

                  format_descriptions(unwrap( ?TESTED:to_internal(DBeam) ))).

edoc_format_function_g(_) ->
    {ok, DBeam} = docsh_beam:from_loaded_module(edoc_example2),
    D = function_description({g,0}, unwrap(?TESTED:to_internal(DBeam))),
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
           "  What if the definition spans multiple lines?\n"
           "\n"
           "      Let's see.\n"
           "      It can't be that hard.\n"
           "\n"
           "  - Some\n"
           "  - Random\n"
           "  - Para 1\n"
           "    \n"
           "    Para 2\n"
           "\n"
           "  1. One\n"
           "  2. Two\n"
           "  3. Three:\n"
           "    \n"
           "      - a\n"
           "      - b\n"
           "      - c\n\n">>],
        [D]).

edoc_format_h2(C) ->
    edoc_format(C, h2, <<"This fun has a header in its description.\n"
                         "\n"
                         "## This is it\n"
                         "\n"
                         "A following paragraph\n">>).

edoc_format_code_in_loose_text(C) ->
    edoc_format(C, code_in_loose_text, <<"Fetch the internal state of an OTP process.\n"
                                         "Calls sys:get_state/2 directly in R16B01+, and fetches\n"
                                         "it dynamically on older versions of OTP.\n\n">>).

edoc_format_non_p_paragraphs(C) ->
    edoc_format(C, non_p_paragraphs, <<"First non-p paragraph with inline elements.\n"
                                       "\n"
                                       "Second non-p paragraph with inline elements.\n"
                                       "\n"
                                       "Third one.\n\n">>).

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
                        "a paragraph.\n\n">>).

edoc_format_pre(C) ->
    edoc_format(C, pre, <<"    pre\n"
                          "      formatted\n"
                          "        text\n\n">>).

edoc_format_text1(C) ->
    edoc_format(C, text1, <<"Some\n"
                            "loose text,\n"
                            "not a paragraph.\n\n">>).

edoc_format_text2(C) ->
    edoc_format(C, text2, <<"Some\n"
                            "loose text,\n"
                            "not a paragraph.\n"
                            "\n"
                            "A paragraph.\n\n">>).

edoc_format_ul(C) ->
    edoc_format(C, ul, <<"  - Module is any atom representing a module\n"
                         "  - Function is any atom representing a function, or the wildcard\n"
                         "    '_'\n"
                         "  - Args is either the arity of a function (0..255), a wildcard\n"
                         "    pattern ('_'), a\n"
                         "    match specification,\n"
                         "    or a function from a shell session that can be transformed into\n"
                         "    a match specification\n\n">>).

edoc_format(_, Element, Expected) ->
    {ok, B} = docsh_beam:from_loaded_module(edoc_example2),
    D = function_description({Element, 0}, unwrap(?TESTED:to_internal(B))),
    ct:pal("~p", [D]),
    ?eq([Expected], [D]).

function_description({N, A}, #{items := Items}) ->
    [Function] = [ F || F = #{kind := function, name := Name, arity := Arity} <- Items,
                        Name =:= N,
                        Arity =:= A ],
    iolist_to_binary(?TESTED:format_edoc(maps:get(description, Function), #{})).

source_file(Mod) ->
    proplists:get_value(source, Mod:module_info(compile)).

unwrap({ok, V}) -> V;
unwrap(Else) -> erlang:error(not_ok, [Else]).

format_descriptions(Internal) ->
    #{description := ModEDoc} = Internal,
    NewItems = [ Item#{description := iolist_to_binary(?TESTED:format_edoc(ItemEDoc, #{}))}
                 || #{description := ItemEDoc} = Item <- maps:get(items, Internal) ],
    Internal#{description := iolist_to_binary(?TESTED:format_edoc(ModEDoc, #{})),
              items := NewItems}.

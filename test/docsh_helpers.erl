-module(docsh_helpers).
-compile([export_all]).

-define(b2l(B), binary_to_list(B)).
-define(il2b(IL), iolist_to_binary(IL)).

sh(Command) when is_binary(Command) -> sh([Command]);
sh(Command) ->
    sh(Command, []).

sh(Command, Opts) ->
    case erlsh:oneliner(?b2l(?il2b(Command))) of
        {done, 0 = Code, Result} = R ->
            get(sh_log) == true andalso sh_log(Command, Code, Result),
            R;
        {done, Code, Result} = R ->
            get(sh_log) == true andalso sh_log(Command, Code, Result),
            proplists:get_value(dont_fail, Opts, false) /= false
                orelse begin
                           sh_log(Command, Code, Result),
                           ct:fail(R)
                       end,
            R
    end.

sh_log(Command, Code, Result) ->
    ct:pal("command : ~ts\n"
           "code    : ~p\n"
           "result  : ~ts",
           [Command, Code, Result]).

check_precondition({Name, P}, Config) ->
    try
        P(Config),
        ok
    catch _:Reason ->
        ct:fail("~ts failed: ~p", [Name, Reason])
    end.

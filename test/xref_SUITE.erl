-module(xref_SUITE).

-export([all/0]).
-export([xref/1]).

-spec all() -> [xref].
all() -> [xref].

-spec xref(lsl_test_utils:config()) -> {comment, []}.
xref(_Config) ->
    Dirs = [filename:join(code:lib_dir(docsh), "ebin")],
    [] = filter_out_rebar3_as_dep( xref_runner:check(undefined_function_calls, #{dirs => Dirs}) ),
    [] = xref_runner:check(locals_not_used, #{dirs => Dirs}),
    [] = xref_runner:check(deprecated_function_calls, #{dirs => Dirs}),
    [] = xref_runner:check(deprecated_functions, #{dirs => Dirs}),
    {comment, ""}.

filter_out_rebar3_as_dep(Warnings) ->
    F = fun (#{target := {TargetMod, _, _}}) ->
                case atom_to_list(TargetMod) of
                    "rebar" ++ _ -> false;
                    "providers" ++ _ -> false;
                    _ -> true
                end
        end,
    lists:filter(F, Warnings).

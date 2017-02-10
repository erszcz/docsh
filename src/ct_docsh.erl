-module(ct_docsh).

-export([core_transform/2]).

%% Silence _unused_ warning. Don't use this function.
-export([templates/0]).

-import(docsh_lib, [print/2]).

-type cerl() :: cerl:cerl().

-compile({parse_transform, ct_expand}).

-spec core_transform(cerl:c_module(), _) -> cerl:c_module().
core_transform(Mod, _Opts) ->
    %print("core ast: ~p~n", [Mod]),
    %print("core: ~s~n", [core_pp:format(Mod)]),
    Addons = addons(Mod, ct_templates()),
    After = cerl:update_c_module(Mod,
                                 cerl:module_name(Mod),
                                 exports(Addons, cerl:module_exports(Mod)),
                                 cerl:module_attrs(Mod),
                                 defs(Addons, cerl:module_defs(Mod))),
    %print("after: ~s~n", [core_pp:format(After)]),
    After.

addons(Mod, Templates) ->
    [h0(Mod, Templates),
     h2(Mod, Templates),
     format(Templates)].

h0(Mod, Templates) ->
    partially_apply_c_fun(h, get_proto(cerl:c_fname(h, 1), Templates),
                          [cerl:module_name(Mod)], []).

h2(Mod, Templates) ->
    partially_apply_c_fun(h, get_proto(cerl:c_fname(h, 3), Templates),
                          [cerl:module_name(Mod)],
                          [cerl:c_var(docsh0), cerl:c_var(docsh1)]).

format(Templates) ->
    FName = cerl:c_fname(format, 3),
    {FName, get_proto(cerl:c_fname(format, 3), Templates)}.

partially_apply_c_fun(Name, F, Args, Params) ->
    cerl:fun_arity(F) == length(Args) + length(Params) orelse error(arity_mismatch),
    Body = cerl:c_apply(F, Args ++ Params),
    NewF = cerl:c_fun(Params, Body),
    FName = cerl:c_fname(Name, length(Params)),
    {FName, NewF}.

get_proto(FName, Templates) ->
    case lists:keyfind(FName, 1, Templates) of
        false -> error({not_found, FName});
        {FName, CoreFun} -> CoreFun
    end.

exports(Addons, Exports) ->
    [ FName || {FName, _} <- Addons ] ++ Exports.

defs(Addons, Defs) ->
    Addons ++ Defs.

-spec templates() -> [{cerl(), cerl()}].
templates() ->
    {source, Source} = lists:keyfind(source, 1, docsh_embeddable:module_info(compile)),
    {ok, _, CoreTemplate} = compile:file(Source, [to_core, binary]),
    cerl:module_defs(CoreTemplate).

ct_templates() ->
    %% Bake the templates into this module.
    ct_expand:term(templates()).

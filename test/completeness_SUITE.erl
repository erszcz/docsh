-module(completeness_SUITE).
-compile(export_all).

-import(docsh_helpers, [sh/2]).

init_per_suite(_) -> {skip, "work in progress"}.

all() ->
    [sanity_check,
     test1].

sanity_check(_) -> ok.

test1(_) ->
    %put(sh_log, true),
    ct:pal("app_docs: ~p", [app_docs(stdlib)]),
    ct:fail("intentional").

apps() ->
    [
     stdlib
    ].

app_docs(App) ->
    load_application(App),
    {ok, Modules} = application:get_key(App, modules),
    app_modules_docs(App, Modules).

app_modules_docs(App, Modules) ->
    ModulesBeamsSources = [ begin
                                {ok, B} = docsh_beam:from_loadable_module(M),
                                Source = docsh_beam:source_file(B),
                                case has_edoc_comments(Source) of
                                    true -> {has_edoc, M, B, Source};
                                    false -> {no_edoc, M, B, Source}
                                end
                            end || M <- Modules ].

load_application(App) ->
    case application:load(App) of
        ok -> ok;
        {error, {already_loaded, App}} -> ok;
        {error, R} -> error({cannot_load, App, R})
    end.

has_edoc_comments(SourceFile) ->
    case sh(["grep -F @doc ", SourceFile, " > /dev/null"], [dont_fail]) of
        {done, 0, _} -> true;
        {done, _, _} -> false
    end.

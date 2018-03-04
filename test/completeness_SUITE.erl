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
    Stats = [ app_stats(app_sources(App)) || App <- apps() ],
    ct:pal("test1: ~p", [Stats]),
    ct:fail("intentional").

apps() ->
    [
     kernel,
     stdlib
    ].

app_sources(App) ->
    load_application(App),
    {ok, Modules} = application:get_key(App, modules),
    ModulesSources = app_modules_sources(App, Modules),
    {App, ModulesSources}.

app_modules_sources(_App, Modules) ->
    [ begin
          {ok, B} = docsh_beam:from_loadable_module(M),
          {M, docsh_beam:source_file(B)}
      end || M <- Modules ].

app_stats({App, ModulesSources}) ->
    WithEdocFlag = [ case has_edoc_comments(Source) of
                         true -> {M, has_edoc, Source};
                         false -> {M, no_edoc, Source}
                     end || {M, Source} <- ModulesSources ],
    Stats = docsh_lib:group_by(fun ({_,HasEdoc,_}) -> HasEdoc end,
                               WithEdocFlag),
    {App, [ {Feature, length(Items), Items} || {Feature, Items} <- dict:to_list(Stats) ]}.

load_application(App) ->
    case application:load(App) of
        ok -> ok;
        {error, {already_loaded, App}} -> ok;
        {error, R} -> error({cannot_load, App, R})
    end.

has_edoc_comments(SourceFile) ->
    %% It's **so** unlikely that a file is EDoc-documented, but does not use the @doc tag.
    case sh(["grep -F @doc ", SourceFile, " > /dev/null"], [dont_fail]) of
        {done, 0, _} -> true;
        {done, _, _} -> false
    end.

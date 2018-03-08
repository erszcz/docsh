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
     asn1,
     common_test,
     compiler,
     cosEvent,
     cosEventDomain,
     cosFileTransfer,
     cosNotification,
     cosProperty,
     cosTime,
     cosTransactions,
     crypto,
     debugger,
     dialyzer,
     diameter,
     edoc,
     eldap,
     erl_docgen,
     erl_interface,
     erts,
     et,
     eunit,
     hipe,
     ic,
     inets,
     jinterface,
     kernel,
     megaco,
     mnesia,
     observer,
     odbc,
     orber,
     os_mon,
     otp_mibs,
     parsetools,
     public_key,
     reltool,
     runtime_tools,
     sasl,
     snmp,
     ssh,
     ssl,
     stdlib,
     syntax_tools,
     tools,
     wx,
     xmerl
    ].

app_sources(App) ->
    load_application(App),
    {ok, Modules} = application:get_key(App, modules),
    ModulesSources = app_modules_sources(App, Modules),
    {App, ModulesSources}.

app_modules_sources(App, Modules) ->
    [ ModSource
      || M <- Modules,
         {_, _} = ModSource <-
             [case docsh_beam:from_loadable_module(M) of
                  {ok, B} ->
                      {M, docsh_beam:source_file(B)};
                  R ->
                      ct:pal("can't get source for ~p ~p: ~p", [App, M, R]),
                      skip
              end] ].

app_stats({App, ModulesSources}) ->
    WithEdocFlag = [ WithFlag
                     || {M, Source} <- ModulesSources,
                        {_, _, _} = WithFlag <-
                            [try has_edoc_comments(Source) of
                                 true -> {M, has_edoc, Source};
                                 false -> {M, no_edoc, Source}
                             catch
                                 _:_ -> ct:pal("can't tell if has edoc: ~p ~p ~p", [App, M, Source]),
                                        skip
                             end] ],
    Stats = docsh_lib:group_by(fun ({_,HasEdoc,_}) -> HasEdoc end,
                               WithEdocFlag),
    %{App, [ {Feature, length(Items)} || {Feature, Items} <- dict:to_list(Stats) ]}.
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

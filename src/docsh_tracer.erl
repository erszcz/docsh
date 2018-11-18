-module(docsh_tracer).
-compile([export_all, nowarn_export_all]).

-define(il2b, iolist_to_binary).

%% @doc Start and monitor a tracer process.
%%
%% Default dbg:dhandler/2 is nice,
%% but when you want to use domain knowledge
%% or discard some of the data (like too long arg lists / retvals)
%% it might be convenient to write your own dbg handler function.
%% However, this is error prone and since you can't trace your trace function
%% (d'oh!) hard to debug.
%%
%% The way to go is to setup a monitor on the trace handler process.
%% When the trace function errors and kills the tracer process,
%% you'll be notified.
%% Still, if the monitor is setup by the shell, the 'DOWN' message
%% might simply end up in the message queue of the shell
%% and never be received - you won't know a crash happened.
%%
%% So, it's best to setup a process which will monitor the trace handler
%% and print any 'DOWN' messages it receives.
start() ->
    %{ok, Tracer} = dbg:tracer(process, {fun ?MODULE:handler/2, standard_io}),
    {ok, Tracer} = dbg:tracer(process, {fun ?MODULE:handler/2, user}),
    { {tracer, Tracer},
      {tracer_monitor, spawn_link(?MODULE, tracer_monitor, [Tracer])} }.

tracer_monitor(Pid) ->
    MRef = erlang:monitor(process, Pid),
    receive
        {'DOWN', MRef, process, Pid, Info} ->
            io:format("~s ~p exited with info: ~p~n",
                      [?MODULE, Pid, Info])
    end.

handler({trace_ts, _Pid, call, _MFA, TS} = Trace, Out) ->
    print(Out, "~s ~s", [format_timestamp(TS), format_call(strip_ts(Trace))]),
    Out;
handler({trace, _Pid, call, _MFA} = Trace, Out) ->
    print(Out, "~s", [format_call(Trace)]),
    Out;

handler({trace_ts, _Pid, return_from, _MFA, _Ret, TS} = Trace, Out) ->
    print(Out, "~s ~s", [format_timestamp(TS),
                         format_return_from(strip_ts(Trace))]),
    Out;
handler({trace, _Pid, return_from, _MFA, _Ret} = Trace, Out) ->
    print(Out, "~s", [format_return_from(Trace)]),
    Out;

handler(Trace, Out) ->
    pass_to_dbg(Trace, Out).

strip_ts({trace_ts, Pid, call, MFA, _TS})             -> {trace, Pid, call, MFA};
strip_ts({trace_ts, Pid, return_from, MFA, Ret, _TS}) -> {trace, Pid, return_from, MFA, Ret}.

format_call({trace, Pid, call, {M, F, Args}} = _Trace) ->
    [ io_lib:format("~p call ~s:~s/~b:\n", [Pid, M, F, length(Args)]),
      [ io_lib:format("  arg ~b: ~p\n", [I, A]) || {I, A} <- enum(Args) ] ].

format_return_from({trace, Pid, return_from, {M, F, Arity}, Ret} = _Trace) ->
    [ io_lib:format("~p returned from ~s:~s/~b\n  -> ~p\n",
                    [Pid, M, F, Arity, Ret]) ].

translate_args({trace, Pid, call, {M, F, Args}}) ->
    NewArgs = [ translate_one(Arg) || Arg <- Args ],
    {trace, Pid, call, {M, F, NewArgs}}.

translate_ret({trace, Pid, return_from, MFA, Ret}) ->
    {trace, Pid, return_from, MFA, translate_one(Ret)}.

translate_one(Val) ->
    lists:foldl(fun (F, AccV) -> F(AccV) end, Val, translations()).

translations() ->
    [fun flatten_if_state/1].

flatten_if_state(State) when is_tuple(State), element(1, State) == state -> state;
flatten_if_state(Arg) -> Arg.

pass_to_dbg(Trace, Out) ->
    dbg:dhandler(Trace, Out),
    io:format("~n", []),
    Out.

%% {M,F,[A1, A2, ..., AN]} -> "M:F(A1, A2, ..., AN)"
%% {M,F,A}                 -> "M:F/A"
ffunc({M,F,Argl}) when is_list(Argl) ->
    io_lib:format("~p:~p(~s)", [M, F, fargs(Argl)]);
ffunc({M,F,Arity}) ->
    io_lib:format("~p:~p/~p", [M,F,Arity]);
ffunc(X) -> io_lib:format("~p", [X]).

%% Integer           -> "Integer"
%% [A1, A2, ..., AN] -> "A1, A2, ..., AN"
fargs(Arity) when is_integer(Arity) -> integer_to_list(Arity);
fargs([]) -> [];
fargs([A]) -> io_lib:format("~p", [A]);  %% last arg
fargs([A|Args]) -> [io_lib:format("~p,", [A]) | fargs(Args)];
fargs(A) -> io_lib:format("~p", [A]). % last or only arg

print(Handle, Fmt, Args) ->
    io:format(Handle, Fmt, Args).

enum(L) ->
    lists:zip(lists:seq(1, length(L)), L).

format_timestamp(TS) -> format_timestamp(TS, micro).

format_timestamp({_, _, Micro} = TS, Precision) ->
    {_, {H,M,S}} = calendar:now_to_local_time(TS),
    [io_lib:format("~2.10B:~2.10.0B:~2.10.0B", [H, M, S]),
     case Precision of
         seconds -> "";
         milli   -> io_lib:format( ".~3.10.0B", [erlang:round(Micro / 1000)]);
         micro   -> io_lib:format( ".~6.10.0B", [Micro])
     end].

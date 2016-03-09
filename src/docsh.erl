-module(docsh).

-export([main/1]).

-export_type([external/0,
              internal/0]).

-import(docsh_lib, [print/2, print/3]).

%% External documentation format.
%% Right now the only supported format is docsh_elixir_docs_v1 which
%% (TODO) aims to be compatible with Elixir at some point in the future.
-type external() :: any().

%% Internal documentation format.
%% All `docsh_reader` modules convert to this format from their input.
%% All `docsh_writer` modules convert from this format to desired output.
-type internal() :: [{atom(), any()}].

%%
%% Escript API
%%

main(["transform", BEAMFile]) ->
    'try'(fun () -> docsh_lib:process_beam(BEAMFile) end);
main(["transform", BEAMFile, "to", NewBEAMFile]) ->
    'try'(fun () -> {ok, NewBEAM} = docsh_lib:process_beam(BEAMFile),
                    ok = file:write_file(NewBEAMFile, NewBEAM) end);
main(["diff", BEAM1, BEAM2]) ->
    'try'(fun () -> print("~p~n", [docsh_lib:beam_diff(BEAM1, BEAM2)]) end);
main(_) ->
    usage(),
    erlang:halt(1).

%%
%% Helpers
%%

'try'(F) ->
    try F()
    catch
        _:R -> print(standard_error, "~s: ~s~n", [progname(), R]),
               print(standard_error, "~p~n", [erlang:get_stacktrace()]),
               erlang:halt(2)
    end.

usage() ->
    print(standard_error,
          "usage: ~s~n", [progname()]).

progname() ->
    filename:basename(hd(init:get_plain_arguments())).

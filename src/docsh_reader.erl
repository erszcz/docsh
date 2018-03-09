-module(docsh_reader).

-type t() :: module().

%% @doc Return reader module name if reader is available.
%% This might be based on support library availability in a constrained release
%% or information about the passed in beam file.
-callback available(docsh_beam:t()) -> R when
      R :: [docsh_reader:t()].

%% @doc Read information about the beam file and return in docsh internal format.
-callback to_internal(docsh_beam:t()) -> R when
      R :: {ok, docsh:internal()}
         | {error, any(), [erlang:stack_item()]}.

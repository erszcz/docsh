-module(docsh_reader).

-callback to_internal(file:filename()) -> R when
      R :: {ok, docsh:internal()}
         | {error, any()}.

-module(docsh_reader).

-callback to_internal(docsh_beam:t()) -> R when
      R :: {ok, docsh:internal()}
         | {error, any()}.

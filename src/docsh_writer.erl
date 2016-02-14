-module(docsh_writer).

-callback from_internal(docsh:internal()) -> any().

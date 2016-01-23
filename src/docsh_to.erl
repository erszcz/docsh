-module(docsh_to).

-callback from_internal(docsh:internal()) -> any().

-module(docsh_format).

-export_type([kna/0]).

-type t() :: any().
-type kna() :: {atom(), atom(), arity()}.

-callback lookup(docsh_beam:t(), kna()) -> [binary()].
-callback merge([t()]) -> t().

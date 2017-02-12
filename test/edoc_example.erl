%% @doc Top-level module doc.
%% @end
-module(edoc_example).
-export([f/0]).

-type l(A) :: list(A).
-type l() :: list().

%% @type r(). Doc for type r().
-type r() :: ok.

%% @doc Doc for f/0.
%% @end
-spec f() -> r().
f() ->
    ok.

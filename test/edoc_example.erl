%% @doc Top-level module doc.
%% @end
-module(edoc_example).
-export([f/0]).

-export_type([r/0,
              t/1]).

-type l(A) :: list(A).
-type l() :: list().

%% @type r(). Doc for type r().
-type r() :: ok.

%% @type s(). Example opaque type s().
-opaque s() :: opaque_type.

%% @type t(Arg). Unary type t/1.
-type t(Arg) :: list(Arg).

%% @doc Doc for f/0.
%% @end
-spec f() -> r().
f() ->
    ok.

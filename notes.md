# Intended API

    (edoc@x4)16> lists:h(keyfind).
    ** exception error: undefined function lists:h/1
    (edoc@x4)17> h(lists, keyfind).
    ** exception error: undefined shell command h/2


# Useful

    code:where_is_file("lists.beam").


# Using Core Erlang

There are some snippets for getting / compiling / printing Core Erlang in `snippets.erl`.

Using `parse_trans_codegen` for defining `h/0` and `h/2` helpers leads to quite verbose
and unwieldy code.
Either it has to be written as one huge blob, which is obviously not cool,
or if split into logical components, has to be manually glued together by
passing around `erl_syntax` tree bits and using `parse_trans` `$form`
metavariables to stick these bits together.

It would be much simpler to write the helpers[^ft:helpers] in plain Erlang
and be able to call them as ordinary functions as well as use them as
templates for code generation.
This is what `docsh_rt` (runtime) tries to achieve,
but it's not completely solved yet.

[^ft:helpers]: By _the helpers_ I mean all the support code which might be
               required for `docsh` to fetch and present documentation to
               the user after code is deployed.
               This _support code_ could be shipped as a library with the system.
               Then only thin stubs would need to be compiled into each
               `docsh`-enabled module, but these stubs would require a runtime dependency.
               That's how `module_info/0,2` functions work - they're present in every module,
               but the heavy lifting is done by a call into `erlang:get_module_info/1` which is
               guaranteed to exist.

               Alternatively, the support could be compiled into each `.beam` file of the project,
               alleviating the need for any runtime dependency
               That's the current approach.
               The only _helpers_ right now are `h/0,2` and `__docs/0`.

For maximum code reuse the final handler of the extracted documentation
run in the deployed system (`docsh_rt:h/1,3`) is passed as a fun to the outer guard
functions (see `docsh_rt:h0/1`).
While elegant, this solution doesn't yield an easily embeddable parse tree
to use in the _no runtime_ variant of deployment.
Either more gluing resembling the call chain of functions in `docsh_rt`
would have to be done manually or we could rely on the compiler to inline
this call chain and use the inlined version in the _no runtime_ scenario.

We're at the core (sic!) of the problem now.
The compiler inlining pass runs over the Core Erlang representation,
much later than `pt_docsh` is run.
Unless it's possible (TODO: is it?) to recover an Erlang syntax tree from
a Core Erlang (`cerl` from now on) representation,
it won't be possible to use `cerl` functions as templates for embedding into
the module which is transformed by `pt_docsh`.
However, `pt_docsh` could be replaced by `ct_docsh`, a Core Erlang transformation.

There're two more points, though, which require some comments:

-   The inlined body of `docsh_rt:h0/1` (see `docsh_rt.core:9`, variable `_cor0`)
    doesn't contain the module name into which it's to be embedded - that's why
    although the function is a template of a nullary function, it still is of arity 1.
    This parameter would have to be provided when generating final code
    for embedding along with the documentation into a target `.beam` file.

-   The compiler doesn't inline `fun` references even if they're static and known
    at compile time (see `docsh_rt:h/1` and compare with `docsh_rt.core:10`).
    Either this last step would have to be monkey-patched or a generic core
    transformation pass could be developed (doesn't seem hard at the first glance).


# Pretty printing

First we need an AST: `epp:parse_file/2` is the easiest way to get it
apart from running a `parse_transform`.

However, some of the tools below don't work with this AST,
only with the one defined by `erl_syntax`.
See `erl_tidy` for getting the latter from the former.
`epp_dodger:parse_file/1` seems to return `erl_syntax` trees.

Nothing I could find works as expected:

- `prettypr` - generic pretty printer
- `erl_prettypr` - print ASTs, not that easy to use
- `erl_pp` - built in, easy to use, ugly output
- `erl_tidy` - reading it shows how to use `erl_prettypr`
- `edoc_layout` - a terrible mess of tiny functions,
  but gives the best results; see branch `dead-end-edoc-types`

The biggest surprise is that
`erl_tidy:file("test/edoc_example.erl", [{stdout, true}])`,
which seemed to be the most advanced / best tool for the job,
gives this:

```
 1	%% @doc Top-level module doc.
 2	%% @end
 3	-module(edoc_example).
 4	-export([f/0]).
 5	-include_lib("docsh/include/pt_docsh.hrl").
 6	-type({r, {atom, 8, ok}, []}).
 7	%% @doc Doc for f/0.
 8	%% @end
 9	-spec({{f, 0},
10	       [{type, 12, 'fun',
11	         [{type, 12, product, []}, {user_type, 12, r, []}]}]}).
12	f() -> ok.
```

I.e. lines 6 and 9-11 are mangled, the file doesn't even compile anymore.
No support for `-type` and `-spec`?

There might be a way forward thanks to the `hook()` (see
`erl_prettypr.erl:199`).
It can be used to pretty-print terms the default formatter doesn't recognize.
See `dialyzer_utils.erl:746` for an example.

## 2016-02-02 update

`dialyzer_behaviours.erl:159` uses `erl_types:t_to_string/2` for pretty printing.
The latter seems to support type unions, products and whatnot, but the type
representation is yet different from EDoc, `erl_parse`, `erl_syntax`,
and `erl_prettypr`.
Compund types are represented as:

```erlang
-define(any,  any).
-define(none, none).
-define(unit, unit).
%% Generic constructor - elements can be many things depending on the tag.
-record(c, {tag               :: tag(),
      elements  = []          :: term(),
      qualifier = ?unknown_qual :: qual()}).

-opaque erl_type() :: ?any | ?none | ?unit | #c{}.
```

# Some brainstorming on UI / API

```erlang
9> erlang:fun_info(fun lists:keyfind/3).
[{module,lists},
 {name,keyfind},
 {arity,3},
 {env,[]},
 {type,external}]
10> erlang:fun_info(fun lists:keyfind/3, module).
{module,lists}
11> erlang:fun_info(fun lists:keyfind/3, arity).
{arity,3}
12> erlang:is_function(fun lists:keyfind/3).
true
13> erlang:is_function({lists, keyfind}).
false
14> erlang:is_function({lists, keyfind, 3}).
false
15> t(fun lists:keyfind/3).
** exception error: undefined shell command t/1
16>
```

# Generating documentation at runtime

The assumption I had for generating documentation for OTP modules was that
if Erlang was installed via kerl, I could rely on `M:module_info/0`
returning valid paths to source files in the local file system.
This assumption is invalid, as the `otp_src_VERSION.tar.gz` bundles come
with precompiled `.beam` files - not just for the `bootstrap/` subtree,
but also for the modules under `lib/`.
This means returned paths to source files point at some arbitrary
locations in the package builder's file system not in mine.

For the time being, I've manually deleted all the `.beam` files in the
build directory and rebuilt the whole tree.
This allows me to proceed with development of the mechanism for runtime
documentation generation.
Erlang installation with docsh support on a developer's workstation
requires further consideration.

It seems that `beam_lib` and `code`/`code_server` can't fetch chunks which
are actually loaded by the emulator - they always read them from the `.beam`
file or the passed in binary.
This means that simply reloading the module (see `reload_with_exdc/2`
at `docsh_shell.erl:28`) won't be sufficient for `docsh_embeddable:h/1,3`
to pick up the newly generated ExDc chunk.

# `user_default` extensions

TODO: Put this into the README later.

A functional calling style is being worked on:

```erlang
> h(fun lists:keyfind/3).
> h(lists, keyfind, 3).
```

To be able to use it add this to your `user_default` module and make sure `docsh`
are in the Erlang code path (e.g. install it as a Rebar3 global plugin and use
`rebar3 shell` or use `ERL_LIBS` when calling `erl`):

```erlang
%% file: user_default.erl
h(M) -> docsh_shell:h(M).
h(M, F, A) -> docsh_shell:h(M, F, A).
```

`docsh_shell` is also the entry point into providing docs for modules
which don't carry them embedded inside - i.e. when `docsh_shell` is called
we might try fetching the docs from different places (initially the source
code if it's available).

# `.beam` file cache for modules

Rely on `DOCSH_CACHE` env var to look for `.beam` files with embedded docs
for use from the shell.
This way, for each looked up module, we would first look into the cache,
rebuild the module if not present, but source code is available,
and then provide docs from the cached `.beam` instead of the original.

# 2016-05-17 status update

Shell extensions can now extract type specs and docs (when available)
from the OTP apps' modules:

```erlang
> h(fun compile:file/1).
-spec file(module() | file:filename()) -> comp_ret().

undefined
ok
> h(fun lists:keyfind/3).
-spec keyfind(Key, N, TupleList) -> Tuple | false
                 when
                     Key :: term(),
                     N :: pos_integer(),
                     TupleList :: [Tuple],
                     Tuple :: tuple().

undefined
ok
```

This still requires one manual step when building Erlang,
because the tarballs from erlang.org carry compiled `.beam` files,
not just the source code.

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

## Finding the source files

The assumption I had for generating documentation for OTP modules was that
if Erlang was installed from source or via kerl, I could rely on `M:module_info/0`
returning valid paths to source files in the local file system.
This assumption is invalid, as the `otp_src_VERSION.tar.gz` bundles come
with precompiled `.beam` files - not just for the `bootstrap/` subtree,
but also for the modules under `lib/`.
This means that the source file paths contained in the modules are valid on the build host,
but are not valid on my or your machine,
even though the source files are present in the distributed tarball.

## Fixing the source paths

For the time being, I've manually deleted all the `.beam` files in the
build directory and rebuilt the whole tree.
This allows me to proceed with development of the mechanism for runtime
documentation generation.

In the future, docsh should take this into consideration and try to
heuristically rewrite the known build-host-valid paths to local paths.

## Accessing loaded modules' chunks directly

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

# 2017-05-15 No EDoc for modules distributed with Erlang/OTP #7

Locally compiled Erlang (reaching this printout with unmodified
Kerl/Erlang installation is the desired result):

```
3> h(wx, new, 1).

wx:new/1

-spec new([Option]) -> wx_object()
             when
                 Option ::
                     {debug, list() | atom()} |
                     {silent_start, boolean()}.

Starts a wx server.
Option may be {debug, Level}, see debug/1.
Or {silent_start, Bool}, which causes error messages at startup to
be suppressed. The latter can be used as a silent test of whether
wx is properly installed or not.

ok
```

Because:

```
$ ./bin/beamfile chunk CInf /Users/erszcz/apps/erlang/18.2-local/lib/wx-1.6/ebin/wx.beam
beam file:    /Users/erszcz/apps/erlang/18.2-local/lib/wx-1.6/ebin/wx.beam
chunk name:    CInf
chunk data:
[{options,[{outdir,"/Users/erszcz/.kerl/builds/18.2/otp_src_18.2/lib/wx/src/../ebin"},
           {i,"/Users/erszcz/.kerl/builds/18.2/otp_src_18.2/lib/wx/src/../include"},
           warn_unused_vars,debug_info]},
 {version,"6.0.2"},
 {time,{2016,5,9,14,58,27}},
 {source,"/Users/erszcz/.kerl/builds/18.2/otp_src_18.2/lib/wx/src/wx.erl"}]
```

Source-installed (actualy kerl-installed) Erlang:

```
7> h(wx, new, 0).

Source file for wx is not available. If it's a standard module distributed with Erlang/OTP, the issue is known (https://github.com/erszcz/docsh/issues/7) and will be addressed in the future. Otherwise, you might've found a bug - please report it!

wx:new/0

-spec new() -> wx_object().

ok
```

Because:

```
$ ./bin/beamfile chunk CInf /Users/erszcz/apps/erlang/18.2/lib/wx-1.6/ebin/wx.beam
 beam file:    /Users/erszcz/apps/erlang/18.2/lib/wx-1.6/ebin/wx.beam
chunk name:    CInf
chunk data:
[{options,[{outdir,"/net/isildur/ldisk/daily_build/18_prebuild_opu_o.2015-12-15_21/otp_src_18/lib/wx/src/../ebin"},
           {i,"/net/isildur/ldisk/daily_build/18_prebuild_opu_o.2015-12-15_21/otp_src_18/lib/wx/src/../include"},
           warn_unused_vars,debug_info]},
 {version,"6.0.1"},
 {time,{2015,12,15,20,59,33}},
 {source,"/net/isildur/ldisk/daily_build/18_prebuild_opu_o.2015-12-15_21/otp_src_18/lib/wx/src/wx.erl"}]
```

The clue is difference between `source` parameters.

How to get the desired path suffix?

```
lists:reverse(lists:takewhile(fun ("lib") -> false; (_) -> true end,
                              lists:reverse(string:tokens("/Users/erszcz/.kerl/builds/18.2/otp_src_18.2/lib/wx/src/wx.erl",
                                                          "/")))).
```

Returns:

```
["wx","src","wx.erl"]
```

How to tell if Kerl is enabled (i.e. is this Erlang activated by Kerl)?

```
%% Returns string (truthy) or false.
os:getenv("_KERL_PATH_REMOVABLE").
```

How to programatically get the current OTP version?

```
{ok, Version} = file:read_file([proplists:get_value(root, init:get_arguments()),
                                "/releases/", erlang:system_info(otp_release), "/OTP_VERSION"]).
```

Algo steps:

-   when loading a new module

-   if it's an Erlang/OTP module with a nonexistent source file
    and if Kerl is enabled:

    * guess the source destination (just assume/hardcode a valid value for Kerl)
    * rewrite the source path
    * write the module to docsh cache

# 2017-05-16 False warning on EDoc / source availability

A module for which we inferred the source file, once written do docsh cache,
when processed again with h(Mod, Fun) still warns about source code
unavailability.
The docs extracted from source code are available, though, as they're
already written to the cached .beam file's ExDc chunk.

# 2017-05-30 Hooking into IEx

Use Elixir from https://github.com/erszcz/elixir/tree/beam-doc-provider:

```
22:43:36 erszcz @ x2 : ~
$ cat ~/.iex.exs 
IEx.configure colors: [ eval_result: [ :cyan, :bright ] ]
#IEx.configure beam_doc_provider: :docsh_iex
Application.put_env(:iex, :beam_doc_provider, :docsh_iex)
22:43:39 erszcz @ x2 : ~
$ iex 
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

Enabled docsh from: /home/erszcz/work/erszcz/docsh
Interactive Elixir (1.4.4) - press Ctrl+C to exit (type h() ENTER for help)
iex(1)> h :docsh

                                     :docsh                                     

# Module docsh

## Description

Welcome to docsh, the missing documentation viewer for the Erlang shell. This
project provides a number of helpers for accessing module and function doc
comments, function specs and exported types.

To access this documentation you've quite likely typed:

h(docsh).

h/1,2,3 is the most generic invocation of docsh. h/1 prints documentation for a
given module (as shown above) or function:

h(fun lists:keyfind/3).

h/2,3 are limited to functions. h/2 displays information about all functions of
the given name from a particular module, while h/3 also takes the expected
function arity:

h(proplists, get_value). h(proplists, get_value, 3).

s/1,2,3 is the helper to use if you're only interested in function specs, not
their full documentation:

s(fun proplists:get_value/3). s(proplists, get_value). s(proplists, get_value,
3).

t/2,3 is the helper for printing types exported from modules:

t(gen_tcp, connect_option). t(gen_udp, socket).

All the helpers described above are only available if you installed the
user_default extensions that ship with docsh. See
https://github.com/erszcz/docsh/README.md if unsure about the installation
steps or what user_default is.

If you want to call docsh as a library, please refer to docsh_shell. (TODO) As
of now it's the only way to use docsh from Elixir's iex.

## Types

-type external() :: any().

-type internal() :: [{atom(), any()}].

iex(2)> 
```

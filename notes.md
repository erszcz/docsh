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

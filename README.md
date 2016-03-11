# Docs in the Shell

Still can't write match specs from the top of your head?
Forgetting again and again `dbg` flags or the syntax of `recon` trace patterns?
Ever wished for access to documentation straight from `erl`
the way it's possible in modern languages like Python, Ruby or Elixir?

```
> recon:h().
## Description

Recon, as a module, provides access to the high-level functionality
contained in the Recon application.
... snip ...

## Types

-type proc_attrs() ::
          {pid(),
           Attr :: _,
           [Name ::
                atom() |
                {current_function, mfa()} |
                {initial_call, mfa()},
            ...]}.

-type inet_attrs() :: {port(), Attr :: _, [{atom(), term()}]}.

... snip ...

> recon:h(get_state, 1).
-spec get_state(pid_term()) -> term().

Shorthand call to recon:get_state(PidTerm, 5000)

ok
> recon:h(get_state, 2).
-spec get_state(pid_term(), Ms :: non_neg_integer() | infinity) ->
                   term().

Fetch the internal state of an OTP process.
Calls sys:get_state/2 directly in R16B01+, and fetches
it dynamically on older versions of OTP.

ok
>
```

You're in the right place.
`docsh` makes online (as in _when connected to a live system_,
not _in the internets_) access to documentation possible in Erlang.


## Try it

Check out the [example project using docsh][docsh-example].
Otherwise, here's an example session:

```sh
git clone https://github.com/erszcz/docsh.git
cd docsh
./rebar3 escriptize
erlc +debug_info -pa _build/default/lib/docsh/ebin/ test/recon.erl
_build/default/bin/docsh transform recon.beam to recon.beam
erl # now try out the examples from the previous listing: recon:h() ...
```

[docsh-example]: https://github.com/erszcz/docsh-example


## Use

### With Rebar 3

The setup is a bit quirky right now, because I don't know yet how to use
the same app as a plugin and a project dependency at the same time:

```erlang
{erl_opts, [debug_info, {core_transform, ct_docsh}]}.

{deps,
 [
  {docsh, {git, "https://github.com/erszcz/docsh", {branch, "rebar3-plugin"}}}
 ]}.

{plugins,
 [
  {rebar3_docsh, {git, "https://github.com/erszcz/docsh", {branch, "rebar3-plugin"}}}
 ]}.

{provider_hooks,
 [
  {post, [{compile, {docsh, compile}}]}
 ]}.
```

The `{core_transform, ct_docsh}` option enables documentation for all
modules in the project.
If you want to be more specific about modules which should contain
embedded docs and which should not, don't use the option.
Instead, include the header file in your module:

```erlang
-include_lib("docsh/include/docsh.hrl").
```

Each approach of enabling the core transformation will embed helper code
needed for accessing the documentation into your modules.
The documentation itself is embedded straight into the `.beam` file by the
post-compile `{docsh, compile}` hook.
The support code makes your documentation accessible wherever you ship your code.
No separate doc package - when you deploy your code,
you automagically deploy your docs.

`docsh` - the dependency - is needed so that Rebar can find `ct_docsh` transformation.
`rebar3_docsh` - the plugin - provides the post-compile pass which bakes
the docs into the modules.


## ToDo

- [x] Make [Recon](https://github.com/ferd/recon) compile with `docsh`
      and provide useful docs for all commented modules.

- [x] Use a `core_transform` instead of a `parse_transform` to generate code.
      [Notes on using Core Erlang](notes.md#using-core-erlang)
      describe why it makes sense.

- [x] Include defined types in module description.

- [x] Include specs in function descriptions.
      Extract specs from the AST or input docs if possible.

- [ ] Fix EDdoc extraction and formatting ~~by calculating element
      indentation based on its path in the document tree and its formatting
      based on its type.~~ Almost done!

    * [ ] Extract all [module tags][edoc:module-tags].

- [x] Provide [an example repo showing how to use `docsh`][docsh-example].

- [ ] Provide a tool to embed docs into a .beam file "ExDc" chunk like Elixir does:

    * [ ] Try to find source code and munge it to get the docs if debug
          info is not available.
    * [x] EScript for command-line use.
    * [ ] Rebar3 plugin for a post-compile build step.
          Almost! Needs some polishing.

- [ ] `user_default` extensions:

    * [ ] To allow for functional `h(Mod)`, `h(Mod, Fun, Arity)`
          style calls.
    * [ ] To enable reading Elixir embedded documentation;
          the same doc format is used intentionally,
          though for now `docsh` doesn't store docs in the same place as Elixir does.
    * [ ] Provide documentation for well-known modules (read: OTP)
          from a cache seeded from .erl sources or an external database?

[edoc:module-tags]: http://erlang.org/doc/apps/edoc/chapter.html#Module_tags


## ?!

Yes, I've seen _Ghost in the Shell_ ;)

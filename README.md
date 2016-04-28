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

Check out the [example project using docsh][gh:docsh-example]
or [Recon with docsh enabled out of the box][gh:recon-docsh].
Otherwise, here's an example session:

```sh
git clone https://github.com/erszcz/docsh.git
cd docsh
./rebar3 escriptize
erlc +debug_info -pa _build/default/lib/docsh/ebin/ test/recon.erl
_build/default/bin/docsh transform recon.beam to recon.beam
erl # now try out the examples from the previous listing: recon:h() ...
```


## Use


### As a [Rebar3 global plugin][rebar3:plugins]

[`docsh-example`][gh:docsh-example] shows how to use this approach.

Rebar3 global plugins are downloaded and installed automatically.
Make sure `rebar3_docsh` plugin is in your `~/.config/rebar3/rebar.config`
as shown below:

```erlang
{plugins,
 [
  {rebar3_docsh, {git, "https://github.com/erszcz/docsh", {tag, "0.2.0"}}}
 ]}.
```

With the plugin in place, the configuration of your project is minimal:

```erlang
{erl_opts, [debug_info, {core_transform, ct_docsh}]}.

{provider_hooks,
 [
  {post, [{compile, {docsh, compile}}]}
 ]}.
```

The `{core_transform, ct_docsh}` option enables documentation for all
modules in the project.
If you want to be more specific about which modules should provide
embedded docs and which should not don't use the option.
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


### As a Rebar3 project plugin

The setup is a bit quirky, because I don't know how to use
the same app as a plugin and a project dependency at the same time:

```erlang
{erl_opts, [debug_info, {core_transform, ct_docsh}]}.

{deps,
 [
  {docsh, {git, "https://github.com/erszcz/docsh", {tag, "0.2.0"}}}
 ]}.

{plugins,
 [
  {rebar3_docsh, {git, "https://github.com/erszcz/docsh", {tag, "0.2.0"}}}
 ]}.

{provider_hooks,
 [
  {post, [{compile, {docsh, compile}}]}
 ]}.
```

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

- [x] Provide [an example repo showing how to use `docsh`][gh:docsh-example].

- [ ] `user_default` extensions:

    * [x] To allow for functional `h(Mod)`, `h(Mod, Fun, Arity)`
          style calls.

          Done.
          See `notes.md` for usage and `src/docsh_shell.erl` for
          implementation.
          See notes on UX in the todos.

    * [ ] Provide documentation for modules which don't have it embedded.
          See _`.beam` file cache for modules_ in `notes.md` for details.

    * [ ] To enable reading Elixir embedded documentation;
          the same doc format is used intentionally,
          though for now `docsh` doesn't store docs in the same place as Elixir does.

- [ ] Provide a tool to embed docs into a .beam file "ExDc" chunk like Elixir does:

    * [ ] Try to find source code and munge it to get the docs if debug
          info is not available.
    * [x] EScript for command-line use.
    * [x] Rebar3 plugin for a post-compile build step.
    * [ ] Make sure "ExDc" chunk format is compatible with Elixir and
          works with IEx.

- [ ] Polish the UX:

    * [ ] Don't error out when asked about local / undefined functions
          (`Mod:h(some_local_fun, 0)`).
    * [ ] Make `M:h/2` display type info apart from the already
          supported info for functions.
    * [ ] Provide arity agnostic `M:h/1` to display information about all
          functions / types of the given name.
    * [ ] Provide `M:t/2` to display just the `-spec` / `-type` attribute.
          In some cases we're only interested in the order
          of parameters and in general already know what a function does.

- [ ] Fix EDdoc extraction and formatting ~~by calculating element
      indentation based on its path in the document tree and its formatting
      based on its type.~~ Almost done!

    * [ ] Extract all [module tags][edoc:module-tags].
    * [ ] EDoc doesn't extract doc for functions marked with `@private`.
          See [`recon_trace:h(count_tracer, 1)`][gh:recon-docsh].
          Extract the marker from the AST and provide a disclaimer in the shell.
          This info should also be useful to present as `def` / `defp`
          distinction for Elixir compatibility.


## ?!

Yes, I've seen _Ghost in the Shell_ ;)


[edoc:module-tags]: http://erlang.org/doc/apps/edoc/chapter.html#Module_tags
[gh:docsh-example]: https://github.com/erszcz/docsh-example
[gh:recon-docsh]: https://github.com/erszcz/recon
[rebar3:plugins]: http://www.rebar3.org/docs/using-available-plugins

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

Just copy and paste:

```
git clone https://github.com/erszcz/docsh.git
cd docsh
./rebar3 ct
./rebar3 as test shell
recon:h().
```

## Use

**WARNING: This section is temporarily invalid,
since documentation is no longer embedded at the parse transformation stage,
and no rebar3 plugin is available yet!**
For now it's only possible to embed docs into a .beam file using the EScript, like so:

```sh
./rebar3 compile && ./rebar3 escriptize
erlc +debug_info -pa _build/default/lib/docsh/ebin/ test/recon.erl
_build/default/bin/docsh transform recon.beam to recon.beam
```

The resulting `recon` module will have embedded documentation and the `h/0,2` helpers available.

Rebar3:

```erlang
{docsh, {git, "https://github.com/erszcz/docsh.git", {branch, master}}}
```

Rebar2, if you still have to (sorry, no semantic versioning yet):

```erlang
{docsh, ".*", {git, "https://github.com/erszcz/docsh.git", {branch, master}}}
```

Include the public header file in your module exposing a shell-usable API
with embedded documentation:

```erlang
-include_lib("docsh/include/docsh.hrl").
```

The header contains a `compile` directive which will embed the
documentation straight into the generated `.beam` file,
therefore making it accessible wherever you ship your code.
No extra build steps, no separate doc package - when you deploy your code,
you automagically deploy your docs.


## Goals

- Provide added value to existing community projects and libraries as well as OTP.
- Require minimum fuss to enable in a project.
- Elixir `iex` doesn't provide access to documentation for Erlang modules.
  If possible, change that.

## ToDo

In no particular order:

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

- [ ] Provide an example repo/branch showing how to use `docsh`.

- [ ] Provide a tool to embed docs into a .beam file "ExDc" chunk like Elixir does:

    * [ ] Try to find source code and munge it to get the docs if debug
          info is not available.
    * [x] EScript for command-line use.
    * [ ] Rebar3 plugin for a post-compile build step.

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

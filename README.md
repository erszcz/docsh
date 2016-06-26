# Docs in the Shell

Still can't write match specs from the top of your head?
Forgetting again and again `dbg` flags or the syntax of `recon` trace patterns?
Ever wished for access to documentation straight from `erl`
the way it's possible in modern languages like Python, Ruby or Elixir?

```
> h(recon).
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

> h(fun recon:get_state/1).
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


## Use it

`$DOCSH_ROOT` is just a placeholder - use whatever location/variable
makes the most sense to you.

```
cd $DOCSH_ROOT
git clone https://github.com/erszcz/docsh
cd docsh
./rebar3 compile
```

Now make sure to place these lines in your
[`user_default`](http://erlang.org/doc/man/shell_default.html) file:

```erlang
h(M) -> docsh_shell:h(M).
h(M, F, A) -> docsh_shell:h(M, F, A).
```

Don't forget to compile it!
With the shell extensions in place:

```
git clone https://github.com/ferd/recon
cd recon
ERL_LIBS=$DOCSH_ROOT/docsh/_build/default/lib erl -pa ebin
```

Once in the Erlang shell:

```erlang
> h(recon_trace).
## Description

recon_trace is a module that handles tracing in a safe manner for single
Erlang nodes, currently for function calls only. Functionality includes:

  - Nicer to use interface (arguably) than dbg or trace BIFs.

...
> h(fun recon_trace:calls/3).
-spec calls(tspec() | [tspec(), ...], max(), options()) -> num_matches().

Allows to set trace patterns and pid specifications to trace
function calls.

...
```

Try it with your project and let me know what the results are!
Better yet, send a PR if you find any issues ;)


## ToDo

- [x] Make [Recon](https://github.com/ferd/recon) compile with `docsh`
      and provide useful docs for all commented modules.

- [x] Use a `core_transform` instead of a `parse_transform` to generate code.
      [Notes on using Core Erlang](notes.md#using-core-erlang)
      describe why it makes sense.

- [x] Include defined types in module description.

- [x] Include specs in function descriptions.
      Extract specs from the AST or input docs if possible.

- [x] Provide an example repo showing
      [how to embed documentation in your .beam files][gh:docsh-example]
      by using `docsh` as a Rebar plugin.

- [ ] `user_default` extensions:

    * [x] To allow for functional `h(Mod)`, `h(Mod, Fun, Arity)`
          style calls.
          If debug info/source code is available the shell extension
          will just extract any documentation and type information it can find
          and present it.

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

# Docs in the Shell

Still can't write match specs from the top of your head?
Forgetting again and again `dbg` flags or the syntax of `recon` trace patterns?
Ever wished for access to documentation straight from `erl`
the way it's possible in modern languages like Python, Ruby or Elixir?

![docsh in action](https://github.com/erszcz/blog/blob/69e14100229b6733a9ece5840fb23f29f32d2b33/posts/3.update-on-docsh/docsh-shell.gif)

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
> h(fun recon:get_state/2).
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

Now make sure to [place these lines in your `user_default` file](#the-user_default-file):

```erlang
h(M) -> docsh_shell:h(M).
h(M, F, A) -> docsh_shell:h(M, F, A).
```

Don't forget to compile it!
With the shell extensions in place:

```
git clone https://github.com/ferd/recon
cd recon
./rebar compile
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

[`docsh` also supports OTP libraries and applications](#use-it-with-otp),
but requires a small hack for it to work.
Moreover, this is not well tested yet, so _expect bugs_.

```erlang
> h(fun proplists:get_value/3).
-spec get_value(Key, List, Default) -> term()
                   when
                       is_subtype(Key, term()),
                       is_subtype(List, [term()]),
                       is_subtype(Default, term()).

ok
```

Try it with your project and let me know what the results are!
Better yet, send a PR if you find any issues ;)


### The `user_default` file

The Erlang shell can be customized by using
the [`user_default`](http://erlang.org/doc/man/shell_default.html) file.
To do so you first need to create `$HOME/.erlang` which will load
your customizations into the shell:

```erlang
% file: $HOME/.erlang
code:load_abs(os:getenv("HOME") ++ "/.erlang.d/user_default").
```

Then create `$HOME/.erlang.d` directory, `user_default.erl` inside it,
and compile the module:

```
mkdir $HOME/.erlang.d
cd $HOME/.erlang.d
cat <<EOF > user_default.erl
-module(user_default).
-compile(export_all).
h(M) -> docsh_shell:h(M).
h(M, F, A) -> docsh_shell:h(M, F, A).
EOF
erlc user_default.erl
```

That's it!
If you're curious about what else might go into this file then have a look at
[Serge Aleynikov's example `user_default`](https://github.com/saleyn/util/blob/master/src/user_default.erl).


### Use it with OTP

Erlang "source" tarballs aren't really source tarballs - just the
C code is compiled on your machine if you "install from source".
What does it mean for docsh?

If `debug_info` isn't available in a .beam file
(and it's not the case for the OTP modules)
docsh looks for a source file the module is compiled from.
If the location of this file is not a valid path on the local machine,
docsh won't find the .erl and fail.

Here's how to fix it for an Erlang built by kerl.
Go to the Erlang/OTP build directory, remove all .beam files below it
and rebuild them:

```
cd ~/.kerl/builds/18.1/otp_src_18.1
find . -name \*.beam -exec rm '{}' \;
make
```

Now reinstall the build (possibly under a new name) as you would usually
do with kerl:

```
kerl install 18.1 ~/apps/erlang/18.1-rebuilt
```

Once you activate this installation,
all the source paths in OTP modules will target local files on your machine.
This will enable docsh to work even for Erlang/OTP modules.


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

    * [x] Provide documentation for modules which don't have it embedded.
          See _`.beam` file cache for modules_ in `notes.md` for details.

          If debug info/source code is available the shell extension
          will just extract any documentation and type information it can find
          and present it.

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

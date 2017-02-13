# Docs in the Shell

[![TravisCI Build Status](https://travis-ci.org/erszcz/docsh.svg?branch=master)](https://travis-ci.org/erszcz/docsh)

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


## Installation

```
git clone https://github.com/erszcz/docsh
cd docsh
./rebar3 compile
make activate
```

Now, activate docsh for your current shell session
(`$DOCSH` is where you cloned the project).
All it does is exporting `ERL_LIBS` with the proper path set up:

```sh
. $DOCSH/activate
```

Otherwise, you can source the `activate` script from your `.bashrc`
(or similar for ZSH etc).

Make sure to [include docsh header in your `user_default` file](#the-user_default-file)
just below the exports:

```erlang
-export([...]).

-include_lib("docsh/include/docsh_user_default.hrl").
```

Don't forget to compile it!


## Usage

Activate docsh for the session or in your shell's config file:

```sh
. $DOCSH/activate
```

Let's see what docsh can give us for some OTP modules.
We call `h/2` to get the doc for `lists:keyfind` no matter the arity:

```
$ erl
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V8.2  (abort with ^G)
1> h(lists, keyfind).

lists:keyfind/3

-spec keyfind(Key, N, TupleList) -> Tuple | false
                 when
                     Key :: term(),
                     N :: pos_integer(),
                     TupleList :: [Tuple],
                     Tuple :: tuple().

ok
2>
```

Let's try with Recon:

```
git clone https://github.com/ferd/recon
cd recon
./rebar compile
erl -pa ebin/
```

Once in the Erlang shell:

```erlang
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V8.2  (abort with ^G)
> s(recon_trace, calls).

recon_trace:calls/2

-spec calls(tspec() | [tspec(), ...], max()) -> num_matches().


recon_trace:calls/3

-spec calls(tspec() | [tspec(), ...], max(), options()) -> num_matches().

ok
> h(recon_trace, calls, 2).

recon_trace:calls/2

-spec calls(tspec() | [tspec(), ...], max()) -> num_matches().

Equivalent to calls({Mod, Fun, Args}, Max, [])
See calls/3

ok
>
```

As you can see above, `s/2` gives us just the function specs.
Having read them, we want a more detailed description of `recon_trace:calls/2`,
so we ask for the doc and specify the arity with `h/3`.

Try it with your project!


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
-include_lib("docsh/include/docsh_user_default.hrl").
EOF
```

Activate docsh (otherwise the header will not be found) and compile the file:

```
. $DOCSH/activate
erlc user_default.erl
```

That's it!
If you're curious about what else might go into this file then have a look at
[Serge Aleynikov's example `user_default`](https://github.com/saleyn/util/blob/master/src/user_default.erl).


## ToDo

- [x] Make [Recon](https://github.com/ferd/recon) compile with `docsh`
      and provide useful docs for all commented modules.

- [x] ~~Use a `core_transform` instead of a `parse_transform` to generate code.
      [Notes on using Core Erlang](notes.md#using-core-erlang)
      describe why it makes sense.~~

      Not a goal anymore.
      Helper code generation is a burden which slows down
      useful feature development.
      Given it's easy to bundle docsh as a library in a release,
      the goal of embedding helper code into .beam files is dropped.
      Don't confuse it with storing the doc/spec information in the .beam files
      as a separate chunk - this is the core functionality and works just fine.

- [x] Include defined types in module description.

- [x] Include specs in function descriptions.
      Extract specs from the AST or input docs if possible.

- [x] Provide an example repo showing
      [how to embed documentation in your .beam files][gh:docsh-example]
      by using `docsh` as a Rebar plugin.

- [x] `user_default` extensions:

    * [x] To allow for functional `h(Mod)`, `h(Mod, Fun, Arity)`
          style calls.

    * [x] Provide documentation for modules which don't have it embedded.
          See _`.beam` file cache for modules_ in `notes.md` for details.

          If debug info/source code is available the shell extension
          will just extract any documentation and type information it can find
          and present it.

- [x] Provide a tool to embed docs into a .beam file.

    * [x] EScript for command-line use.
    * [x] Rebar3 plugin for a post-compile build step.
    * [x] Shell extension for interactive use.

- [x] ~~Properly format/print out record definitions.~~

      Since Erlang/OTP 19.0 [one of the standard pretty printers
      is much more capable](https://github.com/erlang/otp/commit/ee80210).
      ~~Use `erl_prettypr` instead of `erl_pp`.~~
      `erl_prettypr` is now used when available,
      while `erl_pp` with older versions.

- [ ] Polish the UX:

    * [x] ~~Don't error out when asked about local / undefined functions
          (`Mod:h(some_local_fun, 0)`).~~
          Display `not found` or similar message.
    * [x] Make `t/2` display type info apart from the already
          supported info for function docs and specs.
          ~~Possibly make the types display along with function docs
          when `h/2,3` is called and a function/type name clash occurs.~~
    * [x] Provide arity agnostic `M:h/1` to display information about all
          functions / types of the given name.
    * [x] Provide `s/2,3` to display just the `-spec` attribute.
          In some cases we're only interested in the order
          of parameters and in general already know what a function does.

- [ ] Make installation easy:

    * [x] Provide `activate` script to source in a Bash session/config file.
    * [x] Provide a header file to include in `user_default.erl`
          which exports up to date docsh shell interface.
    * [ ] Provide an installation script for generating `user_default.erl`
          or including the docsh header in it if the file already exists.

- [ ] Provide builtin documentation for `docsh` and `docsh_shell` modules

- [ ] Fix EDdoc extraction and formatting ~~by calculating element
      indentation based on its path in the document tree and its formatting
      based on its type.~~ Almost done!

    * [ ] Extract all [module tags][edoc:module-tags].
    * [ ] EDoc doesn't extract doc for functions marked with `@private`.
          See [`recon_trace:h(count_tracer, 1)`][gh:recon-docsh].
          Extract the marker from the AST and provide a disclaimer in the shell.
          This info should also be useful to present as `def` / `defp`
          distinction for Elixir compatibility.

- [ ] Elixir compatibility.

    * [ ] Make sure "ExDc" chunk format is compatible with Elixir and
          works with IEx.


## ?!

Yes, I've seen _Ghost in the Shell_ ;)


[edoc:module-tags]: http://erlang.org/doc/apps/edoc/chapter.html#Module_tags
[gh:docsh-example]: https://github.com/erszcz/docsh-example
[gh:recon-docsh]: https://github.com/erszcz/recon
[rebar3:plugins]: http://www.rebar3.org/docs/using-available-plugins

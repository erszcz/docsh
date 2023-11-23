# ARCHIVED

`docsh` was a prototype of a documentation system for the Erlang shell,
inspired by languages like Python, Ruby, or Elixir,
in which docs are first class elements of the language.

It predated Erlang/OTP 23, which was the first release to
include [`shell_docs`](https://www.erlang.org/doc/man/shell_docs)
authored by @garazdawi.

For Erlang/OTP 24 I (@erszcz) contributed a new feature to Erlang's EDoc:
[emitting EEP-48 style doc chunks][edoc_chunks],
which can be consumed by `shell_docs` or [ExDoc](https://github.com/elixir-lang/ex_doc).

If you want to document an Erlang Rebar3 project with ExDoc or generate doc chunks for it,
check out @starbelly's neat
[rebar3_ex_doc](https://github.com/starbelly/rebar3_ex_doc) plugin!

These ecosystem changes make `docsh`, the prototype, obsolete, so I'm archiving this repo.
I'm really happy, though, that the idea spearheaded by this project caught on!

[edoc_chunks]: https://www.erlang.org/doc/apps/edoc/chapter#Doc_chunks

# Docs in the Shell

[![TravisCI Build Status](https://travis-ci.org/erszcz/docsh.svg?branch=master)](https://travis-ci.org/erszcz/docsh)
[![Hex version badge](https://img.shields.io/hexpm/v/docsh.svg)](https://hex.pm/packages/docsh)

Still can't remember if it's `(Tab, Key)` or `(Key, Tab)` in `ets:lookup/2`?
What about `gb_trees:lookup/2`? Hint: it's not the same!
Ever wished for access to function signatures and documentation straight from `erl`
the way it's possible in languages like Python, Ruby or Elixir?

[![docsh - light and dark background](https://raw.githubusercontent.com/erszcz/docsh/master/doc/light-dark-bg.png)](https://github.com/erszcz/docsh/blob/master/doc/light-dark-bg.png)


## Use in Rebar3 shell

Add these lines to your `rebar.config`:

```erlang
{plugins,
 [
  {rebar3_docsh, "0.7.2", {pkg, docsh}}
 ]}.
{shell, [{script_file, "_build/default/plugins/rebar3_docsh/priv/docsh_rebar3_shell.escript"}]}.
```

`rebar3 shell` will now have a set of new helpers to access docs. Start with `h(docsh)`.

Note: This will dynamically compile and load a `user_default` module shipped with docsh.
It will override your own `user_default` if you use one.


## Use in `erl`

```
git clone https://github.com/erszcz/docsh
cd docsh
./install.sh
```

The script will ask if you're sure you want to create some
Erlang configuration files:

```
$ ./install.sh

Installing docsh

This install script will make docsh globally available in your
user environment.
It will install the following files:

  /Users/erszcz/.erlang
  /Users/erszcz/.erlang.d/user_default.erl

To know more about these files please refer to:

  man erl - sections about 'The .erlang startup file'
            and 'user_default and shell_default'
  man shell_default - parts about user_default

Do you want to proceed? [y/N]
```

Even if you agree to install, but the target files exist,
it won't proceed - don't worry if you have customized your
Erlang environment already.

Once the installation is complete,
`erl` will greet you in a bit different way than you might be used to:

```erlang
$ erl
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:4:4] [async-threads:10] [kernel-poll:false]

Enabled docsh from: /Users/erszcz/work/erszcz/docsh
Eshell V8.2  (abort with ^G)
1>
```


## Access docs in the shell

Let's see what docsh can give us for some OTP modules.
We call `h/2` to get the doc for `lists:keyfind` no matter the arity:

```
$ erl
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

Enabled docsh from: /Users/erszcz/work/erszcz/docsh/_build/default/lib/docsh
Call h(docsh) for interactive help.

Eshell V8.2  (abort with ^G)
1> h(proplists).

# proplists

Support functions for property lists.

Property lists are ordinary lists containing entries in the form
of either tuples, whose first elements are keys used for lookup and
insertion, or atoms, which work as shorthand for tuples {Atom,
true}. (Other terms are allowed in the lists, but are ignored
by this module.) If there is more than one entry in a list for a
certain key, the first occurrence normally overrides any later
(irrespective of the arity of the tuples).

Property lists are useful for representing inherited properties,
such as options passed to a function where a user may specify options
overriding the default settings, object properties, annotations,
etc.

% @type property() = atom() | tuple()

ok
2> t(proplists).
-type property() :: atom() | tuple().
-type proplist() :: [property()].
ok
3>
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

Enabled docsh from: /Users/erszcz/work/erszcz/docsh/_build/default/lib/docsh
Call h(docsh) for interactive help.

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


## Embed docs in your modules

Make sure to document your code with EDoc.
Then add the following to your project's `rebar.config`:

```erlang
{plugins,
 [
  {rebar3_docsh, "0.7.2", {pkg, docsh}}
 ]}.

{provider_hooks,
 [
  {post, [{compile, {docsh, compile}}]}
 ]}.
```


[edoc:module-tags]: http://erlang.org/doc/apps/edoc/chapter.html#Module_tags
[gh:recon-docsh]: https://github.com/erszcz/recon

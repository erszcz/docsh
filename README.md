# Docs in the Shell

Still can't write match specs from the top of your head?
Forgetting again and again `dbg` flags or the syntax of `recon` trace patterns?
Ever wished for access to documentation straight from `erl`
the way it's possible in civilized languages like Python, Ruby or Elixir?

```erlang
> recon:h().
Recon, as a module, provides access to the high-level functionality
contained in the Recon application.
...
```

You're in the right place.
`docsh` is an effort to make online (as in _when connected to a live
system_, not _in the internets_) access to documentation possible in Erlang.

**This is a work in progress! You've been warned.**


## Use

Rebar3:

```erlang
{docsh, {git, "https://github.com/lavrin/docsh.git", {branch, master}}}
```

Rebar2, if you still have to (sorry, no semantic versioning yet):

```erlang
{docsh, ".*", {git, "https://github.com/lavrin/docsh.git", {branch, master}}}
```

Include the public header file in your module exposing a shell-usable API
with embedded documentation:

```erlang
-include_lib("docsh/include/pt_docsh.hrl").
```

The header contains a `parse_transform` directive which will embed the
documentation straight into the generated `.beam` file,
therefore making it accessible wherever you ship your code.
No extra build steps, no separate doc package - when you deploy your code,
you automagically deploy your docs.


## ToDo

In no particular order:

- [ ] Make whole [Recon](https://github.com/ferd/recon) compile with docsh.

- [ ] Extract specs from the AST.

- [ ] Figure out how to embed the docs into the "ExDc" chunk like Elixir does.
      It's most likely not possible from within a parse transformation.
      What would be a sensible approach?
      A Rebar3 plugin embedding the extra chunk after actual compilation?

- [ ] Add support for type spec extraction from the AST.


## ?!

Yes, I've seen _Ghost in the Shell_ ;)

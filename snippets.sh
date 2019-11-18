./rebar3 compile && ./rebar3 escriptize
erlc +debug_info -pa _build/default/lib/docsh/ebin/ test/recon.erl 
_build/default/bin/docsh transform recon.beam to recon.beam

# generate ExDoc documentation with Wojtek's docs_chunks
rm -rf docs && r3 compile && ERL_LIBS=/Users/erszcz/work/erszcz/docsh/_build/default/lib /Users/erszcz/work/wojtekmach/docs_chunks/docs_chunks -project && /Users/erszcz/work/elixir-lang/ex_doc/ex_doc docsh 0.7.1 _build/default/lib/docsh/ebin -m docsh -o docs

# generate ExDoc documentation with rebar3_edoc_chunks plugin
rm -rf docs && r3 compile && /Users/erszcz/work/elixir-lang/ex_doc/ex_doc docsh 0.7.2 _build/default/lib/docsh/ebin -m docsh -o docs

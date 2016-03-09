./rebar3 compile && ./rebar3 escriptize
erlc +debug_info -pa _build/default/lib/docsh/ebin/ test/recon.erl 
_build/default/bin/docsh transform recon.beam to recon.beam

# docsh 0.7.0

- Look up type descriptions not just definitions (1ae2df9)
- Store EDoc in `docs_v1` chunk - the consumer is free to format it in any
  way that is desirable (4fdfacf)
- Provide a `docs_v1` EDoc formatter for `erl` output - the previous
  formatter was spaghetti, now it's way cleaner,
  but still only supports a subset of EDoc/HTML;
  tested mostly on Recon and Meck (4fdfacf)
- Support OTP 21 (2671154, 07bae15, eb57ce8)
- Drop support for OTP 18 (2671154)
  * Drop `no_map_specs` ifdefs (b449d1f)
  * Drop `erl_prettypr_no_specs` ifdefs (240465e)


# docsh 0.6.0

- Support EEP 48 - https://github.com/erszcz/docsh/issues/18
- Rebar3 plugin - https://github.com/erszcz/docsh/commit/d7fadb83dacadb2d2576d83ca5343697021a3d87
- Hex package


# docsh 0.5.0

- Support Erlang 20 "Dbgi" chunk (01661fa)
- Guess source file location even if src/ has a hierarchical structure (b442346)
- Fix inconsistent lookup times (7149177)
- Expand shell helper documentation (ab3e143)
- Clean up remaining compiler / Dialyzer warnings (60a202f)
- Apply gomoripeti's fix for proplists' doc retrieval (5407e56)
- Describe manual docsh setup (ecf3582)

# K7 OxCaml port

Imported https://github.com/samoht/ocaml-k7 at
`dcc79a8ca8b24ce834471357390019cf76d2abcb` on 2026-11-01. The full source tree
is retained except Git and CI metadata, including upstream's Matrix backport.

Local adaptations:

- `compat/` supplies the OCaml 5.4 Char.Ascii and List functions used by K7.
  Its Sys.signal_to_int stub translates OCaml's negative signal constants to
  operating-system signal numbers using the OCaml runtime, rather than
  assuming they already are POSIX numbers.
- Dune opens the compatibility module and omits the upstream CI warning
  variable. The compiler floor is OxCaml 5.2.
- Fmt callbacks are eta-expanded. Jsont absent defaults are factories.
- Eio sink callbacks accept local Cstruct lists. The Tail sink consumes them
  directly. Test sinks copy their contents before retaining them.

Keep upstream README.md, LICENSE and the backport provenance. On re-vendoring,
reapply these changes and update `../upstreams.json`. Verify:

```
dune build vendor/k7/bin/main.exe
dune build --force @@vendor/k7/test/runtest @u/avsm/arod/test/console_port/runtest
```

The external guard checks signal-number conversion. K7's tests exercise PTY
recording, replay, rendering and cleanup. Optional fuzz suites need Alcobar.

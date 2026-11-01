# Console OxCaml port

Imported Console 0.9.0 from https://github.com/samoht/ocaml-console at
`de6681a6956fd57f759d761b6eecaf515e41159c` on 2026-11-01. The full source tree
is retained except Git and CI metadata. This replaces `samoht/console`.

Local adaptations:

- `compat/` supplies the OCaml 5.4 `Char.Ascii` and `List` functions used by
  upstream on OxCaml 5.2. Dune opens it in each dependent compilation unit.
  The upstream CI warning variable is omitted.
- Jsont absent defaults are factories. Event enum mappings are factories too,
  so portable codecs capture no nonportable closure records. Formatting in
  portable validation uses Stdlib rather than Fmt.
- `Task_text.capped` uses Matrix's portable grapheme iterator. It preserves
  whole grapheme boundaries without capturing Console's mutable width cache
  or nonportable Uucp callbacks. Both segmenters use Unicode 17 in this switch.
  The Unicode dependency floor is 17.0.0. Matrix is a runtime dependency.
- `Color` retains the previous port's cached Lab palette and unboxed float
  arithmetic, using the compiler-provided `stdlib_upstream_compatible`. Differential tests compare
  it with pristine upstream arithmetic.
- Fmt callbacks are eta-expanded to avoid escaping local partial applications.
- `dune` and `examples/dune` set `LC_ALL=C.UTF-8` for their actions in
  place of upstream's `en_US.UTF-8`. glibc always provides `C.UTF-8`, while
  `en_US.UTF-8` exists only where it was generated. Each shell an action
  starts otherwise warns that it cannot set the locale.
- The cross-domain rejection test uses Eio's explicitly unsafe test entry
  point to exercise rejection of the nonportable display API. Assertions run
  on the main domain.

The old Glyph substitution and Canvas buffering patch are obsolete with the
new upstream View renderer. Applications use View, Task, Progress and
Console_tty directly. Progress uses Eio monotonic clocks.

## Re-vendoring checks

Keep upstream README.md and LICENSE. Reapply the adaptations above and update
`../upstreams.json`. Build the CLI consumers under `avsm/`, then run:

```
dune build --force @avsm/arod/test/console_port/runtest @avsm/siftbot/runtest @avsm/bushel/test/knowledge_output/runtest
dune build --force @@vendor/console/test/runtest @@vendor/console/test/input/runtest @@vendor/console/test/text/runtest @@vendor/console/test/tty/runtest @@vendor/console/test/eio/runtest @@vendor/console/test/invariants/runtest @@vendor/console/test/cram/runtest
```

The guard outside vendor compares colours with pristine upstream and grapheme
segmentation with Uuseg. Sift exercises real PTYs, plain output and SIGINT
cleanup. Optional fuzz suites require Alcobar, which is not installed here.

The checked allocation build is `dune build --profile release-check
vendor/console/lib/console.cmxa vendor/k7/lib/k7.cmxa`.

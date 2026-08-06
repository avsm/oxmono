# oxcaml mode-system reproductions

Small test cases for behaviour found while adding `portable` annotations
to Eio (see `PROFFER.md`). Compiler: `5.2.0+ox`.

Each directory is self-contained. The dune rules assert the current
behaviour with `with-accepted-exit-codes`, so:

    dune build @oxcaml-repro/runtest

passes today, and a directory's rule starts failing on the day a
compiler fix changes the behaviour it pins down.

| Case | Summary |
|------|---------|
| `01-nested-sig-default-override` | Item-level `@@ nonportable` cannot override a file-level `@@ portable` default inside a nested signature. |
| `02-mode-crossing-attr-vs-abstract-kind` | `[@@unsafe_allow_any_mode_crossing]` is silently inert unless the declaration also repeats an explicit kind annotation. |
| `03-global-value-no-mode-inference` | Constants of abstract types read as `contended` from portable functions, and only the defining library can declare the crossing kind. |
| `04-extensible-variant-exception` | Extensible payload types make an exception unmatchable in portable code, with no safe escape. |
| `05-effect-perform-nonportable` | `Effect.perform` is nonportable, so no effect-performing code can be annotated without re-declaring the primitive. |

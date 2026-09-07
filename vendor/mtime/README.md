## mtime - monotonic wall-clock time for OCaml

This is mtime 2.2.0, vendored from https://erratique.ch/software/mtime and
patched for OxCaml portability. Both the core library and the clock
sublibrary are here.

The copy was taken from `~/.opam/default/.opam-switch/sources/mtime.2.2.0`,
the source tree opam unpacked for the `mtime.2.2.0` package. `mtime.ml`,
`clock/mtime_clock_stubs.c` and `LICENSE.md` are byte-identical to it. The
patch is `mtime.mli`, `clock/mtime_clock.mli` and the three externals at the
head of `clock/mtime_clock.ml`, and it is annotations only.

### The patch, hunk by hunk

* `mtime.mli` carries `@@ portable` at the head of the file, which annotates
  every value in the interface, the `Span` submodule included. Nothing needed
  a narrower annotation, and `mtime.ml` compiles unchanged against it. Unlike
  ptime, there was no table to replace: the module holds no array and no
  mutable state, only `int64` arithmetic. `Span.pp`, `Span.dump` and the two
  timestamp formatters are portable too, since only the global formatters of
  `Format` are nonportable and each of these takes its own.
* `Mtime.span` and `Mtime.t` are given the `immutable_data` kind. Both are an
  `int64`, so a span or a timestamp crosses portability and contention and
  can be held at module level and read from a portable handler. They are
  abstract in the interface, so without the annotation they would cross
  nothing.
* `clock/mtime_clock.mli` carries `@@ portable` at the head of the file, and
  `Mtime_clock.counter` is given the `immutable_data` kind for the same
  reason as the two above.
* `clock/mtime_clock.ml` carries `@@ portable` in type position on
  `elapsed_ns`, `now_ns` and `period_ns`. A signature ascription does not
  lift a structure-level external, so the annotation has to be on the type.

### Why the clock externals are portable

Unlike ptime's `current_tz_offset_s`, which reads static storage on every
call and is annotated `@@ nonportable` there, all three stubs here are safe
to call from any domain.

`now_ns` and `period_ns` hold no state on POSIX: they are `clock_gettime` and
`clock_getres` on a caller-provided `struct timespec`. `elapsed_ns` subtracts
a `static` origin, and on Darwin all three consult a `static` timebase scale.
Those statics are written lazily on first call, which would be a race between
domains, except that `mtime_clock.ml` ends its initialisation with

    let () = ignore (elapsed_ns ())

so the origin and the scale are both written while the program is still
single-domain, and every later call only reads them. That initialiser is
upstream's, and it is what makes the annotation sound rather than merely
convenient.

One residual window is upstream's own. The POSIX `elapsed_ns` re-initialises
its origin whenever `start.tv_sec` is still `0`, so a program started within
the first second of `CLOCK_MONOTONIC` can rewrite the origin on a later call.
Two domains racing there each write a value read from the same clock a few
nanoseconds apart, and each reads its own result, so the span it returns
stays monotonic. It is not introduced by this patch and it is not worth
diverging from upstream over.

### Why the copy exists

`mtime` is not installed in the `5.2.0+ox` switch at all, so the in-tree
consumers, `avsm/srcsetter`, `bleeding/fetch` and `bleeding/proffer`, and the
vendored `vendor/eio` and `vendor/progress`, had nothing to link against. A
switch copy would not have helped in any case, since its interface carries no
mode annotations and a `Mtime.span` timeout cannot be read from the portable
closures the serving path is built out of.

Nothing is shadowed by this copy. A sweep of every `META` under
`~/.opam/5.2.0+ox/lib` for an `mtime` requirement matched nothing, so no
installed package is forced into the workspace build by it.

### Behaviour identity

`mtime.ml` and `clock/mtime_clock_stubs.c` are byte-identical to upstream
2.2.0, and the change to `clock/mtime_clock.ml` is three type annotations, so
there is no behavioural difference to establish. `diff` against the opam
source tree is the whole proof, and the re-vendoring checklist repeats it.

`avsm/arod/test/test_mtime.ml` is the standing guard, 50 checks. It pins the
scale of every duration constant, the unsigned comparisons that make
`max_span` the longest span rather than the shortest, span and timestamp
arithmetic including the overflow and underflow cases, the float conversions
at their bounds, and all thirteen `Span.pp` forms including the U+03BC
microsecond spelling and the round-towards-positive-infinity rule that prints
12'345ns as `12.4μs`.

Each annotation was reverted in turn to check that it does. Dropping
`@@ portable` from `mtime.mli` or from the three externals fails inside the
vendored clock, which needs the core to be portable to satisfy its own
interface. Dropping it from both interfaces at once fails in the test, at its
first ascription. Dropping the `immutable_data` kinds fails in the test on the
closures that read a module-level span, timestamp or counter, which is the
only thing that would notice: a type used only as a parameter or a result need
not cross anything.

### What else differs from the upstream distribution

* `mtime.opam` is the upstream `opam` file with the `ocamlfind`, `ocamlbuild`
  and `topkg` build dependencies replaced by `dune`, since the workspace
  builds this copy directly, and with a paragraph in the description
  recording the patch.
* `dune`, `dune-project`, `clock/dune` and `clock/os/dune` are written for
  this workspace. Upstream ships `_tags`, `pkg/pkg.ml`, `B0.ml`,
  `myocamlbuild.ml` and the `.mllib` files instead, and none of those are
  vendored.
* Upstream's META names the clock library both `mtime.clock` and, deprecated,
  `mtime.clock.os`. `vendor/eio` and `vendor/progress` write the deprecated
  name, so `clock/os/dune` declares an empty library under it that re-exports
  the real one.
* The `top` sublibrary is not vendored. Nothing in the workspace uses it.
* `clock/runtime.js`, upstream's js_of_ocaml implementation of the three
  stubs, is not vendored. Nothing in the workspace targets JavaScript. A
  re-vendor that needs it should copy it back and add
  `(js_of_ocaml (javascript_files runtime.js))` to `clock/dune`.
* Upstream's `CHANGES.md`, `README.md`, `doc/` and `test/` are not vendored.
  The release this copy came from is named in `mtime.opam` and here.

### Re-vendoring checklist

1. Copy `src/mtime.ml`, `src/mtime.mli`, `src/clock/mtime_clock.ml`,
   `src/clock/mtime_clock.mli` and `src/clock/mtime_clock_stubs.c` from the
   new release over this directory, keeping `dune`, `dune-project`,
   `clock/dune`, `clock/os/dune`, `mtime.opam`, `LICENSE.md` and this file.
2. Reapply the hunks above. All of them are annotations, so a release that
   changes no interface needs no thought beyond replacing them.
3. Update the version in `mtime.opam` and in the first line of this file, and
   confirm the two unpatched files are still identical to the new release:

       diff <upstream>/src/mtime.ml vendor/mtime/mtime.ml
       diff <upstream>/src/clock/mtime_clock_stubs.c \
            vendor/mtime/clock/mtime_clock_stubs.c

   If either differs, the note under **Behaviour identity** no longer holds.
   Re-read the stubs against **Why the clock externals are portable** before
   keeping the annotations, since that argument is about what the C does.
4. Dune skips aliases under a vendored directory, so `dune runtest` does not
   reach anything here. Verify through the consumer aliases instead:

       dune build @avsm/arod/all @avsm/arod/runtest --force
       dune build @avsm/srcsetter/all @bleeding/fetch/all
       dune build @bleeding/proffer/all @vendor/eio/all @vendor/progress/all

   `avsm/arod/test/test_mtime.ml` is the gate. It fails if an annotation is
   dropped, and it fails if a release moves a constant or a `pp` form.

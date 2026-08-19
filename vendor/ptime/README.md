## ptime - POSIX time for OCaml

This is ptime 1.2.0, vendored from https://erratique.ch/software/ptime and
patched for OxCaml portability. Both the core library and the clock
sublibrary are here.

The code patch touches `ptime.ml`, `ptime.mli`, `ptime_clock.ml` and
`ptime_clock.mli`. Nothing in `ptime_clock_stubs.c` changed.

### The patch, hunk by hunk

* `Span.frac_div` was a module-level `int64 array` of the thirteen decimal
  divisors. An array is mutable data, so a portable function cannot read one
  held at module level, and this one is on the path of `Span.round`,
  `Span.truncate`, `Ptime.truncate` and every `to_rfc3339` that renders a
  fractional second. It is now a match from the digit count to the divisor.
  `Ptime.truncate` does not clip its `frac_s` argument, so an out of range
  count used to raise what `Array.get` raises. The match's last arm is
  `invalid_arg "index out of bounds"`, which keeps that exception, message
  included.
* `max_month_day` closed over a module-level `int array` of month lengths. A
  closure over an array is not portable, and `is_date_valid` calls it, so
  `of_date_time` and everything above it were nonportable. The lengths are a
  match instead, and its last arm keeps the same out of range exception.
* `weekday` closed over a module-level array of the seven days, for the same
  reason. It is a match on `weekday_num` instead. `weekday_num` answers in
  `[0;6]`, so the last arm cannot be reached.
* `ptime.mli` carries `@@ portable` at the head of the file, which annotates
  every value in the interface, the `Span` submodule included. Nothing needed
  a narrower annotation. `pp`, `pp_human`, `pp_rfc3339`, `dump` and
  `pp_rfc3339_error` are portable too, since only the global formatters of
  `Format` are nonportable and each of these takes its own.
* `Ptime.t` and `Ptime.span` are given the `immutable_data` kind. Both are an
  `int` paired with an `int64`, so a timestamp or a span crosses portability
  and contention and can be held at module level and read from a portable
  handler.
* `ptime_clock.ml` carries `@@ portable` in type position on the two clock
  externals. A signature ascription does not lift a structure-level external,
  so the annotation has to be on the type. The stubs behind them are
  `clock_gettime` and `gettimeofday`, which are thread safe, so calling them
  from any domain is sound.
* `ptime_clock.mli` carries `@@ portable` at the head of the file and
  `@@ nonportable` on `current_tz_offset_s`. That one function's stub reads
  `localtime` and `gmtime`, which answer with pointers into static storage
  shared by every thread, so two domains calling it at once would race. No
  in-tree caller needs it from a portable context. Switching the stub to
  `localtime_r` and `gmtime_r` would let the annotation go, at the cost of a
  C change that cannot be tested here on anything but Darwin.

Timekeeping behaviour is unchanged. This is checked by a differential run of
the whole interface against the pristine 1.2.0 sources: timestamps, spans,
date-time conversion, RFC 3339 parsing of valid and malformed input under
every combination of `strict`, `sub` and `start`, RFC 3339 rendering at every
fractional digit count and a range of time zone offsets, span arithmetic and
rounding, float conversion including nan and the infinities, and the
pretty printers. All 43'840 lines of transcript are byte identical, as are the
480'814 lines of a second sweep over the three replaced tables, which walks
every month of every year from 0 to 9999 and every weekday.

### What else differs from the upstream distribution

* `ptime.opam` is the upstream file with the `ocamlfind`, `ocamlbuild` and
  `topkg` build dependencies replaced by `dune`, since the workspace builds
  this copy directly, and with a paragraph in the description recording the
  patch.
* `dune`, `dune-project` and `test/` are written for this workspace rather
  than taken from upstream.
* Upstream's META names the clock library both `ptime.clock` and, deprecated,
  `ptime.clock.os`. In-tree consumers write both, so `clock/os/dune` declares
  an empty library under the deprecated name that re-exports the real one.
* The `top` sublibrary is not vendored. Nothing in the workspace uses it.
* `clock/runtime.js`, upstream's js_of_ocaml implementation of the three
  stubs, is not vendored. Nothing in the workspace targets JavaScript. A
  re-vendor that needs it should copy it back and add `(js_of_ocaml (javascript_files runtime.js))`
  to `clock/dune`.
* Upstream's `CHANGES.md`, `README.md` and doc pages are not vendored. The
  release this copy came from is named in `ptime.opam` and here.

### The jsonfeed side effect

A vendored `public_name` shadows the installed package for the whole
workspace, and two libraries named `ptime` cannot be linked into one
executable. Every workspace dependency of ptime therefore has to be built
from source alongside it. Of the installed packages that require ptime,
`syndic` was already vendored, `crunch` is only ever run as a binary, and
`jsonfeed` was not. `vendor/jsonfeed` is that copy, taken unpatched from
jsonfeed v1.1.0 and present only to keep the link consistent.

### Re-vendoring checklist

1. Copy `ptime.ml`, `ptime.mli`, `clock/ptime_clock.ml`,
   `clock/ptime_clock.mli` and `clock/ptime_clock_stubs.c` from the new
   release over this directory, keeping `dune`, `dune-project`, `ptime.opam`,
   `LICENSE.md`, this file and `test/`.
2. Reapply the hunks above. The three tables are the only ones that are more
   than an annotation.
3. Update the version in `ptime.opam` and in the first line of this file.
4. `dune build @avsm/arod/all @avsm/arod/runtest`, which reaches the
   portability ascriptions in `avsm/arod/test/test_ptime.ml`.
5. `dune exec vendor/ptime/test/test_ptime.exe`.
6. Rebuild every consumer, since this copy shadows the installed package for
   all of them: `dune build @avsm/sortal/all @avsm/sortal/runtest
   @avsm/bushel/all @avsm/tessabot/all @bleeding/proffer/all
   @bleeding/httpz/all @bleeding/atp/all @bleeding/apubt/all
   @bleeding/tomlt/all @bleeding/jsonwt/all @bleeding/cookeio/all
   @bleeding/openapi/all @bleeding/immich/all @bleeding/karakeep/all
   @bleeding/peertube/all @bleeding/frontmatter/all @vendor/x509/all
   @vendor/tls/all @vendor/ca-certs/all @vendor/syndic/all
   @vendor/asn1-combinators/all @vendor/cohttp-eio/all @vendor/jsonfeed/all`.

`test/` pins the three replaced tables exhaustively. Dune skips aliases under
a vendored directory, so `dune runtest` does not reach it and it must be run
by name:

    dune exec vendor/ptime/test/test_ptime.exe

Because that test is inert in an ordinary build, `avsm/arod/test/test_ptime.ml`
holds the portability guarantees outside `vendor/`, and so under `dune
runtest`. A re-vendor that drops the patch fails there.

The two test files divide the guarantee. The plain ascriptions pin the
`@@ portable` annotations. The closures that read `Ptime.epoch`, `Ptime.min`,
`Ptime.max` and a module-level span pin the `immutable_data` kinds, which
nothing else here would notice the loss of, since a type used only as a
parameter or a result need not cross anything.

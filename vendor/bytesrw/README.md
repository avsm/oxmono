## bytesrw - composable byte stream readers and writers for OCaml

This is bytesrw 0.4.0, vendored from the `v0.4.0` tag of
https://github.com/dbuenzli/bytesrw and patched so that its `Bytes.Slice`
operations borrow their argument at the `local` mode rather than requiring a
heap value, and so that the `Slice` formatters print their ellipsis. The patch
is the hunks inventoried below, in five files. Everything else is the 0.4.0
release text.

The copy exists because `vendor/jsont` shadows the installed jsont, and jsont
requires bytesrw. Two libraries named `bytesrw` cannot go into one executable,
so the dependency has to be built from source alongside it.

Arod does not call bytesrw itself. It reaches this copy through jsont.

### What differs from the upstream distribution

* `dune` and `dune-project` are written for this workspace. Upstream builds
  with ocamlbuild and topkg and keeps the sources under `src/`.
* `bytesrw.opam` is the installed file. Upstream generates it.
* Only the pure-OCaml core is vendored: `bytesrw`, `bytesrw_fmt`,
  `bytesrw_utf` and `bytesrw_hex`. The `zlib`, `zstd`, `blake3`, `md`,
  `xxhash`, `tls`, `crypto`, `unix` and `sysrandom` sublibraries are not, since
  nothing in this workspace uses them and the C ones would need their system
  libraries. `vendor/decompress/bench/dune` names `bytesrw.zlib`, but the
  switch has no such sublibrary installed either, and dune skips aliases under
  `vendor/`, so that rule has never built.
* Upstream's `CHANGES.md`, `README.md`, `doc/`, `test/`, `B0.ml` and `BRZO`
  are not vendored. The release this copy came from is named in `bytesrw.opam`
  and here.
* The `base` dependency, and the patch below.

### Local patches

Thirty hunks. They came from `opam/bytesrw` on `main`, last present at
`761947088`, across the commits "bytesrw: OxCaml optimizations", "bytesrw: add
OxCaml mode annotations to interfaces", "bytesrw: expand OxCaml local mode
annotations" and "bytesrw: add Base dependency for local-aware Slice
operations". That copy was the same 0.3.0 release, so the hunks transfer
unchanged.

The 0.4.0 update was a three-way merge: pristine 0.3.0 as the base, this copy
as ours, pristine 0.4.0 as theirs. Only `bytesrw.ml` and `bytesrw.mli` changed
upstream between the two releases, so the other three patched files carried
over unchanged. Two hunks conflicted and both resolved to this copy, for the
reasons under "Two behaviour changes" below.

`bytesrw.mli`, six hunks. Eighteen `val`s in `Slice`, `Reader` and `Writer`
take their subject at `t @ local`: `first`, `last`, `length`, `copy`,
`is_eod`, `equal`, `compare`, `to_bytes`, `to_bigbytes`, `to_string`,
`add_to_buffer`, `output_to_out_channel`, `Reader.pos`, `Reader.read_length`,
`Reader.slice_length`, `Writer.pos`, `Writer.slice_length` and
`Writer.written_length`. Eleven of them also gain `[@@zero_alloc]`, which makes
the compiler reject the file if the body ever starts allocating.

`bytesrw.ml`, seventeen hunks. The matching definitions take `@ local`, gain
`[@inline]` and `[@zero_alloc]`, and `make_local` is added with `exclave_` so a
slice can be built into the caller's region. Operations that have to touch the
bytes of a local slice go through `Base.Bytes`, whose `copy`, `sub`,
`to_string` and friends accept a local argument where stdlib's do not: that is
the whole reason for the `base` dependency. Two loops that closed over a `ref`
were rewritten with `let mutable`, since a closure capturing a local escapes
its region.

`bytesrw_fmt.ml` four hunks, `bytesrw_utf.ml` one, `bytesrw_hex.ml` two. Loop
rewrites of the same kind, plus an unboxed `char#` UTF-8 decode-length match in
`bytesrw_utf`. The three `.mli` files are untouched.

### Two behaviour changes, one of them now upstream

These are not mode annotations and they are the reason this copy cannot be
swapped for the installed one without thinking.

**`Slice.equal` and `Slice.compare`, fixed upstream in 0.4.0.** Upstream's
0.3.0 loop ran `while !cmp = 0 && !i < max` with `max = len - 1`, so for two
slices of equal length it never read the last byte, and `Slice.equal` answered
`true` for `"a"` and `"b"`. It was reported and 0.4.0 fixes it the same way
this copy did, by running the loop to `i <= max`. That is one of the two merge
conflicts: both sides had made the same fix, and this copy's version was kept
because it also carries the `let mutable` and `unsafe_get` rewrite the local
mode needs. `test_bytesrw` still checks all 65536 single-byte pairs and 256
two-byte pairs differing in the last byte, and now agrees with upstream rather
than diverging from it.

**`Bytes.Reader.of_slice` on a slice that does not start at 0**, upstream
issue 13, fixed in 0.4.0. This copy never had it. The OxCaml rewrite had
already inlined `read_bytes` into `of_bytes` and `of_slice` and threaded the
slice's own offset through, so the second merge conflict resolved to this copy
and upstream's `read_bytes` is not present here. It has no other caller.

**The `Slice` formatters**, still unfixed upstream. `bytesrw_fmt.ml` is
byte-identical between 0.3.0 and 0.4.0, so this remains the one behaviour
difference from the release. Upstream tests the head cut with `len - 1 > max`
and the empty case with `max < 0`, both of which forget that a slice may start
away from zero. A truncated slice with `first > 0` therefore printed without
the ellipsis that says it was truncated. The vendored copy compares against
`first + len - 1` and `first`. Of the five cases `test_bytesrw` pins, one
differs from upstream: a slice at `first = 8` of length 8 now shows its
ellipsis.

### Where the portable boundary falls

Nowhere. This copy carries no `@@ portable` annotation, and that was measured
rather than skipped. A floating `@@ portable` on `bytesrw.mli` yields four
blockers. Two are fixable: `Slice.make_or_eod` returns `t @ contended` because
it answers the module-level `eod` sentinel, which a three-line change to build
the empty slice per call and make `is_eod` structural resolves at the cost of
`eod` no longer being the unique empty slice; and `Slice.pp` needs
`bytesrw_fmt.mli` annotated too. Two are not. `Slice.tracer` defaults its `ppf`
to `Format.err_formatter`, which the OxCaml stdlib itself declares
`@@ nonportable`. A per-`val` `@@ nonportable` relaxes a floating annotation
only when the two are in the same signature, and `tracer` sits two modules
deep in `Bytes.Slice`, where the enclosing floating annotation reaches it
and the override is ignored. So the copy would need 88 per-`val`
annotations, or would have to stop publishing `tracer`.

`Stream.error` raises a module-level exception whose payload does not cross:
`error` is the extensible variant that lets the codecs add their own cases,
and `error_context` carries a `message : error -> string` closure.

None of that is worth doing on its own, because the thing it would serve,
lifting `Arod_env.t`'s jsont-bound closures, is blocked in jsont regardless.
`TODO.md` carries the full transcript.

### Tests

`avsm/arod/test/test_bytesrw.ml`, 17 checks plus 65792 exhaustive pairs, is the
guard. It lives outside `vendor/` because the root `dune` declares
`(vendored_dirs vendor)` and dune skips aliases there, so a `runtest` alias
under this directory would never run.

It binds slices with `local_` and passes them to the annotated accessors, so it
fails to compile if a re-vendor drops the `@ local` hunks: a local value is
refused where a global one is expected. It pins both behaviour changes above
against oracles outside bytesrw, which is what makes them evidence rather than
a restatement of the code.

The consumers are the behavioural gate: `test_json` drives jsont, which drives
this copy, and pins the bytes of every JSON response arod serves.

### Re-vendoring checklist

1. Copy `src/bytesrw.{ml,mli}`, `src/bytesrw_fmt.{ml,mli}`,
   `src/bytesrw_utf.{ml,mli}` and `src/bytesrw_hex.{ml,mli}` from the new
   release over this directory, keeping `dune`, `dune-project`,
   `bytesrw.opam`, `LICENSE.md` and this file.
2. Re-apply the thirty hunks above. Upstream carries none of them, so a
   straight copy loses the local annotations, the `base` dependency and both
   fixes.
3. Check whether upstream has fixed `Slice.equal` and the formatters. If it
   has, drop those hunks and keep `test_bytesrw`, which will then be pinning
   upstream's behaviour rather than a local divergence.
4. Update the version in `bytesrw.opam` and in the first line of this file.
5. Rebuild every consumer, since this copy shadows the installed package for
   all of them: `dune build @avsm/arod/all @avsm/arod/runtest
   @avsm/sortal/all @bleeding/karakeep/all @bleeding/immich/all
   @bleeding/webfinger/all @bleeding/apubt/all @bleeding/yamlt/all
   @bleeding/jsonwt/all @bleeding/cbort/all @bleeding/tomlt/all
   @bleeding/bytesrw-eio/all`. Do not add `@vendor/bytesrw/all`. The root
   `dune` declares `(vendored_dirs vendor)`, so dune skips aliases there: such
   a build exits 0 having compiled nothing, which reads as a pass and is not
   one.
6. `test_bytesrw` pins the annotations and both fixes.

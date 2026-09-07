## Duration - conversions to various time units

This is duration 0.3.1, vendored from https://github.com/hannesm/duration and
patched for OxCaml portability.

The copy was taken from `~/.opam/default/.opam-switch/sources/duration.0.3.1`,
the source tree opam unpacked for the `duration.0.3.1` package. `duration.ml`,
`duration.opam`, `dune`, `dune-project`, `tests.ml`, `CHANGES.md` and
`LICENSE.md` are byte-identical to it. The upstream `.gitignore` and
`.travis.yml` were dropped. This file and `duration.mli` are the only things
that have moved.

A duration is represented in nanoseconds as an unsigned 64 bit integer. This
has a range of up to 584 years. Functions provided check the input and raise
on negative or out of bound input.

### The patch

One hunk, in `duration.mli`, between the header comment and `type t`:

    @@ portable

A floating `@@ portable` applies to the declarations after it, so placing it
before the first `type` covers every value the module exports. Nothing else
was needed. `duration.ml` compiles unchanged against the stricter interface,
because every value it defines is a top-level function over `int`, `int64`,
`float` and `string`, and the two that hold mutable state, `split_on` and
`of_string_exn`, allocate their `ref` and their `Array.make` inside the
function body rather than capturing one.

`type t = int64` needs no kind annotation. It is a transparent alias, so it
carries the kind of `int64` and crosses portability and contention on its own.
An abstract `t` would have needed one.

`Format.formatter` appears in `val pp`, and that is not a problem, because it
is a parameter rather than something the closure captures. A formatter held in
a record field would have been.

### Why the copy exists

The switch has `duration.0.3.1` installed, but its interface carries no mode
annotations, so a `Duration.t` timeout cannot be read from the portable
closures the serving path is built out of. Nothing in the switch requires
`duration` at a version this copy would shadow badly: it has no dependencies
beyond `ocaml` and `dune`, and the in-tree consumers, `avsm/arod`,
`bleeding/httpz`, `bleeding/fetch`, `bleeding/proffer` and `bleeding/apubt`,
all name the library `duration` and link unchanged.

### Behaviour identity

`duration.ml` is byte-identical to upstream 0.3.1, so there is no behavioural
difference to establish. `diff` against the opam source tree is the whole
proof, and the re-vendoring checklist repeats it.

`avsm/arod/test/test_duration.ml` is the standing guard. It pins the scale of
every constructor, the truncation direction of the projections, the four `pp`
forms and the microsecond spelling, the parser including the repeated-metric
rejection and both microsecond signs, and the bounds checks. It bites: with
the `@@ portable` hunk reverted it fails to compile at its first ascription,
`Duration.of_sec` is `nonportable` but expected `portable`.

### Re-vendoring checklist

1. Copy `duration.ml`, `duration.mli`, `duration.opam`, `dune`,
   `dune-project`, `tests.ml`, `CHANGES.md` and `LICENSE.md` from the new
   release over this directory, keeping this file.
2. Reapply the hunk above.
3. Update the version in the first line of this file, and confirm
   `duration.ml` is still identical to the new release:

       diff <upstream>/duration.ml vendor/duration/duration.ml

   If it is not, the note under **Behaviour identity** no longer holds and a
   differential against the pristine sources is owed before relying on it.
4. Dune skips aliases under a vendored directory, so `dune runtest` does not
   reach anything here, and the upstream `tests.ml` never runs. Verify through
   the consumer aliases instead:

       dune build @avsm/arod/all @avsm/arod/runtest --force
       dune build @bleeding/httpz/all @bleeding/fetch/all
       dune build @bleeding/proffer/all @bleeding/apubt/all

   `avsm/arod/test/test_duration.ml` is the gate. It fails if the annotation
   is dropped, and it fails if a release moves a conversion or a `pp` form.

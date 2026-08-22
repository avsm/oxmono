## jsont - declarative JSON manipulation for OCaml

This is jsont 0.2.0, vendored unpatched from
https://erratique.ch/software/jsont.

`jsont.ml`, `jsont.mli`, `jsont_base.ml`, `jsont_base.mli` and the two files
under `bytesrw/` are byte identical to the ones opam installed. Only `dune`,
`dune-project` and `jsont.opam` are written for this workspace.

The copy is here to keep the link consistent. `vendor/bytesrw` shadows the
installed bytesrw for the whole workspace, and two libraries named `bytesrw`
cannot go into one executable, so every workspace dependency of bytesrw has to
be built from source alongside it. Of the packages installed in the switch,
only `jsont` and `jsonfeed` require bytesrw. `jsonfeed` was already vendored.
This is the other one.

`jsont.brr` is not vendored. It needs `brr`, and nothing in this workspace uses
it.

Nothing here is annotated for portability, and the annotation was tried and
ruled out rather than skipped. `TODO.md` carries the transcript under **jsont
and bytesrw**. The short version is that `Jsont.t` cannot be given a kind:
its representation is a tree of records whose `dec` and `enc` fields hold the
caller's own closures, so declaring `type 'a t : value mod portable contended`
reports the actual kind as `immutable_data with 'a Repr.any_map with 'a t
lazy_t with ('a, 'a) Object.map with ...` and the compiler gives up
simplifying it. Making it cross would mean requiring every caller's `~enc` to
be portable, which is a change to what jsont is, not an annotation of it.

### What differs from the upstream distribution

* `dune`, `bytesrw/dune` and `dune-project` are written for this workspace.
  Upstream builds with ocamlbuild and topkg and keeps the sources under `src/`.
* `jsont.opam` is the installed file. Upstream generates it.
* Upstream's `CHANGES.md`, `README.md`, `doc/`, `test/`, `attic/`, `paper/` and
  the `brr` codec are not vendored. The release this copy came from is named in
  `jsont.opam` and here.

### Tests

`avsm/arod/test/test_json.ml` drives this copy through `Arod_json.encode` and
pins the bytes of every JSON response arod serves, including the escaping rule.
That is the behavioural gate: an unpatched copy has no annotation to guard, so
there is nothing here of the kind `vendor/xmlm` and `vendor/bytesrw` need.

### Re-vendoring checklist

1. Copy `src/jsont.ml`, `src/jsont.mli`, `src/jsont_base.ml`,
   `src/jsont_base.mli` and `src/bytesrw/jsont_bytesrw.{ml,mli}` from the new
   release over this directory, keeping `dune`, `bytesrw/dune`, `dune-project`,
   `jsont.opam`, `LICENSE.md` and this file.
2. Update the version in `jsont.opam` and in the first line of this file.
3. There is no patch to re-apply. Confirm that with a diff against the release
   sources, which should be empty for all six files.
4. `dune build @avsm/arod/all @avsm/arod/runtest`, and the consumers under
   `bleeding/`: karakeep, immich, webfinger, apubt, yamlt, jsonwt and tomlt all
   reach jsont. Do not add `@vendor/jsont/all`. The root `dune` declares
   `(vendored_dirs vendor)`, so dune skips aliases there: such a build exits 0
   having compiled nothing, which reads as a pass and is not one.
5. `test_json` pins the response bytes.

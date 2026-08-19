## xmlm - streaming XML codec for OCaml

This is xmlm 1.4.0, vendored from https://erratique.ch/software/xmlm. Nothing
is patched. `xmlm.ml` and `xmlm.mli` are byte identical to the ones in the
1.4.0 release tarball, which are also the ones opam installed.

The copy exists as a prerequisite, not as a fix. It is needed for two reasons.

`avsm/sortal/lib/feed` and `avsm/sortal/test` call `Xmlm` directly, so the
workspace has a first-party caller whose portability depends on this
interface, independently of anything else.

Syndic's interface is written in terms of `Xmlm.pos`, `Xmlm.input`,
`Xmlm.dest`, `Xmlm.tag`, `Xmlm.attribute` and `Xmlm.name`, so a portable
syndic would need a portable xmlm first. Syndic itself is deliberately not
annotated and must not be, for reasons `vendor/syndic/README.md` records, and
xmlm is only one of the things standing in its way. A vendored `public_name`
is what would let an annotation land here at all.

Annotating is a separate pass, so that its diff is exactly the patch and this
import is exactly the release.

### What differs from the upstream distribution

* `dune` and `dune-project` are written for this workspace. Upstream builds
  with ocamlbuild and topkg.
* `xmlm.opam` is the upstream file with the `ocamlfind`, `ocamlbuild` and
  `topkg` build dependencies replaced by `dune`, and with a paragraph in the
  description recording why the copy exists.
* Upstream's `CHANGES.md`, `README.md`, `doc/` and `test/` are not vendored.
  The release this copy came from is named in `xmlm.opam` and here.

### The sitemap side effect

A vendored `public_name` shadows the installed package for the whole
workspace, and two libraries named `xmlm` cannot be linked into one
executable. Every workspace dependency of xmlm therefore has to be built from
source alongside it. Of the installed packages whose META requires xmlm,
`syndic` was already vendored and `sitemap` was not. `vendor/sitemap` is that
copy, taken unpatched from sitemap v1.0 and present only to keep the link
consistent.

### Re-vendoring checklist

1. Copy `src/xmlm.ml` and `src/xmlm.mli` from the new release over this
   directory, keeping `dune`, `dune-project`, `xmlm.opam`, `LICENSE.md` and
   this file.
2. Update the version in `xmlm.opam` and in the first line of this file.
3. Rebuild every consumer, since this copy shadows the installed package for
   all of them: `dune build @avsm/arod/all @avsm/arod/runtest
   @avsm/sortal/all @avsm/sortal/runtest`. Arod reaches this copy through
   `vendor/sitemap`, sortal through `vendor/syndic` and through its own direct
   dependency. Do not add `@vendor/xmlm/all` or any other alias under
   `vendor/`. The root `dune` declares `(vendored_dirs vendor)`, so dune skips
   aliases there: such a build exits 0 having compiled nothing, which reads as
   a pass and is not one.

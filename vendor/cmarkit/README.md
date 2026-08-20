## cmarkit - CommonMark parser and renderer for OCaml

This is cmarkit 0.3.0, vendored from https://erratique.ch/software/cmarkit.
The nine sources under this directory are byte-identical to the release, and
will stay that way until the portability pass lands. Every patch that pass
makes will be recorded below, hunk by hunk.

The copy was taken from `~/.opam/5.2.0+ox/lib/cmarkit`, which the switch built
from `cmarkit.0.3.0+ox`. That package applies one patch to upstream, in
`tool/cmd_latex.ml`, and the tool is not vendored. Every vendored file
therefore matches both the installed copy and the upstream tarball, which was
checked file by file with `cmp`.

### What differs from the upstream distribution

* `cmarkit.opam` is the upstream file with the `ocamlfind`, `ocamlbuild` and
  `topkg` build dependencies replaced by `dune`, since the workspace builds
  this copy directly. The `cmdliner` depopt and the `build:` stanza are
  dropped with the command line tool they served.
* `dune` and `dune-project` are written for this workspace rather than taken
  from upstream.
* Upstream builds one library whose modules are all top level and publishes
  five of them, `Cmarkit`, `Cmarkit_renderer`, `Cmarkit_commonmark`,
  `Cmarkit_html` and `Cmarkit_latex`, through topkg's `~api` list. Dune cannot
  publish some modules of an unwrapped library and hide the rest, so all nine
  are visible here. That is a superset of the upstream interface and no
  in-tree module names collide with the four now exposed.
* `tool/`, upstream's `cmarkit` command line program, is not vendored.
  Nothing in the workspace runs it, and it is the only thing that wanted
  `cmdliner`.
* `test/`, `doc/`, `support/`, `B0.ml` and upstream's `CHANGES.md` and
  `README.md` are not vendored. The release this copy came from is named in
  `cmarkit.opam` and here.

### Why the copy exists

Nothing in the switch requires cmarkit. A sweep of every `META` under
`~/.opam/5.2.0+ox/lib` for a `cmarkit` requirement matched only cmarkit's own,
so shadowing the installed package with this one pulls no other package into
the workspace build. The four in-tree consumers, `avsm/arod/lib`,
`avsm/arod/bin`, `avsm/bushel/lib` and `avsm/bushel/lib_web`, name the library
`cmarkit` and link unchanged.

### Re-vendoring checklist

1. Copy the nine `.ml` files, the seven `.mli` files and `LICENSE.md` from the
   new release over this directory, keeping `dune`, `dune-project`,
   `cmarkit.opam` and this file.
2. Reapply the hunks above.
3. Update the version in `cmarkit.opam` and in the first line of this file.
4. Dune skips aliases under a vendored directory, so `dune runtest` does not
   reach anything here. Verify through the consumer aliases instead:

       dune build @avsm/arod/all @avsm/arod/runtest --force
       dune build @avsm/bushel/all @avsm/sortal/all @avsm/sortal/runtest

   `avsm/arod/test/test_md_golden.ml` is the gate. It renders four documents
   through the three markdown renderers and the sidenote collector and
   byte-compares 34 results against checked-in files. A cmarkit change that
   alters one byte of output fails there. Never regenerate the goldens to make
   it pass.

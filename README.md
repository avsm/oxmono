# oxmono2

A slim OxCaml monorepo containing only bleeding-edge code: work-in-progress
libraries in [bleeding/](bleeding/) and deployed binaries in [avsm/](avsm/).
Most third-party dependencies are installed from opam via the
[oxcaml opam repository](https://github.com/oxcaml/opam-repository). A focused
set of portability-sensitive dependencies is kept under [vendor/](vendor/);
this remains much smaller than the predecessor `oxmono` repository, which
vendored roughly 400 packages under `opam/`.

## Setup

The Zarr and Tessera packages use system C libraries. On Debian or Ubuntu,
install them before creating the switch:

```sh
sudo apt install pkg-config libproj-dev proj-data libblosc-dev libzstd-dev
```

[PROJ](https://proj.org/) supplies both `proj.h` at build time and `proj.db`
at run time. The repository vendors its OCaml binding, not the C library.

```sh
opam switch create . --no-install \
  --repos ox=git+https://github.com/oxcaml/opam-repository.git,default \
  --packages oxcaml-compiler.5.2.0minus39
eval $(opam env)
opam install dune
# httpz needs the local eio fork (Eio.Path.open_subtree etc.)
for p in eio eio_main eio_posix eio_linux; do
  opam pin add -y $p.1.3+ox git+file://$HOME/src/git/avsm/eio#main
done
# mdx doc tests need the local mdx clone; the version must be 2.5.0+ox
# to satisfy oxcaml-mdx-patches
opam pin add -n -k git mdx.2.5.0+ox git+file://$HOME/src/git/realworldocaml/mdx#main
opam install -y mdx
opam install --deps-only ./avsm/*/*.opam ./bleeding/*/*.opam
dune build --profile=release avsm/sortal avsm/bushel avsm/arod
```

The primary deployment targets are the `sortal`, `bushel` and `arod`
executables.

## Vendored packages

A focused set of packages is vendored under [vendor/](vendor/) (declared via
`(vendored_dirs vendor)` in the root `dune` file) because the TLS/X.509
closure needs OxCaml mode annotations that are not all available from opam.
The closure currently includes Zarith 1.14+ox, Cstruct 6.2.0, Digestif 1.3.1,
Mirage Crypto 2.4.1, TLS 2.1.2, X.509 1.1.1, CA certificates 1.0.3, and their
smaller dependencies. The client-facing Eio resource/flow boundary contains no
`Obj.magic_portable`.

The source revision, path mapping, monorepo adaptations and update procedure
are recorded in
[HTTPZ_SYNC.md](HTTPZ_SYNC.md). The shared HTTP stack contract is in
[HTTPZ_RELEASE.md](HTTPZ_RELEASE.md).

These should be re-checked against the ox opam repository on each compiler
upgrade and dropped once upstream packages expose the required modes.

`base64` is vendored for a different reason: it installs from opam, but its
interface carries no mode annotations, so a `portable` function cannot call it.
The vendored copy is 3.5.2 with the alphabet tables made immutable and the
interface annotated. See [vendor/base64/README.md](vendor/base64/README.md).
It can be dropped once upstream is annotated.

`htmlit` is vendored for the same reason. The vendored copy is 0.2.0 with the
void element set replaced by a match, since a set cannot be read from a
portable function, and the interface annotated. See
[vendor/htmlit/README.md](vendor/htmlit/README.md).

`ptime` is vendored for the same reason. The vendored copy is 1.2.0 with the
three module-level tables the arithmetic indexed replaced by matches, since an
array cannot be read from a portable function, the timestamp and span types
given the `immutable_data` kind, and both interfaces annotated. The clock
sublibrary comes with it. See [vendor/ptime/README.md](vendor/ptime/README.md).

`jsonfeed` is vendored as a consequence of that, and is unpatched. A vendored
`public_name` shadows the installed package for the whole workspace, and two
libraries named `ptime` cannot be linked into one executable, so every
workspace dependency of ptime has to be built from source alongside it. See
[vendor/jsonfeed/README.md](vendor/jsonfeed/README.md).

`xmlm` is vendored for the same reason. The vendored copy is 1.4.0 with the
UTF-8 byte length array and the predefined entity table replaced by code,
since neither an array nor a hash table can be read from a portable function,
and the interface annotated. Sortal's feed library calls `Xmlm` directly, and
syndic's interface is written in terms of `Xmlm.pos`, `Xmlm.input` and
`Xmlm.tag`, so a portable syndic needs a portable xmlm first. See
[vendor/xmlm/README.md](vendor/xmlm/README.md).

`sitemap` is vendored as a consequence of that, and is unpatched, for the same
shadowing reason as jsonfeed. See
[vendor/sitemap/README.md](vendor/sitemap/README.md).

`syndic` is vendored for a third reason as well, to carry parse fixes for
feeds that publishers actually emit. It is now annotated too. Its published
types are built from `Uriz.t` and `Ptime.t`, both of which cross portability,
so a parsed feed can be held by a portable closure, and every one of its nine
interfaces carries `@@ portable`. Getting there needed modes on the two
functions its eighty-odd parsers are partial applications of, so that those
parsers stay portable module-level values, and four module-level tables turned
into code. See [vendor/syndic/README.md](vendor/syndic/README.md).

`mdx` from opam is likewise blocked on minus39, hence the local-clone pin
in the setup steps above.

- `ocaml-uri` — provides `uriz`, the OxCaml port of ocaml-uri used by
  httpz; not yet released to opam.

`avsm/httpz` tracks the standalone [avsm/httpz](https://github.com/avsm/httpz)
repository (libraries `httpz`, `httpz.route`, `httpz.eio_server`), which
replaced the older `httpz.server`/`httpz.eio` split. It requires the eio
fork pinned above.

# oxmono2

A slim OxCaml monorepo containing only bleeding-edge code: work-in-progress
libraries in [bleeding/](bleeding/) and deployed binaries in [avsm/](avsm/).
All third-party dependencies are installed from opam via the
[oxcaml opam repository](https://github.com/oxcaml/opam-repository) rather
than vendored (unlike the predecessor `oxmono` repository, which vendored
~400 packages under `opam/`).

## Setup

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

A small number of packages are vendored under [vendor/](vendor/) (declared
via `(vendored_dirs vendor)` in the root `dune` file) because they cannot
currently be installed from opam on `oxcaml-compiler.5.2.0minus39`:

- `Zarith` — the dune-ported OxCaml build; opam's `zarith.1.14+ox` conflicts
  with compilers newer than 5.2.0minus38, and `1.12+ox3` is too old for
  mirage-crypto 2.x.
- `mirage-crypto`, `tls`, `x509`, `ca-certs`, `asn1-combinators`, `kdf`,
  `gmap` — the modern (2.x/1.x, cstruct-free) TLS stack requires
  `zarith >= 1.13`, so it is transitively blocked from opam by the above.
- `digestif` — upstream 1.3.x fails to compile under minus39's stricter
  mode inference (in the pure-OCaml backend); the workspace build only
  demands the C backend, which is fine.

These should be re-checked against the ox opam repository on each compiler
upgrade and dropped once installable.

`base64` is vendored for a different reason: it installs from opam, but its
interface carries no mode annotations, so a `portable` function cannot call it.
The vendored copy is 3.5.2 with the alphabet tables made immutable and the
interface annotated. See [vendor/base64/README.md](vendor/base64/README.md).
It can be dropped once upstream is annotated.

`mdx` from opam is likewise blocked on minus39, hence the local-clone pin
in the setup steps above.

- `ocaml-uri` — provides `uriz`, the OxCaml port of ocaml-uri used by
  httpz; not yet released to opam.

`avsm/httpz` tracks the standalone [avsm/httpz](https://github.com/avsm/httpz)
repository (libraries `httpz`, `httpz.route`, `httpz.eio_server`), which
replaced the older `httpz.server`/`httpz.eio` split. It requires the eio
fork pinned above.

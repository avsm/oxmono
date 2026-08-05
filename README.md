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
opam install --deps-only ./avsm/*/*.opam ./bleeding/*/*.opam
dune build --profile=release avsm/sortal avsm/bushel avsm/arod
```

The primary deployment targets are the `sortal`, `bushel` and `arod`
executables.

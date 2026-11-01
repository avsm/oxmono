# nx-oxcaml

An experimental backend for [Nx](https://github.com/raven-ml/raven), the
n-dimensional array library of the Raven ecosystem, written in
[OxCaml](https://oxcaml.org), Jane Street's branch of the OCaml compiler.

It implements Nx's backend interface with OxCaml's unboxed types and SIMD
intrinsics:

- **Unboxed arithmetic**: arrays and loops over `float#`, `float32_u`,
  `int32_u`, `int64_u`, `int8#` and `int16#`, so element access allocates
  nothing.
- **SIMD kernels**: NEON on arm64 and SSE on amd64, chosen at build time.
- **Parallel execution**: large operations split across domains.

## Relation to Raven

nx-oxcaml was developed inside the Raven monorepo and moved here with its
history. Raven's development branch uses OCaml 5.5 syntax, and the newest
OxCaml compiler, `ocaml-variants.5.4.0+ox`, is based on OCaml 5.4, so the
backend cannot build against it.

Until OxCaml accepts OCaml 5.5 syntax, this repository tracks the released
`nx` 1.0.0~alpha3 from opam, which builds on OCaml 5.2 and later. Its backend
interface is the `nx.backend` virtual library: an executable that lists
`nx-oxcaml` among its libraries runs every Nx operation on this backend in
place of the default C backend.

```dune
(executable
 (name main)
 (libraries nx nx-oxcaml))
```

## Building

Create an OxCaml switch with the OxCaml opam repository (see
<https://oxcaml.org/get-oxcaml/>), install the dependencies, then build and
test:

```sh
opam switch create . ocaml-variants.5.4.0+ox \
  --repos ox=git+https://github.com/oxcaml/opam-repository.git,default \
  --no-install
eval $(opam env)
opam install . --deps-only --with-test
dune build
dune test
```

## Benchmarks

`bench/` times backend operations; [bench/README.md](bench/README.md) records
results against Nx's C backend. Linking `nx-oxcaml` replaces the C backend in
the whole executable, so the rows labelled `Nx (C)` also run on this backend.

```sh
dune exec bench/bench_nx_oxcaml.exe
```

## License

ISC, see [LICENSE](LICENSE).

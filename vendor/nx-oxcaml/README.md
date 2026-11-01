# nx-oxcaml

OxCaml numerical backend for [Nx](https://github.com/raven-ml/raven), imported
from [raven-ml/nx-oxcaml](https://github.com/raven-ml/nx-oxcaml) at
`e0800f150544990412e690a8ad881858424ac6a5`.
Upstream author: Thibaut Mattio and the Raven contributors.
See README.upstream.md for backend design and usage.

The library implements the `nx.backend` virtual library with unboxed arrays
and architecture-selected NEON or SSE kernels. Link `nx-oxcaml` alongside
`nx` or `nx.io` to select it for an executable. The compatible Nx release,
`1.0.0~alpha3`, and its bundled dependencies live in `../../vendor/nx`.

    dune build --profile release-check @vendor/nx-oxcaml/all
    dune runtest --force --profile release-check vendor/nx-oxcaml

## Workspace changes

- Unboxed scalar aliases in `lib/import.ml`, `simd_neon.ml` and
  `simd_sse.ml` use the compiler's `Int64_u.t`, `Int32_u.t` and
  `Float32_u.t` module names. This supports this workspace's `5.2.0+ox`
  compiler. The package metadata accepts that compiler instead of requiring
  upstream's exact `5.4.0+ox` switch.
- Upstream's library, C stubs and 963 backend checks are retained. Benchmarks
  and their optional ubench dependency are omitted.
- The backend tests remain in this project. The Nx NPY differential and
  streaming tests live in `vendor/nx/test/io`, alongside Nx's pristine
  reference source and C backend. This project's source archive no longer
  reads a sibling vendor checkout.

The upstream domain pool is retained. Creating an Nx context starts its
workers. Its global pool is not designed for simultaneous unrelated calls
from multiple domains. Tessera's CLI uses Nx's genarray I/O API
without creating an Nx context or starting this pool.

## Refreshing

Replace `lib/` and the upstream test from the selected commit, retain
licenses, reapply the scalar aliases, and build both architecture variants
on their target machines. Run the backend and I/O tests, then Tessera's
forced tests. Refresh the Nx release only with a compatible backend.

## License

ISC. SIMD modules also credit Jane Street's MIT-licensed ocaml_simd.

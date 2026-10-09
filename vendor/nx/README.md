# Vendored Nx

Nx `1.0.0~alpha3` from [raven-ml/raven](https://github.com/raven-ml/raven),
commit `f1c456f7161cfe3a57a9bd4083605175f72989b4`.
The release archive SHA-256 is
`96d35ce03dfbebd2313657273e24c2e2d20f9e6c7825b8518b69bd1d6ed5870f`.

## Import scope

The array frontend, buffer, core, effect interface, virtual backend, C backend
and tensor I/O are imported. Nx's bundled camlzip, stb image readers/writers
and pocketfft are retained. Other Raven packages, examples, documentation
builds and upstream test frameworks are omitted. See README.upstream.md for
Nx usage. Tessera selects the separately imported `nx-oxcaml` backend.

Nx is ISC licensed. The NPY implementation credits Laurent Mazare's
ocaml-npy and is Apache-2.0 licensed, with its license in LICENSE.npy.
Bundled camlzip uses LGPL-2.1 with a linking exception. Pocketfft uses BSD-3.
The stb sources retain their license notices.

## Local patches

- Build metadata isolates Nx from the Raven monorepo and omits its mdx
  documentation stanza. Library source is otherwise preserved except below.
- `lib/buffer/nx_buffer_stubs.c` switches on integer layout flags rather than
  casting to an enum containing a non-layout sentinel. This removes a
  compiler warning without changing either layout branch.
- `lib/io/npy.ml` accepts an explicit header alignment and checks the version
  1.0 length limit. Its existing file writer retains 16-byte alignment.
- `lib/io/nx_io.ml` and `.mli` expose `npy_header` and
  `write_npy_genarray`. These use the upstream encoder and buffer conversion,
  support 64-byte NumPy alignment, and stream through a caller's callback
  in at most 64 KiB fragments. They avoid Unix I/O and whole-array copies.

The pristine `lib/io/npy.ml` is retained as `upstream/npy.ml` for differential
checks. No local NPY parser or dtype encoding is duplicated in Tessera.

## Re-vendoring

1. Use the Nx release required by nx-oxcaml. Replace `lib/` and `vendor/`
   from that release and retain its licenses and provenance.
2. Reapply the isolated build metadata, layout-switch and NPY API patches.
   Refresh `upstream/npy.ml` from the pristine release.
3. Build `vendor/nx/lib/backend_c/nx_c.cmxa`, `@bleeding/nx-oxcaml/all` and
   `@bleeding/tessera/all` with the workspace OxCaml compiler.
4. Force tests under `bleeding/nx-oxcaml` and `bleeding/tessera`.
   They exercise the backend, compare streamed NPY bytes with the pristine
   writer, read streamed files through Nx, and preserve Tessera's NumPy
   golden bytes. Scalar headers are compared separately because the
   pristine file writer rejects rank-zero arrays in Unix.map_file.

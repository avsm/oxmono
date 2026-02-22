(lang dune 3.0)

(name zarr)

(version 0.1.0)

(generate_opam_files true)

(source
 (github mtelvers/ocaml-zarr))

(authors "Mark Elvers <mark.elvers@tunbury.org>")

(maintainers "Mark Elvers <mark.elvers@tunbury.org>")

(license MIT)

(package
 (name zarr)
 (synopsis "Pure OCaml implementation of Zarr v3")
 (description
  "A complete implementation of the Zarr v3.1 specification for chunked, compressed N-dimensional arrays. This core library provides types, codecs, and metadata handling with no I/O dependencies.")
 (depends
  (ocaml (>= 5.1))
  (dune (>= 3.0))
  (yojson (>= 2.0))
  (decompress (>= 1.5))
  (checkseum (>= 0.5))
  (optint (>= 0.3))
  (zstd (>= 0.4))
  (alcotest :with-test)
  (qcheck :with-test)
  (qcheck-alcotest :with-test)))

(package
 (name zarr-sync)
 (synopsis "Synchronous store implementations for Zarr")
 (description
  "Synchronous (blocking) store implementations for Zarr v3, including memory and filesystem stores using Unix I/O.")
 (depends
  (ocaml (>= 5.1))
  (dune (>= 3.0))
  (zarr (= :version))
  (alcotest :with-test)))

(package
 (name zarr-eio)
 (synopsis "Eio-based async store implementations for Zarr")
 (description
  "Effect-based async store implementations for Zarr v3, using Eio for non-blocking I/O on OCaml 5.x.")
 (depends
  (ocaml (>= 5.1))
  (dune (>= 3.0))
  (zarr (= :version))
  (eio (>= 1.0))
  (eio_main (>= 1.0))
  (alcotest :with-test)))

(package
 (name zarr-blosc)
 (synopsis "Blosc codec for Zarr - high-performance meta-compressor")
 (description
  "Optional Blosc codec support for the Zarr library. Provides high-performance compression with shuffling using the c-blosc library. Supports multiple internal compressors: lz4, lz4hc, blosclz, zstd, snappy, zlib.")
 (depends
  (ocaml (>= 5.1))
  (dune (>= 3.0))
  (zarr (= :version))
  (conf-blosc :build)))

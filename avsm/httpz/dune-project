(lang dune 3.20)
(name httpz)

(generate_opam_files true)

(source (github avsm/httpz))
(license ISC)
(authors "Anil Madhavapeddy")
(maintainers "anil@recoil.org")

(package
 (name httpz)
 (synopsis "Zero-allocation HTTP/1.1 parser for OxCaml")
 (description
  "A high-performance HTTP/1.1 parser and serializer that achieves near-zero
heap allocations using OxCaml's unboxed types and local allocations. Includes
an Async-based static file server with zero-copy bigstring I/O.")
 (depends
  (ocaml (>= 5.2))
  base
  base_bigstring
  (core (>= v0.17))
  (core_unix (>= v0.17))
  (async (>= v0.17))
  (async_unix (>= v0.17))
  (core_bench :with-test)
  (eio :with-test)
  (eio_main :with-test)
  (ppx_jane :with-test)))

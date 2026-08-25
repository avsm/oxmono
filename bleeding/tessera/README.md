# tessera

OCaml client for [Tessera](https://geotessera.org) geospatial embeddings
over the Zarr V3 store, built on zarrz. See DESIGN.md. Projections use
the vendored geocaml/ocaml-proj over the system PROJ.

    dune build @bleeding/tessera/all @bleeding/tessera/runtest

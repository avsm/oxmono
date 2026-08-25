# tessera

OCaml client for [Tessera](https://geotessera.org) geospatial embeddings
over the Zarr V3 store, built on zarrz. See DESIGN.md. Projections use
the vendored geocaml/ocaml-proj over the system PROJ.

Queries take a WGS84 longitude and latitude in that order. Pixels come
back on the store's own UTM grid, never resampled, under the northern
EPSG code of the zone in both hemispheres, so a southern point has a
negative northing.

The `tessera` command reads the public store over HTTP by default. Put
`--` before a negative coordinate, which the parser would otherwise take
for an option name.

    tessera info
    tessera probe --year 2024 -- -3.44 56.19
    tessera patch 0.0918 52.2109 --size 32 --year 2024 -o p.npy

    dune build @bleeding/tessera/all @bleeding/tessera/runtest

# Geoembeddings convention fixtures

Data for the `zarrz.geoemb` tests. Nothing here is generated, every file
is copied verbatim from its source.

## Convention repository

- Source: <https://github.com/geo-embeddings/embeddings-zarr-convention>.
- Commit: `0655212938f36351245dbd3e5e8868f811d43663`. The repository has
  no `v1` tag yet, although the schema and the specification both cite
  URLs under `refs/tags/v1`.
- Fetched on 2026-08-24.

Files:

- `schema.json`, the JSON Schema of the convention, for reference. The
  codecs in `lib_geoemb` follow its `required`, `enum`, `oneOf` and
  numeric bounds.
- `aef_example.json`, `clay_example.json`, `tessera_example.json`, the
  three examples of `examples/` in that repository. Each is a whole
  group `zarr.json`, so the convention object is under `attributes`.

## Live store

- Source: the root `zarr.json` of the TESSERA v1 store at
  <https://data.source.coop/tessera/tessera/zarr/v1>.
- Fetched on 2026-08-24.

Files:

- `tessera_attributes.json`, the `attributes` object of that document,
  extracted whole. It is the bare attributes object, not a `zarr.json`.
  It carries a `geoemb:stretch` member that the schema does not define,
  which is why the codec keeps unknown members.

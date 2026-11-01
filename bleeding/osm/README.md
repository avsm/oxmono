# osm

Typed OCaml interfaces to Overpass and Nominatim, using Jsont, Fetch and Eio.
The source requires an OxCaml toolchain compatible with these dependencies.

Read the [package guide](doc/index.mld) for standalone setup, typed query
examples, result traversal, codecs and error handling. Public API documentation
starts at [Osm](lib/osm.mli).

| Module | Purpose |
| --- | --- |
| `Point`, `Bounds`, `Reference` | Validated coordinates, named bounds and object identities |
| `Tags`, `Address`, `Place` | Extensible tags and typed Nominatim results |
| `Element`, `Dataset` | OSM variants, indexed lookup and explicit missing references |
| `Query` | Composable element and count queries tied to their result codecs |
| `Nominatim`, `Overpass` | HTTP operations through a caller-supplied Fetch client |

Defaults use private services on `marpe:8888`, not public OSM infrastructure:

| Operation | Default endpoint |
| --- | --- |
| `Overpass.run`, `query`, `query_raw` | `http://marpe:8888/overpass/api/interpreter` |
| `Overpass.status` | `http://marpe:8888/overpass/api/status` |
| `Nominatim.search`, `search_query` | `http://marpe:8888/nominatim/search` |
| `Nominatim.reverse`, `reverse_at` | `http://marpe:8888/nominatim/reverse` |

Override base URLs with `Osm.v ~overpass_url ~nominatim_url`. Responses default
to a 16 MiB limit. Set client deadlines, credentials, retries and rate limits
on the caller's Eio/Fetch stack. A query's server timeout is separate from an
HTTP deadline.

## Verification in this workspace

```sh
opam exec --switch=5.2.0+ox -- dune build @bleeding/osm/all
opam exec --switch=5.2.0+ox -- dune runtest --force bleeding/osm
opam exec --switch=5.2.0+ox -- dune build @bleeding/osm/fmt
opam exec --switch=5.2.0+ox -- dune build \
  _doc/_html/osm/index.html _doc/_html/osm/Osm/index.html
```

Open `_build/default/_doc/_html/osm/index.html` for the generated guide and API.
In a standalone checkout, `dune build @doc` builds the package documentation.
Tests use fixtures and a mock HTTP backend. The optional live example contacts
all four services and tests generated queries:

```sh
opam exec --switch=5.2.0+ox -- dune exec bleeding/osm/examples/smoke.exe
```

Its area query requires Overpass's area database and dispatcher to be running.

The interface draws on [Overpy](https://python-overpy.readthedocs.io/en/latest/api.html)
and [OSMPythonTools](https://wiki.openstreetmap.org/wiki/OSMPythonTools).
It uses immutable values, variants and explicit I/O. Raw queries cover QL
outside the builder. Plotting, caching, OSM editing and XML object models are
outside this library.

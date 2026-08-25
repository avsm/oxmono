# tessera design

An OCaml client for Tessera geospatial embeddings, covering the whole read
lifecycle over the Zarr V3 store: store discovery, UTM zone routing,
coordinate projection, point probing, region and patch reads, and
dequantisation. It is a port of the Zarr read path of the Python
`geotessera` package (`GeoTesseraZarr` and `TesseraAccessor` in
`../geotessera/geotessera/store.py`, relative to the monorepo parent),
built on `zarrz`. The live store is
`https://data.source.coop/tessera/tessera/zarr/v1`.

Out of scope, deliberately: the legacy npy and parquet manifest path,
store writing, GeoTIFF export, stretch statistics computation and the
`global_rgb` preview builder. The Python package remains the reference
for those.

## The store, as the reader sees it

One Zarr V3 group per UTM zone, `utm01` to `utm60`. Per zone:

- `embeddings`: int8, shape `(T, 128, H, W)`, dims `(time, band, y, x)`,
  shards `(1, 128, 4096, 4096)`, inner chunks `(1, 128, 32, 32)`,
  blosc zstd inside `sharding_indexed`, fill 0.
- `scales`: float32, shape `(T, H, W)`, same shard geometry, fill `+inf`.
  Scale sentinels: `NaN` is water or in-tile nodata, `+inf` is never
  written (an untouched region reads as the fill value), finite and
  positive and below 1.0 is real data. Dequantised value is
  `float32 embedding * scale`, and any non-finite scale dequantises to
  a NaN row.
- `time`: int32 `(T,)` holding years by value (2017..2025 today).
  Lookup is by value, never by index.
- `x`, `y`, `band`: coordinate arrays. The reader never fetches `x` or
  `y`: they are affine derived, `x_i = ox + (i + 0.5) * px` and
  `y_j = oy - (j + 0.5) * px`, so index arithmetic on the group's
  `spatial:transform` is exact.

Zone group attributes: `proj:code` is always the northern UTM code
`EPSG:{32600 + zone}` for both hemispheres, with southern data at
negative northings on a continuous axis. `spatial:transform` is the
six-element GDAL affine `[px, 0, ox, 0, -px, oy]` with `px = 10.0`.

The root group carries the geo-embeddings convention (parsed with
`zarrz.geoemb`) and zarr-python consolidated metadata under a
`consolidated_metadata` member with `must_understand: false`.

## Packages

`bleeding/tessera`, one package `tessera`, standard house skeleton
(cbort template, AI-disclosure opam template, CI file, CHANGES.md).

- `lib/` -> library `tessera`. Depends on `zarrz`, `zarrz.geoemb`,
  `jsont`, `base_bigstring`. Store agnostic: everything takes a
  `Zarrz.Store.t`, so the same code runs over HTTP, a local directory
  or a memory store in tests.
- `bin/` -> public executable `tessera` (cmdliner). Depends on
  `tessera`, `zarrz-fetch`, `zarrz-eio`, `fetch-curl`, `eio_main`.
- `test/` -> per-stage test dirs as in zarrz.

## Modules

### Affine

GDAL-order 2-D affine `{ a; b; c; d; e; f }` mapping pixel to world as
`x = c + a * col + b * row`, `y = f + d * col + e * row`.
`of_spatial : float array -> t` reads the six-element
`spatial:transform`. `apply`, `invert : t -> t` (raises on a singular
transform), and the pixel-centre helpers used everywhere:
`col_of_x t x = (x - c) / a - 0.5` and its row twin, plus
`x_of_col t col = c + (col + 0.5) * a`. Rotation terms `b` and `d` are
carried but the store always writes them 0.

### Crs

Projections come from the vendored `geocaml/ocaml-proj` bindings
(`vendor/ocaml-proj`, virtual `proj` implemented by `proj.c` over the
system PROJ 9 with `/usr/share/proj/proj.db`). `Crs` wraps them with
the two shapes this library needs:

```ocaml
type t
val utm_north : zone:int -> t        (* "EPSG:{32600 + zone}" *)
val patch : lon:float -> lat:float -> t
    (* "+proj=tmerc +lat_0=0 +lon_0={lon} +k=0.9996 +x_0=500000
       +y_0={0 north, 1e7 south} +datum=WGS84 +units=m +no_defs",
       exactly Python's _patch_crs *)
val forward : t -> lon:float -> lat:float -> float * float  (* e, n *)
val inverse : t -> e:float -> n:float -> float * float      (* lon, lat *)
val name : t -> string               (* the EPSG code or proj string *)
```

Every transformation is built from `"EPSG:4326"` and passed through
`Proj.Transformation.normalize_for_visualization`, the equivalent of
pyproj's `always_xy`, so coordinates are always lon and lat order.
Transformations are memoised per `t` inside the value. PROJ objects
are not thread safe, so a `t` belongs to one domain, the same caveat
pyproj carries.

Golden vectors are generated with pyproj 3.7.2 from
`../geotessera/.venv/bin/python` and checked in as a table with the
generator script. Agreement bound 1e-6 m on forward, 1e-9 degrees on
inverse. These tests double as the guard tests for the vendored copy,
which the vendoring rules require to live outside `vendor/`.

### Zone

Pure routing arithmetic, matching `store.py` exactly:

- `for_lon lon = clamp 1 60 (floor ((lon +. 180.) /. 6.) + 1)`.
  No Norway or Svalbard exceptions. Verified: the Python package has
  none either.
- `canonical_epsg z = 32600 + z`. `centre_lon z = -180. + (z - 0.5) * 6`.
- `seam_neighbours lon`: the neighbouring zone (wrapping 1 and 60) when
  `(lon + 180.) mod 6.` is within 0.1 degrees of a seam, else `[]`.
- `spanned lons ~centre_lon`: the contiguous run of zones covering the
  longitudes, walked the short way round the ring so an antimeridian
  patch yields `[60; 1]`.

### Tessera (the store handle)

```ocaml
type t
val of_store : Zarrz.Store.t -> t
val url : string                     (* the default public store URL *)
val geoemb : t -> Zarrz_geoemb.t
val years : t -> int list            (* time values of the first zone *)
val zones : t -> int list            (* zones present in the store *)
val zone : t -> int -> Dataset.t     (* cached per instance *)
```

`of_store` reads the root `zarr.json` once. When the group carries
zarr-python consolidated metadata (an unknown member of the parsed
group, `kind` `"inline"`), the node map is decoded and kept: `zones`
comes from it directly, and zone datasets are built from the inlined
array metadata with `Zarrz.Arr.of_json`, so opening the store costs one
HTTP request and opening a zone costs none. Without consolidated
metadata, `zones` probes lazily and zone opens fetch per-node
`zarr.json`. `years` reads the first present zone's `time` array (one
tiny chunk).

### Dataset (one zone)

```ocaml
type t
val zone : t -> int
val epsg : t -> int
val transform : t -> Affine.t
val shape : t -> int * int           (* H, W *)
val bands : t -> int
val years : t -> int list
val proj : t -> lon:float -> lat:float -> float * float
```

Reads go through an inner-chunk cache: decoded 32 by 32 tiles of
`scales` (4 KiB) and 128 by 32 by 32 blocks of `embeddings` (128 KiB),
keyed by `(array, time_index, tile_y, tile_x)`, in an LRU capped at 256
entries (about 33 MiB). Point workloads revisit tiles constantly and
the cache turns each revisit into zero requests. Region reads bypass
the cache and use `Zarrz.Arr.read` directly.

Point operations, exactly the Python algorithm
(`TesseraAccessor.probe`, `store.py:331`):

```ocaml
type status = Valid | Water | Nodata | Outside
val probe :
  t -> e:float -> n:float -> year:int -> ?search_px:int ->
  unit -> (float array option * status)
```

Column and row come from the affine index arithmetic. A residual above
one pixel in either axis is `Outside`. The `(2r+1)` squared scales
window decides: centre `NaN` is `Water` (never repaired), centre finite
is the pixel itself, else the nearest finite scale in the window by
squared pixel distance, `Nodata` when none. The winning pixel's
embedding column is read and multiplied by its scale into a fresh
`float array` of length `bands`. Default `search_px` 1.

`read_region : t -> e_range -> n_range -> year:int -> Region.t` reads
the enclosing pixel box, fetches `embeddings` and `scales` subsets, and
dequantises in one fused pass writing `(H, W, B)` C-order float32
directly (transpose from `(B, H, W)` and scale multiply combined, NaN
rows for non-finite scales).

```ocaml
module Region : sig
  type t = { data : Zarrz.Slab.t;   (* float32, shape [h; w; bands] *)
             transform : Affine.t; epsg : int }
end
```

### Top-level query API (on Tessera.t)

Mirrors `GeoTesseraZarr`:

- `probe t ~lon ~lat ~year ?cross_zone ?search_px ()`: routes to
  `Zone.for_lon`, then the seam neighbours when `cross_zone` (default
  true). First `Valid` wins. Status precedence `Water > Nodata >
  Outside`. A zone absent from the store is skipped.
- `sample t ~lon ~lat ~year ... : float array option` and
  `sample_points t coords ~year : float array option array` (points
  grouped by zone so the tile cache works; order preserved in the
  result).
- `read_region t ~bbox ~year`: the bbox centre's zone serves the whole
  request, as in Python (a spanning bbox logs nothing here, it is
  documented instead). Corners are projected into the zone and the
  enclosing box is read.
- `read_patch t ~lon ~lat ~year ~size_px`: exact `(size_px, size_px,
  B)` float32 centred on the point, the point in pixel
  `size_px / 2`.
  Single zone: sliced unresampled from the zone grid in the zone CRS,
  off-grid pixels NaN.
  Zone straddling (decided by projecting the densified patch outline
  and checking `Zone.spanned`): merged onto a patch-centred transverse
  Mercator grid (`Crs.patch`). For each contributing zone, read the
  covering region once, then for every destination pixel compute the
  source index by composing `Crs.inverse` of the patch CRS with the
  zone projection (nearest neighbour, matching Python's default
  `Resampling.nearest`), and composite by per-pixel zone ownership:
  the zone owning the pixel's longitude wins, any other zone only
  fills pixels the owner lacks. No rasterio: nearest reprojection is a
  per-pixel double transform and an index lookup.

### Npy

Minimal NumPy `.npy` version 1.0 writer for C-order float32 arrays, so
CLI output interoperates with the Python tooling byte for byte.
`val write : path:_ -> shape:int array -> Zarrz.Slab.t -> unit` style,
in `bin/` support code or `lib/` (implementer's call, `lib/` if the
tests want it).

### CLI (`tessera`)

cmdliner, following the ocaml-dev cmdliner conventions. A `--store`
option accepts an `https://` URL (fetch store via `Fetch_curl.std`) or
a local directory (`zarrz-eio` store), defaulting to the public URL.

- `tessera info`: model, build version, dimensions, years, zone count.
- `tessera probe LON LAT --year Y`: status and the first eight
  dequantised dimensions.
- `tessera region MINLON MINLAT MAXLON MAXLAT --year Y -o out.npy`:
  writes the region, prints shape, affine and EPSG.
- `tessera patch LON LAT --size N --year Y -o out.npy`.

## Testing

- `Zone` and seam and spanned functions: the exact expectation table
  from `../geotessera/tests/store_check.py` (seam neighbours at 3.0,
  138.03, 137.97, 138.2, 138.0, -179.97, 179.97; zones for 2.35 and
  -120.5; spanned for `[2.0;2.5]`, `[-0.01;0.01]`,
  `[179.97;-179.97]`).
- `Crs`: pyproj-generated golden vectors both hemispheres, zone
  centres and edges, the `patch` CRS identities
  (`forward (patch 0.5 52.0) 0.5 52.0` gives easting 500000, southern
  patch northing above 5e6), and forward and inverse round trips.
- Synthetic store goldens: build the `store_check.py` fake zone in a
  `Zarrz.Store.memory` (bands 4, every embedding `[1;2;3;4]`,
  EPSG:32653, transform `[10;0;300000;0;-10;4050000]`, scales
  patterns) and port its probe table verbatim: search radius 0 and 1
  and 2 cases, water never repaired, nearest finite scale wins, the
  10 km outside case, and the seam patch pair (zone 30 scale 0.05,
  zone 31 scale 0.07 at lon 0.0: shape, coverage, owner wins on
  overlap, neighbour fills slivers, centre pixel maps to the projected
  point within 1e-6).
- A live smoke test binary or alias kept out of `runtest`.

## Milestones

1. `Affine`, `Crs`, `Zone` with golden tests, over the vendored proj.
2. `Tessera`, `Dataset`, probe and sample and `read_region`, the
   consolidated metadata fast path, the tile cache, synthetic store
   tests.
3. `read_patch` (native and merged), `Npy`, the CLI, live smoke
   verification.

Strictly sequential: 2 builds on 1, 3 on 2.

# zarrz design

A Zarr V3 library for OxCaml. Reads and writes Zarr V3 hierarchies with
unboxed, zero-allocation access to decoded values. HTTP access goes through
`fetch`. The Rust `zarrs` workspace at `../zarrs` (relative to the monorepo
root's parent) is the behavioural oracle. Spec:
https://zarr-specs.readthedocs.io/en/latest/v3/core/.

## Scope

In scope for the first release:

- Zarr V3 metadata only. `zarr.json` for arrays and groups.
- Data types: `bool`, `int8..int64`, `uint8..uint64`, `float16`, `bfloat16`,
  `float32`, `float64`, `complex64`, `complex128`, `r*` (multiple of 8 bits).
- Codecs: `bytes` (endian), `transpose`, `gzip`, `zstd`, `crc32c`,
  `sharding_indexed`.
- Chunk grid: `regular`. Chunk key encodings: `default` and `v2`.
- Stores: memory, Eio filesystem, fetch HTTP (read only).
- Reading chunks, chunk subsets and array subsets. Writing whole chunks and
  whole shards, and creating arrays and groups, on writable stores.
- A conformance CLI compatible with `zarrs_conformance`.

Deferred, with representation slots reserved where noted: `blosc` (user
decision), variable-length `string`/`bytes` and the vlen codecs (the decoded
value type has a `Variable` slot), Zarr V2 metadata, storage transformers,
consolidated metadata, `numpy.datetime64`/`timedelta64`, partial shard
append on write.

## Packages

Two new top-level projects, each its own dune-project per house convention
(see `bleeding/cbort` for the template: ISC, `generate_opam_files`,
`(source (tangled anil.recoil.org/ocaml-<name>))`, `.tangled` CI copied from
cbort, `.ocamlformat` 0.28.1, CHANGES.md with `## Unreleased`, opam template
carrying the AI disclosure fields as in `bleeding/cookeio`).

### bleeding/zstdz

Fresh C bindings to libzstd (system 1.5.7, `conf-zstd` in depends).
Single library `zstdz` in `lib/` with `zstdz_stubs.c`. Independent of Zarr.

### bleeding/zarrz

- `lib/` -> `zarrz` (package `zarrz`). Pure core: no unix, no eio. Depends
  on `jsont`, `base_bigstring`, `decompress` (`decompress.de`,
  `decompress.gz`), `checkseum`, `zstdz`, `stdlib_stable`.
- `lib_eio/` -> package `zarrz-eio`: filesystem store over `Eio.Path`, plus
  the conformance executable in `conformance/`.
- `lib_fetch/` -> package `zarrz-fetch`: HTTP store over `Fetch`.
- `test/` per package stanza, alcotest. `test/fixtures/` holds golden data
  copied from the oracle (see Testing).

The core is a wrapped library. Public modules: `Error`, `Dtype`,
`Fill_value`, `Ext`, `Metadata`, `Chunk_grid`, `Chunk_key`, `Byte_range`,
`Store`, `Byte_source`, `Slab`, `Subset`, `Codec`, `Node`, `Group`, `Arr`.
The array module is `Arr`, not `Array`: the library is wrapped, so a
module named `Array` would shadow `Stdlib.Array` in every module of the
library and in any scope that opens `Zarrz`.
Every `.ml` has a matching `.mli` with the ISC banner.

## Core representation

### Buffers

All chunk bytes live in `Base_bigstring.t` (called bigstring below), as in
`httpz`. Bigstrings are off heap, blit with memcpy, are readable from C
stubs without copying, and admit unboxed scalar reads. Store reads land
directly in bigstrings. The codec pipeline is bigstring to bigstring.

### Dtype

```ocaml
type t =
  | Bool
  | Int8 | Int16 | Int32 | Int64
  | Uint8 | Uint16 | Uint32 | Uint64
  | Float16 | Bfloat16 | Float32 | Float64
  | Complex64 | Complex128
  | Raw of int          (* size in bytes, from r{8k} *)

val size : t -> int     (* bytes per element *)
val of_name : string -> t option   (* accepts "r24" etc, rejects r bits
                                      not a multiple of 8 *)
val name : t -> string
```

Everything is fixed size. In-memory element bytes are always native endian
after decode. `Uint64` shares representation with `Int64` (the accessor
returns `int64#` reinterpreted, as in Rust's u64/i64 transmute).

### Fill_value

`Fill_value.t` is an immutable `string` holding the native-endian bytes of
one element, exactly Rust's `FillValue(Vec<u8>)`. Parsing from JSON is per
dtype and must match the oracle
(`zarrs_metadata/src/v3/array/fill_value.rs`, `nan_representations.rs`):

- bool: JSON true/false.
- ints and uints: JSON integer, range checked per width.
- floats: JSON number, or `"Infinity"`, `"-Infinity"`, `"NaN"`, or a hex
  string `"0x…"` whose byte length exactly matches the type, parsed
  case-insensitively as big-endian bytes. `"NaN"` decodes to the canonical
  Zarr NaN: sign 0, exponent all ones, mantissa MSB 1, rest 0
  (`0x7fc00000` for float32, `0x7ff8000000000000` for float64, and the
  equivalent patterns for float16/bfloat16).
- complex: two-element JSON array, each element by the float rules.
- r*: JSON array of size-in-bytes integers in [0,255], length must match.
  Also accept a base64 string on input.

Encoding back to JSON reverses this: +inf and -inf and canonical NaN use
the strings, a non-canonical NaN becomes the lowercase hex string of its
big-endian bytes, finite floats become JSON numbers, r* becomes an array of
byte integers. These exact rules are also the conformance CLI's output
lexicon.

Known limitation, to be documented in the `.mli`: `jsont` represents JSON
numbers as floats, so integer fill values above 2^53 do not round-trip.

### Metadata

`jsont` codecs mirroring `ArrayMetadataV3`/`GroupMetadataV3`. Follow the
house `_jsont` naming (`bleeding/apubt` shows the style).

`Ext.t` is the extension-point object:

```ocaml
type t = { name : string; config : Jsont.json option; must_understand : bool }
```

Decode accepts either a bare JSON string or an object with `name`,
optional `configuration`, optional `must_understand` (default true), and
rejects unknown members. Encode emits a bare string when there is no
configuration, `{"name": n}` when the configuration is empty, and includes
`"must_understand": false` only when false. This matches
`zarrs_metadata/src/v3/metadata.rs` exactly.

Array metadata fields: `zarr_format` (must be 3), `node_type` (must be
"array"), `shape : int iarray`, `data_type : Ext.t`, `chunk_grid : Ext.t`,
`chunk_key_encoding : Ext.t`, `fill_value : Jsont.json` (interpreted after
the dtype is known), `codecs : Ext.t list`, optional `attributes`
(`Jsont.json`), optional `storage_transformers` (rejected if non-empty and
any entry has `must_understand` true), optional `dimension_names :
string option list option`. Unknown top-level members are kept via
`Jsont.Object.keep_unknown`. Opening fails if an unknown member is an
object with `must_understand` true or is a non-object (non-objects imply
`must_understand` true), matching `AdditionalFieldV3`.

Group metadata: `zarr_format` 3, `node_type` "group", optional
`attributes`, same unknown-member rule, and drop a literal
`"consolidated_metadata": null` member before checking (zarr-python
<= 3.1.3 emits it).

### Chunk grid and keys

Regular grid only, matching `RegularChunkGrid`:

- `grid_shape.(d) = ceil_div array_shape.(d) chunk_shape.(d)`.
- `chunk_origin i = i.(d) * chunk_shape.(d)`.
- Every chunk, including edge chunks, is stored at the full chunk shape.
  The region beyond the array bounds holds fill values. Reads clip a
  chunk's subset to the array extent before assembly.
- Any zero-length array dimension makes all chunk lookups return absent.

Chunk key encodings, matching the oracle:

- `default` with separator `/` or `.` (default `/`): key is `"c"` alone for
  a 0-dimensional array, else `"c" ^ sep ^ i0 ^ sep ^ i1 …`.
- `v2` with separator `.` or `/` (default `.`): indices joined by the
  separator, `"0"` for 0-dimensional.

The store key for a chunk is the node path without its leading `/`, joined
to the chunk key with `/`. Metadata key is `<path>/zarr.json`, bare
`zarr.json` at the root.

### Store

Missing keys are `None`. Store failures raise `Error.E` (an exception
carrying `Error.t`). A record of closures rather than a functor, so
backends are runtime values:

```ocaml
module Byte_range : sig
  type t = From_start of { off : int; len : int option } | Suffix of int
end

module Store : sig
  type t = {
    get        : key:string -> Base_bigstring.t option;
    get_range  : key:string -> Byte_range.t -> Base_bigstring.t option;
    get_ranges : key:string -> Byte_range.t list -> Base_bigstring.t list option;
    size       : key:string -> int option;
    ranged     : bool;   (* get_range avoids fetching the whole object *)
    set        : (key:string -> Base_bigstring.t -> unit) option;
    erase      : (key:string -> unit) option;
    list       : (prefix:string -> string list) option;
  }
  val memory : unit -> t
end
```

`Suffix n` is required: the sharding index with `index_location: "end"` is
read with a suffix range. `get_ranges` exists so one shard read can batch
its inner-chunk ranges. A store where `ranged` is false may implement
`get_range` by slicing a full `get`.

### Slab and unboxed access

`Slab.t` is a decoded C-order block: a bigstring, a dtype and a shape.
This is the type the performance requirement lives in.

```ocaml
module Slab : sig
  type t
  val dtype : t -> Dtype.t
  val shape : t -> int iarray
  val num_elements : t -> int
  val bigstring : t -> Base_bigstring.t   (* the underlying buffer *)
  val fill : t -> Fill_value.t -> unit

  module F64 : sig
    val get : t -> int -> float#          (* linear C-order index *)
    val unsafe_get : t -> int -> float#
    val set : t -> int -> float# -> unit
    val unsafe_set : t -> int -> float# -> unit
    val get2 : t -> int -> int -> float#  (* 2-d convenience, row major *)
    val get3 : t -> int -> int -> int -> float#
  end
  (* F32 : float32#. I64, U64 : int64#. I32, U32 : int32#.
     I16, U16 : int16#. I8, U8 : int8#. Bl : bool#.
     F16, BF16 : get returns float# (widened on read, narrowed on set).
     C64, C128 : get_re / get_im returning float32# / float#. *)

  val to_genarray :
    t -> ('a, 'b) Bigarray.kind -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t
  (* Zero-copy reinterpret view for interop. Raises on kind mismatch. *)
end
```

Rules for the accessor modules:

- `get`/`set` check the dtype once per call and bounds check the linear
  index. `unsafe_get`/`unsafe_set` check nothing.
- Every accessor carries `[@zero_alloc]` in the `.mli`. The workspace dev
  profile disables the checker, so the test suite includes a compilation
  probe built with `--profile release-check` that asserts the annotations
  hold. Do not claim `[@zero_alloc]` without that probe passing.
- Implementation should first try the unboxed bigstring primitives
  available in the switch (`unboxed` library, as used by `httpz`, or
  `%caml_bigstring_get64u`-family externals with unboxed result types).
  A boxed primitive followed by `Float_u.of_float` is acceptable only if
  the zero-alloc probe still passes.
- After decode the buffer is native endian, so accessors never swap.
- `to_genarray` needs a small C stub that allocates a new bigarray header
  over the same data (the standard `caml_ba_alloc` with the existing
  `data` pointer and a `proxy` reference). It lives in the core library's
  stubs file.

`Subset.t` is `{ start : int iarray; shape : int iarray }`. The indexer
that drives multi-chunk assembly iterates the contiguous C-order runs of a
subset inside an enclosing shape and yields `(src_off, dst_off, len)`
element runs, which the assembler turns into `Base_bigstring.blit` calls.
No per-element copying on the read path. Use `let mutable` loops rather
than closures in the hot iterator.

### Codec framework

Bound codecs are records of closures. Parsing happens in two steps as in
`zarrs_codec`: metadata to an unbound codec, then binding with the dtype
and fill value.

```ocaml
type repr = { dtype : Dtype.t; shape : int iarray }

type a2a = {  (* array -> array *)
  name : string;
  encoded_repr : repr -> repr;
  encode : Slab.t -> Slab.t;
  decode : Slab.t -> repr -> Slab.t;
}

type a2b = {  (* array -> bytes *)
  name : string;
  encoded_size : repr -> size;       (* Fixed n | Bounded n | Unbounded *)
  encode : Slab.t -> Base_bigstring.t;
  decode : Base_bigstring.t -> repr -> Slab.t;
  partial_decode :
    (Byte_source.t -> repr -> Subset.t -> Slab.t) option;
}

type b2b = {  (* bytes -> bytes *)
  name : string;
  encoded_size : size -> size;
  encode : Base_bigstring.t -> Base_bigstring.t;
  decode : Base_bigstring.t -> decoded_size:size -> Base_bigstring.t;
}
```

`Codec.chain` buckets a `codecs` metadata list into zero or more a2a, then
exactly one a2b, then zero or more b2b, in list order within each bucket.
Order in the JSON is not required to be sorted by kind. Errors are
"missing array to bytes codec" and "multiple array to bytes codecs". A
codec whose name is unknown is an error unless its `must_understand` is
false, in which case it is skipped, matching `CodecChain::from_metadata`.

The registry is a match function from name to constructor, not a mutable
table (the portability escape rules in CLAUDE.md forbid module-level
tables, and a match keeps the door open to annotating the core `portable`
later). `Array.open_` takes an optional `?codecs:(string -> constructor
option)` resolver consulted before the built-ins.

Concrete codecs:

- `bytes`: config `{"endian": "little"|"big"}`, endian mandatory for
  multi-byte dtypes and absent or ignored for 1-byte dtypes. Decode is a
  no-op reinterpret when the declared endianness matches the host (the
  buffer is reused, no copy). Otherwise swap in place per element width.
  complex swaps each half separately.
- `transpose`: config `{"order": perm}`, validated as a permutation of
  0..n-1. Decode applies the inverse permutation with a strided copy that
  memcpys the innermost contiguous run.
- `gzip`: config `{"level": 0..9}` for encode. RFC 1952 via
  `decompress.gz` on bigstrings. Decode allocates the exact output when
  the decoded size is `Fixed` (always true for our dtypes under bytes).
- `zstd`: config `{"level": int, "checksum": bool}` for encode, via
  `zstdz`. Decode sizes the output from the chain's `decoded_size`, not
  from the frame header, and errors if the frame disagrees.
- `crc32c`: no config. Encode appends the 4-byte little-endian CRC32C
  (Castagnoli, via `checkseum`). Decode verifies and strips it, raising
  `Error.E (Checksum_mismatch _)` on disagreement.

### Sharding

`sharding_indexed` is an a2b codec with `partial_decode`. Config:
`chunk_shape` (must divide the shard shape exactly), `codecs` (inner
chain), `index_codecs` (index chain), `index_location` `"start"|"end"`
(default end).

Index semantics, matching the oracle exactly:

- The index is a uint64 array of shape `chunks_per_shard ++ [2]`, encoded
  by the index chain bound to dtype `Uint64` with fill value
  `2^64 - 1`. The index chain must have a `Fixed` encoded size.
- Entry `2*i` is the inner chunk's byte offset in the shard and `2*i + 1`
  its byte length, for C-order linear inner-chunk index `i`. Both equal to
  `0xFFFF_FFFF_FFFF_FFFF` means the inner chunk is absent and reads as
  fill value.
- The index bytes are read with `From_start {off=0; len=Some n}` or
  `Suffix n` per `index_location`, where `n` is the computed encoded index
  size. After the index chain's bytes codec has applied its declared
  endianness the u64s are read natively.

Decode of a subset: fetch and decode the index, compute the inner chunks
intersecting the subset, `get_ranges` their byte spans, decode each
through the inner chain, assemble. Encode builds the whole shard in
memory: encode each non-absent inner chunk (a chunk equal everywhere to
the fill value is omitted), lay them out in C order, then place the index.

### Partial reads

The rule that decides between ranged and full reads: a chunk supports
ranged access when the codec chain's b2b list is empty and the store's
`ranged` is true. Then `Byte_source.t` reads ranges straight from the
store. Otherwise the chunk is fetched whole and wrapped as an in-memory
`Byte_source.t`. Only `sharding_indexed` consumes `Byte_source.t`. This
collapses Rust's partial-decoder tower to the one case that matters over
HTTP and is exactly what the standard sharded layout (outer chain empty,
crc32c inside the index and inner chains) needs.

### Array API

```ocaml
module Arr : sig
  type t
  val open_ : ?codecs:resolver -> Store.t -> path:string -> t
  val shape : t -> int iarray
  val dtype : t -> Dtype.t
  val fill_value : t -> Fill_value.t
  val attributes : t -> Jsont.json option
  val dimension_names : t -> string option list option
  val chunk_shape : t -> int iarray
  val grid_shape : t -> int iarray

  val read_chunk : t -> int array -> Slab.t
  (* Full chunk shape. A missing chunk is a fill-value slab. *)
  val read_chunk_opt : t -> int array -> Slab.t option
  val read : t -> Subset.t -> Slab.t
  (* Any subset of the array, assembled across chunks. Out-of-bounds
     raises. Regions whose chunks are absent read as fill value. *)
  val write_chunk : t -> int array -> Slab.t -> unit
  val write : t -> Subset.t -> Slab.t -> unit
  (* Read-modify-write on partially covered chunks. *)

  val create :
    ?attributes:Jsont.json -> ?dimension_names:string option list ->
    ?codecs:Ext.t list ->      (* default: [bytes little-endian] *)
    shape:int array -> chunk_shape:int array ->
    dtype:Dtype.t -> fill_value:Fill_value.t ->
    Store.t -> path:string -> t
end
```

`Group.open_`, `Group.create`, `Group.attributes`, and `Node.open_`
returning ``[ `Array of Array.t | `Group of Group.t ]`` by reading
`zarr.json` and dispatching on `node_type`. Child listing exists only when
the store has `list`.

Reads of multiple chunks run sequentially in the core. Concurrency is the
caller's and the store's business: the fetch store batches `get_ranges`
into concurrent fibers, and callers can fan out `read_chunk` calls under
their own switch. The core stays Eio-free.

## zstdz binding design

`zstdz.mli`, all sizes as OCaml `int` (zstd sizes fit 63 bits):

```ocaml
type dctx   (* reusable decompression context, custom block, finalized *)
type cctx

val compress_bound : int -> int
val compress : ?level:int -> ?checksum:bool ->
  cctx -> src:Base_bigstring.t -> src_off:int -> src_len:int ->
  dst:Base_bigstring.t -> dst_off:int -> dst_len:int -> int
val decompress :
  dctx -> src:Base_bigstring.t -> src_off:int -> src_len:int ->
  dst:Base_bigstring.t -> dst_off:int -> dst_len:int -> int
val content_size :
  Base_bigstring.t -> off:int -> len:int -> int64#
(* Frame header content size. -1L unknown, -2L error, as libzstd. *)

type frame_info = { content_size : int64; window_size : int;
                    dict_id : int32; has_checksum : bool }
val frame_info :
  Base_bigstring.t -> off:int -> len:int -> frame_info @ local
val error_name : int -> string @ local
```

Stub rules:

- Compress and decompress return the written byte count, or the negated
  zstd error code. The OCaml wrappers raise `Zstdz.Error (code, name)` on
  negatives. The C calls never allocate on the OCaml heap, so the
  externals carry `[@zero_alloc]` (checked by the same release-check
  probe pattern as zarrz).
- The runtime lock is released around the C call when `src_len + dst_len`
  exceeds 64 KiB. Bigarray data is off heap, so the pointers stay valid.
  Parameters are read before releasing.
- `frame_info` and `error_name` allocate their results with
  `caml_alloc_local` and `caml_alloc_local_string`, and their result type
  carries `@ local` in the `.mli`, so error formatting and frame probing
  on the hot path never touch the minor heap. Nothing may trigger a GC
  between `caml_alloc_local` and full initialisation of the block. This is
  the first use of `caml_alloc_local` in the tree, so the stub file
  documents the contract at the definition.
- `dctx`/`cctx` are not thread safe. One context per domain. The zarr
  zstd codec keeps one `dctx` per bound codec instance, which is safe
  because Eio fibers cannot switch inside a C call.
- Externals that are pure functions of their arguments are annotated
  `@@ portable` only after a compiler probe confirms it, per CLAUDE.md.

Tests: round trips at several sizes and levels against fixed vectors,
`content_size` on known frames, error paths (truncated frame, wrong dst
size), and a differential test decompressing a frame produced by the
system `zstd` CLI if present, else a checked-in frame.

## Performance contract

- Store to slab with the standard chain (`bytes` little endian on a little
  endian host, optional zstd/gzip outside) performs exactly one
  decompression pass and zero further copies. The `bytes` no-op case hands
  the store buffer to the slab directly.
- Accessors are `[@zero_alloc]`, verified under `release-check`.
- Subset assembly is memcpy runs, never element loops, except for
  `transpose` whose innermost run is still memcpy.
- `read` of a subset lying in one chunk and equal to that chunk delegates
  to `read_chunk` (the oracle's fast path).
- A `bench/` directory with a `core_bench` or hand-rolled timing harness
  compares: chunk decode throughput (zstd and raw), `F64.get` sum loop
  against a `Bigarray.Array1` baseline, and sharded partial read counts
  (number of store range calls, asserted in tests via a counting store).

## Fetch store

```ocaml
val v : ?ranged:bool -> base_url:string -> _ Fetch.t -> Zarrz.Store.t
```

- Key to URL is `base_url ^ "/" ^ key` with no further escaping (store
  keys are already restricted). `base_url` has no trailing slash.
- `get`: GET, 200 buffers the body into a bigstring sized from
  `content_length` when present, 404 and 410 map to `None`, anything else
  raises `Error.E (Store _)` carrying the status. Bodies are read with
  `Eio.Flow.single_read` into the bigstring via `Cstruct.of_bigarray`
  views, so no intermediate strings.
- `get_range`: `Fetch.Header.range` with `` `Range``/`` `Suffix`` specs.
  206 returns the body. 200 means the server ignored the range, so slice
  locally. 416 raises.
- `get_ranges`: one fiber per range under a local switch, bounded by
  `Fetch.with_limits` on the capability the caller built. Coalescing
  adjacent ranges (gap under 1 MiB) into one request is a follow-up, note
  it in the code.
- `ranged` defaults to true. `size`: HEAD with `content_length`.
- `set`, `erase`, `list` are `None`.
- Tests use `Fetch_mock.client`, asserting the exact Range header bytes
  sent for both range forms, the 200-fallback slicing, and 404 to `None`.

The Eio filesystem store mirrors `zarrs_filesystem` key mapping (key is a
relative path) using `Eio.Path.load`/`save` and `Eio.File.pread` for
ranges, with `list` via directory walks.

## Testing

- Golden fixtures copied from `../zarrs/zarrs/tests/data/` into
  `bleeding/zarrz/test/fixtures/`, with a README naming the source commit
  and the MIT/Apache-2.0 dual licence: `array_write_read.zarr`,
  `sharded_array_write_read.zarr`, `hierarchy.zarr`, `array_metadata.json`,
  `group_metadata.json`, and from `v3/`: `array_none.zarr`,
  `array_gzip.zarr`, `array_zstd.zarr`, `array_none_transpose.zarr`, and
  from `v3_zarr_python/`: `array_gzip.zarr`, `array_zstd.zarr`,
  `array_none.zarr` (zarr-python 3.0.8 output, the cross-implementation
  check). Tests open each fixture through the Eio file store and assert
  element values, shapes and metadata round trips.
- Metadata JSON round trip: decode then encode `array_metadata.json` and
  `group_metadata.json` and compare structurally, including the bare
  string codec form and `must_understand` handling.
- Write path: create an array in a memory store with each codec chain,
  write, read back, compare slabs byte for byte. For chains with a
  deterministic encoding (`bytes`, `crc32c`, sharding with `bytes`+
  `crc32c` index) also compare encoded bytes against fixture chunks.
- `conformance/` builds a `zarrz_conformance` executable with the exact
  `zarrs_conformance` CLI contract: `--array_path <dir>`, print one
  fill-value-metadata JSON token per element in C order. This plugs into
  the external `Bisaloo/zarr-conformance-tests` corpus unchanged.
- Fuzz (follow-up, `fuzz/` with crowbar under an inert-by-default alias as
  in cbort): metadata parser, shard index parser, and the
  `parse (encode x) = x` round trip for fill values.

## Build and verification

Per CLAUDE.md, scoped aliases:

    dune build @bleeding/zstdz/all @bleeding/zstdz/runtest
    dune build @bleeding/zarrz/all @bleeding/zarrz/runtest --force

plus the zero-alloc probes with `--profile release-check`. ocamlformat is
absent from the switch, so format by hand at 80 columns.

## Milestones

1. `zstdz` complete with tests.
2. `zarrz` metadata layer: `Error`, `Dtype`, `Fill_value`, `Ext`,
   `Metadata`, `Chunk_grid`, `Chunk_key`, `Byte_range` with unit tests
   against the metadata fixtures.
3. `Slab`, accessors, `Subset` indexer, `to_genarray` stub, zero-alloc
   probe.
4. Codec framework and the non-sharding codecs, `Codec.chain`.
5. `sharding_indexed` and `Byte_source`.
6. `Store`, memory store, `Arr`/`Group`/`Node`, read and write paths.
7. `zarrz-eio` store, fixture tests, conformance CLI.
8. `zarrz-fetch` store with mock tests. Bench harness.

Milestones 1 to 3 are independent. 4 to 6 depend on 2 and 3. 7 and 8
depend on 6.

# DuckDB OxCaml Binding — Implementation Plan

Status: Phase 0 (hello world) complete. Vendored amalgamation builds and
links. Core open/connect/query/column-access works with GC-managed handles,
structured exceptions, and unboxed numerics.

## Architecture

```
test/test_duckdb.ml          ← user code
src/duckdb.ml / .mli         ← high-level OCaml API, exceptions
src/duckdb_stubs.c           ← direct C stubs (no ctypes)
vendor/duckdb_amalg.cpp      ← vendored DuckDB C++ amalgamation (25 MB)
vendor/duckdb.h              ← C API header (6k lines, 545 functions)
vendor/duckdb.hpp            ← C++ header (included by amalgamation)
```

### Core design decisions (locked in)

- **Direct C stubs** — no ctypes, maximum control
- **Structured exceptions** — `Duckdb.Error` raised from C, no `Result.t`
- **GC-managed handles** — `Custom_tag` + finalizers + atomic refcounting
- **Vendored build** — amalgamation compiled via dune rules + ocamlmklib
- **Unboxed numerics** — `[@unboxed]` / `[@untagged]` / `[@@noalloc]`
- **Blocking sections** — `caml_enter/leave_blocking_section` for queries
- **Local allocation** — `caml_alloc_local` for compound returns
- **Depends on Base/Core** — for `Bigstring` unboxed access, `Iarray`, modes

### Zero-copy data access strategy

DuckDB stores columnar data as flat C arrays inside vectors (chunks of
up to 2048 rows). The binding exposes these directly:

```
DuckDB vector memory (C heap)
  ┌─────────────────────────────────┐
  │ int64[0] │ int64[1] │ ... │    │  ← duckdb_vector_get_data()
  └─────────────────────────────────┘
       │
       ▼  caml_ba_alloc (wraps pointer, no copy)
  Bigstring.t  (Core.Bigstring = Bigarray.Array1.t char)
       │
       ▼  Bigstring.unsafe_get_int64_le_unboxed ~pos:(i*8)
     int64#   ← zero-alloc, unboxed
```

- **Numeric vectors**: Wrap `duckdb_vector_get_data()` as a `Bigstring.t`.
  Use `Bigstring.unsafe_get_{int32,int64,float}_le_unboxed` for
  zero-alloc element access. No typed Bigarray intermediary needed —
  Bigstring is the universal view.
- **Validity masks**: `duckdb_vector_get_validity()` returns `uint64_t*`.
  Wrap as Bigstring, extract bits with `unsafe_get_int64_le_unboxed`
  and bit ops. Provide `[@zero_alloc]` `is_valid` helper.
- **String vectors**: `duckdb_string_t` is 16 bytes per element
  (inlined ≤12 bytes, or pointer+prefix). C stub extracts the string
  data pointer and length, returns `string` (must copy for GC safety)
  or `string @ local` (stack-allocated copy for transient use via
  `Bigstring.get_string__local`).
- **No Bigarray↔Bytes copies** — Bigstring *is* the backing store, and
  unboxed accessors read directly from DuckDB's memory.

## Phase 1: Prepared Statements

Prepared statements with typed parameter binding. Stmt holds a ref on its
connection (which holds a ref on its db) via atomic refcounting.

### C stubs to add

```c
// stmt_wrap: Custom_tag with finalizer, refs conn_wrap
caml_duckdb_prepare          : connection -> string -> stmt
caml_duckdb_execute_prepared : stmt -> result
caml_duckdb_bind_int32       : stmt -> int[@untagged] -> int32[@unboxed] -> unit  [@@noalloc]
caml_duckdb_bind_int64       : stmt -> int[@untagged] -> int64[@unboxed] -> unit  [@@noalloc]
caml_duckdb_bind_double      : stmt -> int[@untagged] -> float[@unboxed] -> unit  [@@noalloc]
caml_duckdb_bind_string      : stmt -> int -> string -> unit
caml_duckdb_bind_null        : stmt -> int[@untagged] -> unit                     [@@noalloc]
caml_duckdb_bind_bool        : stmt -> int[@untagged] -> bool -> unit             [@@noalloc]
caml_duckdb_bind_blob        : stmt -> int -> bytes -> unit
caml_duckdb_clear_bindings   : stmt -> unit
caml_duckdb_param_count      : stmt -> int[@untagged]                             [@@noalloc]
```

### OCaml API

```ocaml
type stmt
val prepare         : connection -> string -> stmt
val execute         : stmt -> result
val bind_int32      : stmt -> int -> int32# -> unit
val bind_int64      : stmt -> int -> int64 -> unit
val bind_double     : stmt -> int -> float -> unit
val bind_string     : stmt -> int -> string -> unit
val bind_null       : stmt -> int -> unit
val bind_bool       : stmt -> int -> bool -> unit
val bind_blob       : stmt -> int -> bytes -> unit
val clear_bindings  : stmt -> unit
val param_count     : stmt -> int
```

## Phase 2: Data Chunk / Vector API (Columnar, Zero-Copy)

The high-performance path. DuckDB returns data in columnar chunks of
2048 rows with flat arrays + validity bitmaps.

### DuckDB internal layout

- `duckdb_vector_get_data()` → `void*` pointing to flat typed array
- `duckdb_vector_get_validity()` → `uint64_t*` bitfield (NULL = all valid)
- `duckdb_string_t`: 16-byte union, inline ≤12 chars or pointer
- `duckdb_list_entry`: `{ offset: uint64; length: uint64 }`

### C stubs

```c
// Data chunk: Custom_tag wrapping duckdb_data_chunk, refs result
caml_duckdb_result_chunk_count  : result -> int[@untagged]           [@@noalloc]
caml_duckdb_result_get_chunk    : result -> int[@untagged] -> data_chunk
caml_duckdb_chunk_size          : data_chunk -> int[@untagged]       [@@noalloc]
caml_duckdb_chunk_column_count  : data_chunk -> int[@untagged]       [@@noalloc]

// Vector data as Bigstring (zero-copy wrap of duckdb_vector_get_data)
// Uses caml_ba_alloc with CAML_BA_EXTERNAL to wrap the raw pointer.
// The Bigstring does NOT own the memory — the data_chunk does.
caml_duckdb_vector_data         : data_chunk -> int[@untagged] -> Bigstring.t
caml_duckdb_vector_validity     : data_chunk -> int[@untagged] -> Bigstring.t option
  // Returns None when validity is NULL (all values valid)

// String vector element access (must copy — GC safety)
caml_duckdb_vector_string       : data_chunk -> int[@untagged] -> int[@untagged] -> string
caml_duckdb_vector_string_local : data_chunk -> int[@untagged] -> int[@untagged] -> string[@local]

// Null check via validity bitmap (direct bit test in C)
caml_duckdb_vector_is_valid     : data_chunk -> int[@untagged] -> int[@untagged] -> bool [@@noalloc]
```

### OCaml API

```ocaml
module Data_chunk : sig
  type t  (* GC-managed, refs result *)

  val chunk_count   : result -> int
  val get_chunk     : result -> int -> t
  val size          : t -> int
  val column_count  : t -> int
end

module Vector : sig
  (** Raw column data as Bigstring — zero-copy view of DuckDB memory.
      Valid only while the parent [Data_chunk.t] is alive. *)
  val data : Data_chunk.t -> col:int -> Bigstring.t

  (** Validity bitmap as Bigstring, or None if all values are valid. *)
  val validity : Data_chunk.t -> col:int -> Bigstring.t option

  (** {2 Typed unboxed element access}

      These use [Bigstring.unsafe_get_*_unboxed] on the data view.
      All are [[@zero_alloc]]. The [col] index selects the vector,
      [row] is the element index within the chunk. *)

  val[@zero_alloc] get_int32   : Data_chunk.t -> col:int -> row:int -> int32#
  val[@zero_alloc] get_int64   : Data_chunk.t -> col:int -> row:int -> int64#
  val[@zero_alloc] get_float64 : Data_chunk.t -> col:int -> row:int -> float#
  val[@zero_alloc] get_float32 : Data_chunk.t -> col:int -> row:int -> float32#
  val[@zero_alloc] get_int8    : Data_chunk.t -> col:int -> row:int -> int8#
  val[@zero_alloc] get_int16   : Data_chunk.t -> col:int -> row:int -> int16#
  val[@zero_alloc] get_bool    : Data_chunk.t -> col:int -> row:int -> bool

  (** String access — copies bytes for GC safety *)
  val get_string       : Data_chunk.t -> col:int -> row:int -> string
  val get_string_local : Data_chunk.t -> col:int -> row:int -> string @ local

  (** Null check — direct bit test, zero alloc *)
  val[@zero_alloc] is_valid : Data_chunk.t -> col:int -> row:int -> bool
end
```

### Implementation: zero-alloc element access

The typed accessors are thin wrappers over Bigstring unboxed reads.
For example:

```ocaml
(* In duckdb.ml — using Core.Bigstring unboxed accessors *)

let[@zero_alloc] get_int64 chunk ~col ~row =
  let bs = data chunk ~col in
  Bigstring.unsafe_get_int64_le_unboxed bs ~pos:(row * 8)

let[@zero_alloc] get_float64 chunk ~col ~row =
  let bs = data chunk ~col in
  Bigstring.unsafe_get_float_unboxed bs ~pos:(row * 8)

let[@zero_alloc] get_int32 chunk ~col ~row =
  let bs = data chunk ~col in
  Bigstring.unsafe_get_int32_le_unboxed bs ~pos:(row * 4)

let[@zero_alloc] get_int8 chunk ~col ~row =
  let bs = data chunk ~col in
  Int8_u.of_int_trunc (Char.to_int (Bigstring.unsafe_get bs (row)))

let[@zero_alloc] is_valid chunk ~col ~row =
  match validity chunk ~col with
  | None -> true  (* NULL validity = all valid *)
  | Some bs ->
    let word = Bigstring.unsafe_get_int64_le_unboxed bs ~pos:((row / 64) * 8) in
    Int64_u.logand word (Int64_u.shift_left #1L (row land 63)) <> #0L
```

### Usage pattern

```ocaml
let sum_column result col =
  let mutable total = #0.0 in
  for i = 0 to Data_chunk.chunk_count result - 1 do
    let local_ chunk = Data_chunk.get_chunk result i in
    let n = Data_chunk.size chunk in
    for j = 0 to n - 1 do
      if Vector.is_valid chunk ~col ~row:j then
        total <- Float_u.add total (Vector.get_float64 chunk ~col ~row:j)
    done
  done;
  Float_u.to_float total
```

### Caching the Bigstring per-vector

To avoid re-calling the C stub on every element access, provide a
pre-bound vector type:

```ocaml
module Bound_vector : sig
  type 'kind t  (* local-friendly, caches the Bigstring *)

  type int32_vec
  type int64_vec
  type float64_vec

  val bind_int32   : Data_chunk.t -> col:int -> int32_vec t
  val bind_int64   : Data_chunk.t -> col:int -> int64_vec t
  val bind_float64 : Data_chunk.t -> col:int -> float64_vec t

  val[@zero_alloc] get_int32   : int32_vec t -> int -> int32#
  val[@zero_alloc] get_int64   : int64_vec t -> int -> int64#
  val[@zero_alloc] get_float64 : float64_vec t -> int -> float#
  val[@zero_alloc] is_valid    : _ t -> int -> bool
end
```

Usage:

```ocaml
let process_chunk chunk =
  let v = Bound_vector.bind_float64 chunk ~col:0 in
  let n = Data_chunk.size chunk in
  let mutable sum = #0.0 in
  for i = 0 to n - 1 do
    if Bound_vector.is_valid v i then
      sum <- Float_u.add sum (Bound_vector.get_float64 v i)
  done;
  Float_u.to_float sum
```

## Phase 3: Compound Types with Unboxed Fields

Stack-allocate structured values returned from DuckDB for zero-heap-alloc
access patterns. Use mixed blocks with unboxed fields.

### Date / Time / Timestamp / Interval

```ocaml
(* Mixed blocks — unboxed numeric fields stored flat *)
type date = { year : int; month : int; day : int }

type time_of_day = {
  hour : int; min : int; sec : int;
  micros : int32#;  (* unboxed, avoids int64 boxing *)
}

type timestamp = {
  date : date;       (* boxed sub-record *)
  micros : int64#;   (* unboxed microseconds since epoch *)
}

type interval = {
  months : int; days : int;
  micros : int64#;   (* unboxed *)
}
```

For the chunk API, provide `[@zero_alloc]` access returning unboxed
representations directly:

```ocaml
(* Raw unboxed access — zero alloc *)
val[@zero_alloc] get_date_days     : Data_chunk.t -> col:int -> row:int -> int32#
  (** Days since epoch as unboxed int32. Convert with [Date.of_days]. *)
val[@zero_alloc] get_timestamp_us  : Data_chunk.t -> col:int -> row:int -> int64#
  (** Microseconds since epoch as unboxed int64. *)
val[@zero_alloc] get_time_us       : Data_chunk.t -> col:int -> row:int -> int64#
  (** Microseconds since midnight as unboxed int64. *)

(* Structured access — local-allocated *)
val get_date      : Data_chunk.t -> col:int -> row:int -> date @ local
val get_timestamp : Data_chunk.t -> col:int -> row:int -> timestamp @ local
val get_interval  : Data_chunk.t -> col:int -> row:int -> interval @ local
```

The local-allocated versions use `caml_alloc_local` in C to put the
record on the caller's stack. Callers that need them to escape can
`{ d with year = d.year }` to copy to heap.

### Decimal

```ocaml
(* Unboxed record for zero-alloc decimal access *)
type decimal = #{ width : int8#; scale : int8#; value : int64# }

val[@zero_alloc] get_decimal : Data_chunk.t -> col:int -> row:int -> decimal
```

## Phase 4: Appender (Bulk Insert)

```ocaml
type appender  (* GC-managed with finalizer *)

val appender_create  : connection -> ?schema:string -> table:string -> unit -> appender
val appender_end_row : appender -> unit
val appender_flush   : appender -> unit
val appender_close   : appender -> unit

(* Unboxed/noalloc append functions *)
external appender_int8    : appender -> (int8#[@unboxed])    -> unit = ... [@@noalloc]
external appender_int16   : appender -> (int16#[@unboxed])   -> unit = ... [@@noalloc]
external appender_int32   : appender -> (int32#[@unboxed])   -> unit = ... [@@noalloc]
external appender_int64   : appender -> (int64[@unboxed])    -> unit = ... [@@noalloc]
external appender_float32 : appender -> (float32#[@unboxed]) -> unit = ... [@@noalloc]
external appender_float64 : appender -> (float[@unboxed])    -> unit = ... [@@noalloc]
external appender_bool    : appender -> bool                 -> unit = ... [@@noalloc]
val appender_string  : appender -> string -> unit
val appender_blob    : appender -> bytes -> unit
val appender_null    : appender -> unit
```

## Phase 5: Type System & Column Metadata

```ocaml
module Type : sig
  type t =
    | Boolean | Tinyint | Smallint | Integer | Bigint
    | UTinyint | USmallint | UInteger | UBigint
    | Float | Double | Varchar | Blob
    | Timestamp | Date | Time | Interval
    | Decimal | Hugeint | List | Struct | Map | Array
    | UUID | Unknown of int

  val column_type : result -> int -> t
  val to_string   : t -> string
end

module Logical_type : sig
  type t  (* GC-managed *)
  val column_logical_type : result -> int -> t
  val id : t -> Type.t
  val decimal_width : t -> int
  val decimal_scale : t -> int
  val child_type : t -> t
  val member_count : t -> int
  val member_name  : t -> int -> string
  val member_type  : t -> int -> t
end
```

### Column names as iarray

```ocaml
(** Returns column names as an immutable array — single allocation,
    then zero-cost access thereafter. *)
val column_names : result -> string iarray
```

## Phase 6: Configuration

```ocaml
module Config : sig
  type t  (* GC-managed *)
  val create : unit -> t
  val set    : t -> string -> string -> unit  (** raises Error *)
end

val open_database_ext : ?path:string -> ?config:Config.t -> unit -> database
```

## Phase 7: Eio Integration

Following the sqlite3_eio pattern:

```ocaml
module Duckdb_eio : sig
  type t

  val open_database : ?path:string -> sw:Eio.Switch.t -> unit -> t
  val query : t -> string -> Duckdb.result
  val with_connection : t -> (Duckdb.connection -> 'a) -> 'a
end
```

- Run blocking DuckDB operations in `Eio_unix.run_in_systhread`
- Switch-managed lifecycle for automatic cleanup

## Phase 8: Port Vendoring Scripts to OCaml

Port `update_sources.py` and `package_build.py` to OCaml executables in
`tools/`.

### tools/update_sources.ml

Replaces the Python script. Does:
1. Scan DuckDB source tree for .cpp/.hpp/.h files
2. Resolve extension sources (core_functions, parquet, json)
3. Generate unity-build .cpp files (group sources per directory)
4. Write `manifest.json` (via Jsont)
5. Create `vendor/duckdb.tar.gz` (via Tar library or shell)

### tools/package_build.ml

Port of the file-collection logic:
1. Parse CMakeLists.txt for `add_library_unity` directives
2. Collect source files and include directories
3. Copy to target directory preserving structure
4. Generate extension loader .cpp from template

Dependencies: `eio`, `jsont`, `tar` (or shell out to `tar`).

## Phase 9: Advanced Features (Future)

- **User-defined functions** — register OCaml functions as DuckDB scalar/aggregate/table functions
- **Arrow integration** — zero-copy Arrow C Data Interface interop
- **Streaming results** — `duckdb_execute_prepared_streaming` for large result sets
- **Extension loading** — `duckdb_load_extension` wrapper
- **COPY support** — bulk load from files

## Testing Strategy

- In-memory database for all unit tests (no filesystem deps)
- Test exception paths explicitly
- Benchmark unboxed column access vs boxed to validate zero-alloc claims
- Use `[@zero_alloc]` annotations on hot iteration functions
- Cram tests for query output verification

## Files inventory

```
bleeding/duckdb/
├── dune                         ✅ done
├── vendor/
│   ├── dune                     ✅ done
│   ├── duckdb_amalg.cpp         ✅ done (25 MB, DuckDB v1.5.0-dev)
│   ├── duckdb.hpp               ✅ done
│   └── duckdb.h                 ✅ done
├── src/
│   ├── dune                     ✅ done
│   ├── duckdb.mli               ✅ done (core API)
│   ├── duckdb.ml                ✅ done (core impl)
│   ├── duckdb_stubs.c           ✅ done (core stubs)
│   └── config/
│       ├── dune                 ✅ done
│       └── discover.ml          ✅ done
├── test/
│   ├── dune                     ✅ done
│   └── test_duckdb.ml           ✅ done (hello world passes)
├── tools/
│   ├── dune                     ○ phase 8
│   ├── update_sources.ml        ○ phase 8
│   └── package_build.ml         ○ phase 8
└── PLAN.md                      ✅ this file
```

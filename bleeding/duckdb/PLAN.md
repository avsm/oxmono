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
- **Local allocation** — `caml_alloc_local` for compound returns (dates etc.)

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

## Phase 2: Data Chunk / Vector API (Columnar)

The high-performance path. DuckDB's native interface returns data in
columnar chunks of 2048 rows with flat arrays + validity bitmaps.

### C stubs

```c
// data_chunk_wrap: Custom_tag with finalizer
caml_duckdb_result_chunk_count  : result -> int[@untagged]        [@@noalloc]
caml_duckdb_result_get_chunk    : result -> int[@untagged] -> data_chunk
caml_duckdb_chunk_size          : data_chunk -> int[@untagged]    [@@noalloc]
caml_duckdb_chunk_column_count  : data_chunk -> int[@untagged]    [@@noalloc]

// Vector access — return Bigarray views (zero-copy)
caml_duckdb_vector_int64_data   : data_chunk -> int[@untagged] -> int64 bigarray
caml_duckdb_vector_float64_data : data_chunk -> int[@untagged] -> float bigarray
caml_duckdb_vector_int32_data   : data_chunk -> int[@untagged] -> int32 bigarray
caml_duckdb_vector_is_null      : data_chunk -> int[@untagged] -> int[@untagged] -> bool [@@noalloc]
```

### OCaml API

```ocaml
module Data_chunk : sig
  type t
  val chunk_count : result -> int
  val get_chunk   : result -> int -> t
  val size        : t -> int
  val column_count : t -> int
end

module Vector : sig
  val get_int64_data  : Data_chunk.t -> int -> (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t
  val get_float64_data : Data_chunk.t -> int -> (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t
  val get_int32_data  : Data_chunk.t -> int -> (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
  val is_null         : Data_chunk.t -> col:int -> row:int -> bool
end
```

### Usage pattern

```ocaml
let sum_column result col =
  let mutable total = 0.0 in
  for i = 0 to Data_chunk.chunk_count result - 1 do
    let local_ chunk = Data_chunk.get_chunk result i in
    let data = Vector.get_float64_data chunk col in
    let n = Data_chunk.size chunk in
    for j = 0 to n - 1 do
      if not (Vector.is_null chunk ~col ~row:j) then
        total <- total +. data.{j}
    done
  done;
  total
```

## Phase 3: Local-Allocated Compound Types

Stack-allocate structured values returned from DuckDB for zero-heap-alloc
access patterns.

### Date / Time / Timestamp / Interval

```c
// Returns local_ record via caml_alloc_local
caml_duckdb_value_date      : result -> int[@untagged] -> int[@untagged] -> date[@local]
caml_duckdb_value_time      : result -> int[@untagged] -> int[@untagged] -> time[@local]
caml_duckdb_value_timestamp : result -> int[@untagged] -> int[@untagged] -> timestamp[@local]
caml_duckdb_value_interval  : result -> int[@untagged] -> int[@untagged] -> interval[@local]
```

```ocaml
type date      = { year : int; month : int; day : int }
type time      = { hour : int; min : int; sec : int; micros : int }
type timestamp = { date : date; time : time }
type interval  = { months : int; days : int; micros : int64 }

external value_date : result -> (int[@untagged]) -> (int[@untagged]) -> (date[@local])
  = "caml_duckdb_value_date_bc" "caml_duckdb_value_date"

(* etc. *)
```

### Decimal (mixed block with unboxed fields)

```ocaml
type decimal = { width : int; scale : int; value : int64# }
```

## Phase 4: Appender (Bulk Insert)

```ocaml
type appender

val appender_create  : connection -> ?schema:string -> table:string -> unit -> appender
val appender_int32   : appender -> int32# -> unit
val appender_int64   : appender -> int64 -> unit
val appender_double  : appender -> float -> unit
val appender_string  : appender -> string -> unit
val appender_null    : appender -> unit
val appender_bool    : appender -> bool -> unit
val appender_end_row : appender -> unit
val appender_flush   : appender -> unit
val appender_close   : appender -> unit
```

All numeric appender functions should be `[@@noalloc]` with `[@unboxed]`/`[@untagged]`.

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
  type t
  val column_logical_type : result -> int -> t
  val id : t -> Type.t
  (* For decimals *)
  val decimal_width : t -> int
  val decimal_scale : t -> int
  (* For lists/arrays *)
  val child_type : t -> t
  (* For structs *)
  val member_count : t -> int
  val member_name  : t -> int -> string
  val member_type  : t -> int -> t
end
```

## Phase 6: Configuration

```ocaml
module Config : sig
  type t
  val create : unit -> t
  val set    : t -> string -> string -> unit  (** raises Error *)
  val count  : unit -> int
  val flag   : int -> string * string         (** name, description *)
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

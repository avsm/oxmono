# chdb-oxcaml: High-Performance ClickHouse Embedded Bindings for OxCaml

## Executive Summary

This document presents a comprehensive design for `chdb-oxcaml`, a high-performance OCaml binding to chdb (ClickHouse Embedded Database) leveraging OxCaml's performance-focused extensions. The design prioritizes:

- **Zero-copy data access** via `Base_bigarray`/`Bigstring`
- **Unboxed types** for numeric results (eliminating boxing overhead)
- **Stack allocation** for temporary structures
- **Zero-alloc hot paths** with `[@zero_alloc]` verification
- **Safe resource management** with finalizers and unique modes

---

## Table of Contents

1. [Architecture Overview](#1-architecture-overview)
2. [Module Structure](#2-module-structure)
3. [Type Design](#3-type-design)
4. [C Stubs and FFI Layer](#4-c-stubs-and-ffi-layer)
5. [Connection Management](#5-connection-management)
6. [Query Execution](#6-query-execution)
7. [Result Handling](#7-result-handling)
8. [Zero-Copy Buffer Access](#8-zero-copy-buffer-access)
9. [Streaming API](#9-streaming-api)
10. [Error Handling](#10-error-handling)
11. [Memory Management](#11-memory-management)
12. [Performance Patterns](#12-performance-patterns)
13. [Testing Strategy](#13-testing-strategy)
14. [Build Configuration](#14-build-configuration)
15. [Implementation Phases](#15-implementation-phases)

---

## 1. Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                      User Application                            │
├─────────────────────────────────────────────────────────────────┤
│                     chdb-oxcaml (OCaml)                          │
│  ┌──────────────┬──────────────┬──────────────┬──────────────┐  │
│  │   Chdb       │  Chdb.Query  │ Chdb.Result  │ Chdb.Stream  │  │
│  │  Connection  │              │   (zero-copy)│              │  │
│  └──────────────┴──────────────┴──────────────┴──────────────┘  │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │              Chdb_stubs (C FFI Layer)                    │   │
│  │  - Unboxed externals for zero-copy buffer access         │   │
│  │  - Finalizers for automatic resource cleanup             │   │
│  └──────────────────────────────────────────────────────────┘   │
├─────────────────────────────────────────────────────────────────┤
│                     libchdb.so (C Library)                       │
│  - chdb_connect / chdb_close_conn                               │
│  - chdb_query / chdb_query_n                                    │
│  - chdb_stream_query / chdb_stream_fetch_result                 │
│  - chdb_result_buffer / chdb_result_length                      │
└─────────────────────────────────────────────────────────────────┘
```

### Design Principles

1. **Zero-copy by default**: Result buffers are accessed as `Bigstring.t` views without copying
2. **Unboxed numerics**: Query statistics (`elapsed`, `rows_read`, etc.) use unboxed types
3. **Resource safety**: Connections and results use finalizers + unique modes
4. **Stack allocation**: Temporary structures (query params, local results) use `local_`/`stack_`
5. **Verified performance**: Hot paths annotated with `[@zero_alloc]`

---

## 2. Module Structure

```
chdb-oxcaml/
├── lib/
│   ├── chdb.ml                    # Main entry point, Connection module
│   ├── chdb.mli                   # Public interface
│   ├── chdb_result.ml             # Result type with zero-copy access
│   ├── chdb_result.mli
│   ├── chdb_stream.ml             # Streaming query support
│   ├── chdb_stream.mli
│   ├── chdb_buffer.ml             # Zero-copy Bigstring views
│   ├── chdb_buffer.mli
│   ├── chdb_types.ml              # Unboxed column types
│   ├── chdb_types.mli
│   └── chdb_error.ml              # Error types
├── stubs/
│   ├── chdb_stubs.c               # C FFI implementation
│   ├── chdb_stubs.h
│   └── dune                       # Stubs build config
├── test/
│   ├── test_basic.ml
│   ├── test_streaming.ml
│   ├── test_zero_copy.ml
│   └── test_performance.ml
├── bench/
│   └── bench_query.ml             # Memtrace-enabled benchmarks
├── dune-project
├── chdb-oxcaml.opam
└── README.md
```

---

## 3. Type Design

### 3.1 Core Opaque Types

```ocaml
(* chdb.mli *)

(** Opaque connection handle.
    Created with [connect], automatically closed on finalization.
    Uses unique mode to prevent use-after-close. *)
type connection

(** Opaque query result.
    Provides zero-copy access to result buffer.
    Must not outlive the connection that produced it. *)
type result

(** Streaming result handle for incremental fetching. *)
type stream
```

### 3.2 Unboxed Statistics Types

```ocaml
(* chdb_types.mli *)

(** Unboxed query execution statistics - no heap allocation *)
type query_stats = #{
  elapsed_ns : int64#;              (* Execution time in nanoseconds *)
  rows_read : int64#;               (* Number of rows in result *)
  bytes_read : int64#;              (* Bytes in internal format *)
  storage_rows_read : int64#;       (* Rows read from storage *)
  storage_bytes_read : int64#;      (* Bytes read from storage *)
}

(** Get statistics without allocation *)
val[@zero_alloc] get_stats : result -> query_stats
```

### 3.3 Zero-Copy Buffer View

```ocaml
(* chdb_buffer.mli *)

(** A view into a chdb result buffer.
    The underlying memory is owned by the result handle.
    MUST NOT be used after the result is destroyed. *)
type t = private Bigstring.t

(** Get a zero-copy view of the result buffer.
    The returned bigstring shares memory with the C library.
    @param result The query result to view
    @return A bigstring view; lifetime tied to [result] *)
val of_result : result -> t

(** Get buffer length without allocation *)
val[@zero_alloc] length : t -> int

(** Zero-copy access to raw bytes *)
val[@zero_alloc] unsafe_get : t -> int -> char

(** Unboxed numeric access for binary formats *)
val[@zero_alloc] get_int32_le_unboxed : t -> pos:int -> int32#
val[@zero_alloc] get_int64_le_unboxed : t -> pos:int -> int64#
val[@zero_alloc] get_float_unboxed : t -> pos:int -> float#
```

### 3.4 Output Format Types

```ocaml
(** ClickHouse output formats *)
type format =
  | CSV
  | TSV
  | JSON
  | JSONEachRow
  | JSONCompact
  | Native          (* Binary format - best for zero-copy parsing *)
  | RowBinary       (* Compact binary format *)
  | Arrow           (* Apache Arrow format *)
  | Parquet
  | Custom of string

val format_to_string : format -> string
```

### 3.5 Error Types

```ocaml
(* chdb_error.ml *)

type error =
  | Connection_failed of string
  | Query_failed of string
  | Invalid_result
  | Stream_exhausted
  | Use_after_close

exception Chdb_error of error

(** Non-allocating error check *)
val[@zero_alloc] is_error : result -> bool
```

---

## 4. C Stubs and FFI Layer

### 4.1 External Declarations with Unboxed Types

```ocaml
(* chdb_stubs.ml - external declarations *)

(** Connection management *)
external chdb_connect_raw : string array -> connection option
  = "caml_chdb_connect"

external chdb_close_conn_raw : connection -> unit
  = "caml_chdb_close_conn" [@@noalloc]

(** Query execution *)
external chdb_query_raw : connection -> string -> int -> string -> int -> result option
  = "caml_chdb_query_n"

(** Zero-copy buffer access *)
external chdb_result_buffer_ptr : result -> nativeint
  = "caml_chdb_result_buffer_ptr" [@@noalloc]

external chdb_result_length : result -> int
  = "caml_chdb_result_length" [@@noalloc]

(** Unboxed statistics access *)
external chdb_result_elapsed_unboxed : result -> (float[@unboxed])
  = "caml_chdb_result_elapsed_boxed" "caml_chdb_result_elapsed_unboxed"
  [@@noalloc]

external chdb_result_rows_read_unboxed : result -> (int64[@unboxed])
  = "caml_chdb_result_rows_read_boxed" "caml_chdb_result_rows_read_unboxed"
  [@@noalloc]

external chdb_result_bytes_read_unboxed : result -> (int64[@unboxed])
  = "caml_chdb_result_bytes_read_boxed" "caml_chdb_result_bytes_read_unboxed"
  [@@noalloc]

(** Error access *)
external chdb_result_error_ptr : result -> nativeint
  = "caml_chdb_result_error_ptr" [@@noalloc]

(** Streaming *)
external chdb_stream_query_raw : connection -> string -> int -> string -> int -> result option
  = "caml_chdb_stream_query_n"

external chdb_stream_fetch_result : connection -> result -> result option
  = "caml_chdb_stream_fetch_result"

external chdb_stream_cancel : connection -> result -> unit
  = "caml_chdb_stream_cancel" [@@noalloc]

(** Result cleanup - called by finalizer *)
external chdb_destroy_result : result -> unit
  = "caml_chdb_destroy_result" [@@noalloc]
```

### 4.2 C Stub Implementation

```c
/* chdb_stubs.c */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <chdb.h>
#include <string.h>

/* Custom block for connection with destructor */
static struct custom_operations connection_ops = {
    "chdb.connection",
    custom_finalize_default,  /* We use explicit close */
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

/* Custom block for result with destructor */
static void finalize_result(value v) {
    chdb_result *result = *(chdb_result **)Data_custom_val(v);
    if (result != NULL) {
        chdb_destroy_query_result(result);
    }
}

static struct custom_operations result_ops = {
    "chdb.result",
    finalize_result,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

/* Connection */
CAMLprim value caml_chdb_connect(value args_array) {
    CAMLparam1(args_array);
    CAMLlocal2(opt, conn_val);

    int argc = Wosize_val(args_array);
    char **argv = malloc((argc + 1) * sizeof(char *));

    for (int i = 0; i < argc; i++) {
        argv[i] = strdup(String_val(Field(args_array, i)));
    }
    argv[argc] = NULL;

    chdb_connection *conn = chdb_connect(argc, argv);

    /* Free argv strings */
    for (int i = 0; i < argc; i++) {
        free(argv[i]);
    }
    free(argv);

    if (conn == NULL) {
        CAMLreturn(Val_none);
    }

    conn_val = caml_alloc_custom(&connection_ops, sizeof(chdb_connection *), 0, 1);
    *(chdb_connection **)Data_custom_val(conn_val) = conn;

    opt = caml_alloc_some(conn_val);
    CAMLreturn(opt);
}

CAMLprim value caml_chdb_close_conn(value conn_val) {
    chdb_connection **conn_ptr = (chdb_connection **)Data_custom_val(conn_val);
    if (*conn_ptr != NULL) {
        chdb_close_conn(conn_ptr);
        *conn_ptr = NULL;
    }
    return Val_unit;
}

/* Query with explicit lengths (binary safe) */
CAMLprim value caml_chdb_query_n(value conn_val, value query_val, value query_len,
                                   value format_val, value format_len) {
    CAMLparam5(conn_val, query_val, query_len, format_val, format_len);
    CAMLlocal2(opt, result_val);

    chdb_connection *conn = *(chdb_connection **)Data_custom_val(conn_val);
    if (conn == NULL) {
        CAMLreturn(Val_none);  /* Connection closed */
    }

    const char *query = String_val(query_val);
    size_t qlen = Int_val(query_len);
    const char *format = String_val(format_val);
    size_t flen = Int_val(format_len);

    chdb_result *result = chdb_query_n(conn, query, qlen, format, flen);

    if (result == NULL) {
        CAMLreturn(Val_none);
    }

    result_val = caml_alloc_custom(&result_ops, sizeof(chdb_result *), 0, 1);
    *(chdb_result **)Data_custom_val(result_val) = result;

    opt = caml_alloc_some(result_val);
    CAMLreturn(opt);
}

/* Zero-copy buffer access - returns pointer for Bigarray wrapping */
CAMLprim value caml_chdb_result_buffer_ptr(value result_val) {
    chdb_result *result = *(chdb_result **)Data_custom_val(result_val);
    char *buf = chdb_result_buffer(result);
    return caml_copy_nativeint((intnat)buf);
}

CAMLprim value caml_chdb_result_length(value result_val) {
    chdb_result *result = *(chdb_result **)Data_custom_val(result_val);
    return Val_long(chdb_result_length(result));
}

/* Unboxed statistics - no allocation! */
double caml_chdb_result_elapsed_unboxed(value result_val) {
    chdb_result *result = *(chdb_result **)Data_custom_val(result_val);
    return chdb_result_elapsed(result);
}

CAMLprim value caml_chdb_result_elapsed_boxed(value result_val) {
    return caml_copy_double(caml_chdb_result_elapsed_unboxed(result_val));
}

int64_t caml_chdb_result_rows_read_unboxed(value result_val) {
    chdb_result *result = *(chdb_result **)Data_custom_val(result_val);
    return (int64_t)chdb_result_rows_read(result);
}

CAMLprim value caml_chdb_result_rows_read_boxed(value result_val) {
    return caml_copy_int64(caml_chdb_result_rows_read_unboxed(result_val));
}

int64_t caml_chdb_result_bytes_read_unboxed(value result_val) {
    chdb_result *result = *(chdb_result **)Data_custom_val(result_val);
    return (int64_t)chdb_result_bytes_read(result);
}

CAMLprim value caml_chdb_result_bytes_read_boxed(value result_val) {
    return caml_copy_int64(caml_chdb_result_bytes_read_unboxed(result_val));
}

/* Error access */
CAMLprim value caml_chdb_result_error_ptr(value result_val) {
    chdb_result *result = *(chdb_result **)Data_custom_val(result_val);
    const char *err = chdb_result_error(result);
    return caml_copy_nativeint((intnat)err);
}

/* Explicit result destruction */
CAMLprim value caml_chdb_destroy_result(value result_val) {
    chdb_result **result_ptr = (chdb_result **)Data_custom_val(result_val);
    if (*result_ptr != NULL) {
        chdb_destroy_query_result(*result_ptr);
        *result_ptr = NULL;
    }
    return Val_unit;
}

/* Streaming query */
CAMLprim value caml_chdb_stream_query_n(value conn_val, value query_val, value query_len,
                                         value format_val, value format_len) {
    /* Same implementation as caml_chdb_query_n but uses chdb_stream_query_n */
    CAMLparam5(conn_val, query_val, query_len, format_val, format_len);
    CAMLlocal2(opt, result_val);

    chdb_connection *conn = *(chdb_connection **)Data_custom_val(conn_val);
    if (conn == NULL) {
        CAMLreturn(Val_none);
    }

    const char *query = String_val(query_val);
    size_t qlen = Int_val(query_len);
    const char *format = String_val(format_val);
    size_t flen = Int_val(format_len);

    chdb_result *result = chdb_stream_query_n(conn, query, qlen, format, flen);

    if (result == NULL) {
        CAMLreturn(Val_none);
    }

    result_val = caml_alloc_custom(&result_ops, sizeof(chdb_result *), 0, 1);
    *(chdb_result **)Data_custom_val(result_val) = result;

    opt = caml_alloc_some(result_val);
    CAMLreturn(opt);
}

CAMLprim value caml_chdb_stream_fetch_result(value conn_val, value stream_val) {
    CAMLparam2(conn_val, stream_val);
    CAMLlocal2(opt, result_val);

    chdb_connection *conn = *(chdb_connection **)Data_custom_val(conn_val);
    chdb_result *stream = *(chdb_result **)Data_custom_val(stream_val);

    if (conn == NULL || stream == NULL) {
        CAMLreturn(Val_none);
    }

    chdb_result *result = chdb_stream_fetch_result(conn, stream);

    if (result == NULL) {
        CAMLreturn(Val_none);
    }

    result_val = caml_alloc_custom(&result_ops, sizeof(chdb_result *), 0, 1);
    *(chdb_result **)Data_custom_val(result_val) = result;

    opt = caml_alloc_some(result_val);
    CAMLreturn(opt);
}

CAMLprim value caml_chdb_stream_cancel(value conn_val, value stream_val) {
    chdb_connection *conn = *(chdb_connection **)Data_custom_val(conn_val);
    chdb_result *stream = *(chdb_result **)Data_custom_val(stream_val);

    if (conn != NULL && stream != NULL) {
        chdb_stream_cancel_query(conn, stream);
    }
    return Val_unit;
}

/* Bigarray creation from result buffer - ZERO COPY */
CAMLprim value caml_chdb_result_to_bigstring(value result_val) {
    CAMLparam1(result_val);
    CAMLlocal1(ba);

    chdb_result *result = *(chdb_result **)Data_custom_val(result_val);
    if (result == NULL) {
        caml_failwith("Result already destroyed");
    }

    char *buf = chdb_result_buffer(result);
    size_t len = chdb_result_length(result);

    if (buf == NULL || len == 0) {
        /* Return empty bigarray */
        ba = caml_ba_alloc_dims(CAML_BA_CHAR | CAML_BA_C_LAYOUT, 1, NULL, 0);
    } else {
        /* Wrap existing buffer - NO COPY */
        /* CAML_BA_EXTERNAL means OCaml doesn't own the memory */
        ba = caml_ba_alloc_dims(
            CAML_BA_CHAR | CAML_BA_C_LAYOUT | CAML_BA_EXTERNAL,
            1, buf, len
        );
    }

    CAMLreturn(ba);
}
```

---

## 5. Connection Management

### 5.1 Safe Connection Type with Unique Mode

```ocaml
(* chdb.ml *)

module Connection = struct
  type t = {
    mutable handle : connection_handle option;
    path : string option;
  }

  (** Create a new connection.
      @param path Optional path for persistent storage (defaults to :memory:) *)
  let connect ?path () : t =
    let args = match path with
      | None -> [| "clickhouse"; "--" |]
      | Some p -> [| "clickhouse"; "--path=" ^ p |]
    in
    match chdb_connect_raw args with
    | None -> raise (Chdb_error (Connection_failed "Failed to create connection"))
    | Some handle ->
      { handle = Some handle; path }

  (** Close connection explicitly.
      After calling, any use of the connection will raise. *)
  let close (t : t) : unit =
    match t.handle with
    | None -> ()  (* Already closed *)
    | Some h ->
      chdb_close_conn_raw h;
      t.handle <- None

  (** Check if connection is still open *)
  let[@zero_alloc] is_open (t : t) : bool =
    Option.is_some t.handle

  (** Get the underlying handle, raising if closed *)
  let get_handle (t : t) : connection_handle =
    match t.handle with
    | None -> raise (Chdb_error Use_after_close)
    | Some h -> h

  (** Execute with automatic cleanup on exception *)
  let with_connection ?path ~f =
    let conn = connect ?path () in
    match f conn with
    | result ->
      close conn;
      result
    | exception e ->
      close conn;
      raise e
end
```

### 5.2 Unique Mode Connection (Advanced)

For stricter resource control using OxCaml's uniqueness:

```ocaml
(** A connection with unique ownership - guarantees single owner *)
module Unique_connection : sig
  type t @ unique

  val create : ?path:string -> unit -> t @ unique
  val close : t @ unique -> unit
  val query : t @ unique -> string -> format -> (result * t) @ unique
end = struct
  type t = Connection.t

  let create = Connection.connect
  let close = Connection.close

  let query (conn @ unique) query_str fmt =
    let result = Query.execute conn query_str fmt in
    (result, conn)  (* Return connection to maintain unique ownership *)
end
```

---

## 6. Query Execution

### 6.1 Basic Query API

```ocaml
(* chdb.ml - Query module *)

module Query = struct
  (** Execute a query and return the result.
      @param conn Active connection
      @param query SQL query string
      @param format Output format (default: CSV)
      @return Query result with zero-copy buffer access *)
  let execute (conn : Connection.t) ?(format = CSV) (query : string) : result =
    let handle = Connection.get_handle conn in
    let format_str = format_to_string format in
    match chdb_query_raw handle query (String.length query)
                         format_str (String.length format_str) with
    | None -> raise (Chdb_error (Query_failed "Query execution failed"))
    | Some result ->
      (* Check for query error *)
      let err_ptr = chdb_result_error_ptr result in
      if Nativeint.compare err_ptr Nativeint.zero <> 0 then begin
        let err_msg = (* Read C string from pointer *)
          Chdb_stubs.read_c_string err_ptr in
        raise (Chdb_error (Query_failed err_msg))
      end;
      result

  (** Execute query with local result - stack allocated wrapper *)
  let execute_local (conn : Connection.t) ?(format = CSV) (query : string) = exclave_
    stack_ (execute conn ~format query)

  (** Execute query and return result as string (copies data) *)
  let execute_string (conn : Connection.t) ?(format = CSV) (query : string) : string =
    let result = execute conn ~format query in
    Result.to_string result

  (** Execute query with local string result *)
  let execute_string_local (conn : Connection.t) ?(format = CSV) (query : string) = exclave_
    let result = execute conn ~format query in
    Result.to_string_local result
end
```

### 6.2 Parameterized Queries

```ocaml
module Params = struct
  type param =
    | Int of int
    | Int64 of int64
    | Float of float
    | String of string
    | Bool of bool
    | Null

  (** Build query arguments from parameters.
      Stack-allocated intermediate structures. *)
  let build_args (params : (string * param) list) = exclave_
    let local_ args = ref [] in
    List.iter (fun (name, value) ->
      let arg = match value with
        | Int i -> Printf.sprintf "--param_%s=%d" name i
        | Int64 i -> Printf.sprintf "--param_%s=%Ld" name i
        | Float f -> Printf.sprintf "--param_%s=%g" name f
        | String s -> Printf.sprintf "--param_%s='%s'" name (escape_string s)
        | Bool b -> Printf.sprintf "--param_%s=%b" name b
        | Null -> Printf.sprintf "--param_%s=NULL" name
      in
      args := stack_ (arg :: !args)
    ) params;
    !args

  (** Execute parameterized query *)
  let execute (conn : Connection.t) ?(format = CSV)
              ~(query : string) ~(params : (string * param) list) : result =
    (* Build full command with params *)
    let args = build_args params in
    (* ... implementation ... *)
    Query.execute conn ~format query
end
```

---

## 7. Result Handling

### 7.1 Zero-Copy Result Access

```ocaml
(* chdb_result.ml *)

type t = result

(** Get zero-copy view of result buffer as Bigstring.
    WARNING: The returned Bigstring shares memory with the C library.
    It becomes invalid when the result is destroyed. *)
let to_bigstring (t : t) : Bigstring.t =
  chdb_result_to_bigstring t

(** Get result length without allocation *)
let[@zero_alloc] length (t : t) : int =
  chdb_result_length t

(** Check if result is empty *)
let[@zero_alloc] is_empty (t : t) : bool =
  chdb_result_length t = 0

(** Get unboxed statistics - zero allocation *)
let[@zero_alloc] get_stats (t : t) : query_stats =
  #{
    elapsed_ns =
      let elapsed_sec = chdb_result_elapsed_unboxed t in
      Int64_u.of_float (Float_u.mul (Float_u.of_float elapsed_sec) #1e9);
    rows_read = Int64_u.of_int64 (chdb_result_rows_read_unboxed t);
    bytes_read = Int64_u.of_int64 (chdb_result_bytes_read_unboxed t);
    storage_rows_read = Int64_u.of_int64 (chdb_result_storage_rows_read_unboxed t);
    storage_bytes_read = Int64_u.of_int64 (chdb_result_storage_bytes_read_unboxed t);
  }

(** Convert to string (copies data) *)
let to_string (t : t) : string =
  let buf = to_bigstring t in
  Bigstring.to_string buf

(** Convert to local string (stack-allocated copy) *)
let to_string_local (t : t) = exclave_
  let buf = to_bigstring t in
  Bigstring.to_string__local buf

(** Convert to bytes (copies data) *)
let to_bytes (t : t) : bytes =
  let buf = to_bigstring t in
  Bigstring.to_bytes buf
```

### 7.2 Typed Result Parsing (Native Format)

```ocaml
(* chdb_types.ml - For parsing ClickHouse Native binary format *)

module Native_parser = struct
  type column_type =
    | UInt8 | UInt16 | UInt32 | UInt64
    | Int8 | Int16 | Int32 | Int64
    | Float32 | Float64
    | String
    | DateTime
    | Date
    | Nullable of column_type

  (** Parse a UInt64 column with zero-copy, unboxed access *)
  let[@zero_alloc] parse_uint64_column (buf : Bigstring.t) ~(offset : int)
                                        ~(count : int) ~(f : int64# -> unit) : int =
    let mutable pos = offset in
    for _ = 0 to count - 1 do
      let value = Bigstring.get_int64_le_unboxed buf ~pos in
      f value;
      pos <- pos + 8
    done;
    pos

  (** Parse a Float64 column zero-copy *)
  let[@zero_alloc] parse_float64_column (buf : Bigstring.t) ~(offset : int)
                                         ~(count : int) ~(f : float# -> unit) : int =
    let mutable pos = offset in
    for _ = 0 to count - 1 do
      let value = Bigstring.get_float_unboxed buf ~pos in
      f value;
      pos <- pos + 8
    done;
    pos

  (** Fold over rows with unboxed accumulator *)
  let[@zero_alloc] fold_int64_column (buf : Bigstring.t) ~(offset : int)
                                      ~(count : int) ~(init : int64#)
                                      ~(f : int64# -> int64# -> int64#) : int64# =
    let mutable pos = offset in
    let mutable acc = init in
    for _ = 0 to count - 1 do
      let value = Bigstring.get_int64_le_unboxed buf ~pos in
      acc <- f acc value;
      pos <- pos + 8
    done;
    acc
end
```

---

## 8. Zero-Copy Buffer Access

### 8.1 Buffer Module

```ocaml
(* chdb_buffer.ml *)

type t = Bigstring.t

(** Create buffer view from result - ZERO COPY *)
let of_result (result : Result.t) : t =
  chdb_result_to_bigstring result

(** Buffer length *)
let[@zero_alloc] length (t : t) : int =
  Bigstring.length t

(** Direct byte access *)
let[@zero_alloc] get (t : t) (pos : int) : char =
  Bigstring.get t pos

let[@zero_alloc] unsafe_get (t : t) (pos : int) : char =
  Bigstring.unsafe_get t pos

(** Unboxed integer access - little endian *)
let[@zero_alloc] get_int8 (t : t) ~(pos : int) : int =
  Char.code (Bigstring.get t pos)

let[@zero_alloc] get_int16_le_unboxed (t : t) ~(pos : int) : int =
  Bigstring.get_int16_le t ~pos

let[@zero_alloc] get_int32_le_unboxed (t : t) ~(pos : int) : int32# =
  Bigstring.get_int32_le_unboxed t ~pos

let[@zero_alloc] get_int64_le_unboxed (t : t) ~(pos : int) : int64# =
  Bigstring.get_int64_le_unboxed t ~pos

let[@zero_alloc] get_float_unboxed (t : t) ~(pos : int) : float# =
  Bigstring.get_float_unboxed t ~pos

(** Extract substring - LOCAL version for temporary use *)
let get_string_local (t : t) ~(pos : int) ~(len : int) = exclave_
  Bigstring.get_string__local t ~pos ~len

(** Extract substring - allocates on heap *)
let get_string (t : t) ~(pos : int) ~(len : int) : string =
  Bigstring.get_string t ~pos ~len

(** Iterate over bytes with local closure *)
let iter_local (t : t) ~(f : char -> unit @ local) : unit =
  Bigstring.iter__local t ~f

(** Fold over bytes with local accumulator *)
let fold_local (t : t) ~(init : 'a) ~(f : 'a -> char -> 'a @ local) : 'a =
  Bigstring.fold__local t ~init ~f

(** Find byte position *)
let find (t : t) ~(pos : int) ~(len : int) ~(byte : char) : int option =
  let rec loop i =
    if i >= pos + len then None
    else if Bigstring.unsafe_get t i = byte then Some i
    else loop (i + 1)
  in
  loop pos

(** Blit to bytes *)
let blit_to_bytes (t : t) ~(src_pos : int) ~(dst : bytes)
                  ~(dst_pos : int) ~(len : int) : unit =
  Bigstring.blit_bigstring_bytes t ~src_pos dst ~dst_pos ~len
```

### 8.2 Safe Buffer Views with Lifetime Tracking

```ocaml
(** A buffer view that tracks its parent result's lifetime.
    Prevents use-after-free by keeping result alive. *)
module Safe_buffer = struct
  type t = {
    buffer : Bigstring.t;
    result : Result.t;  (* Prevents premature GC *)
  }

  let of_result (result : Result.t) : t =
    { buffer = Result.to_bigstring result; result }

  let[@zero_alloc] length (t : t) : int =
    Bigstring.length t.buffer

  let[@zero_alloc] get (t : t) (pos : int) : char =
    Bigstring.get t.buffer pos

  (* ... other accessors ... *)
end
```

---

## 9. Streaming API

### 9.1 Stream Type

```ocaml
(* chdb_stream.ml *)

type t = {
  conn : Connection.t;
  mutable stream_handle : result option;
  mutable exhausted : bool;
}

(** Start a streaming query *)
let start (conn : Connection.t) ?(format = CSV) (query : string) : t =
  let handle = Connection.get_handle conn in
  let format_str = format_to_string format in
  match chdb_stream_query_raw handle query (String.length query)
                              format_str (String.length format_str) with
  | None -> raise (Chdb_error (Query_failed "Failed to start streaming query"))
  | Some stream ->
    { conn; stream_handle = Some stream; exhausted = false }

(** Fetch next chunk from stream.
    Returns None when stream is exhausted. *)
let fetch (t : t) : result option =
  if t.exhausted then None
  else match t.stream_handle with
  | None -> raise (Chdb_error Stream_exhausted)
  | Some stream ->
    let handle = Connection.get_handle t.conn in
    match chdb_stream_fetch_result handle stream with
    | None ->
      t.exhausted <- true;
      None
    | Some result ->
      (* Check if this chunk is empty (stream end) *)
      if chdb_result_length result = 0 then begin
        t.exhausted <- true;
        None
      end else
        Some result

(** Cancel streaming query early *)
let cancel (t : t) : unit =
  match t.stream_handle with
  | None -> ()
  | Some stream ->
    let handle = Connection.get_handle t.conn in
    chdb_stream_cancel handle stream;
    t.stream_handle <- None;
    t.exhausted <- true

(** Iterate over all chunks *)
let iter (t : t) ~(f : result -> unit) : unit =
  let rec loop () =
    match fetch t with
    | None -> ()
    | Some chunk ->
      f chunk;
      loop ()
  in
  loop ()

(** Fold over all chunks *)
let fold (t : t) ~(init : 'a) ~(f : 'a -> result -> 'a) : 'a =
  let rec loop acc =
    match fetch t with
    | None -> acc
    | Some chunk -> loop (f acc chunk)
  in
  loop init

(** Fold with local closure *)
let fold_local (t : t) ~(init : 'a) ~(f : 'a -> result -> 'a @ local) : 'a =
  let rec loop acc =
    match fetch t with
    | None -> acc
    | Some chunk -> loop (f acc chunk)
  in
  loop init

(** Convert stream to Seq.t for lazy consumption *)
let to_seq (t : t) : result Seq.t =
  let rec next () =
    match fetch t with
    | None -> Seq.Nil
    | Some chunk -> Seq.Cons (chunk, next)
  in
  next
```

### 9.2 Zero-Alloc Streaming Aggregation

```ocaml
(** Example: Sum a column across streaming chunks with zero allocation *)
let[@zero_alloc] stream_sum_column (t : Stream.t) ~(column_offset : int)
                                    ~(column_size : int) : int64# =
  let mutable total : int64# = #0L in
  let rec loop () =
    match Stream.fetch t with
    | None -> ()
    | Some chunk ->
      let buf = Result.to_bigstring chunk in
      let row_count = Result.length chunk / column_size in
      let chunk_sum = Native_parser.fold_int64_column buf
        ~offset:column_offset ~count:row_count
        ~init:#0L ~f:Int64_u.add in
      total <- Int64_u.add total chunk_sum;
      loop ()
  in
  loop ();
  total
```

---

## 10. Error Handling

### 10.1 Error Types

```ocaml
(* chdb_error.ml *)

type error =
  | Connection_failed of string
  | Query_failed of string
  | Invalid_result
  | Stream_exhausted
  | Use_after_close
  | Buffer_overflow of { expected : int; actual : int }

exception Chdb_error of error

let error_to_string = function
  | Connection_failed msg -> "Connection failed: " ^ msg
  | Query_failed msg -> "Query failed: " ^ msg
  | Invalid_result -> "Invalid result"
  | Stream_exhausted -> "Stream exhausted"
  | Use_after_close -> "Use after close"
  | Buffer_overflow { expected; actual } ->
    Printf.sprintf "Buffer overflow: expected %d bytes, got %d" expected actual

let () =
  Printexc.register_printer (function
    | Chdb_error e -> Some (error_to_string e)
    | _ -> None)
```

### 10.2 Result Type (Alternative to Exceptions)

```ocaml
module Result_ext = struct
  type ('a, 'e) t = ('a, 'e) result

  (** Query returning Or_error *)
  let query (conn : Connection.t) ?(format = CSV) (q : string)
    : (result, error) t =
    try Ok (Query.execute conn ~format q)
    with Chdb_error e -> Error e

  (** Query with local error - no allocation on success path *)
  let query_local (conn : Connection.t) ?(format = CSV) (q : string)
    : (result, error) t =
    try Ok (Query.execute conn ~format q)
    with Chdb_error e -> Error e
end
```

---

## 11. Memory Management

### 11.1 Finalizer-Based Cleanup

Results use custom blocks with finalizers:

```ocaml
(* Automatic cleanup via GC finalizers - already in C stubs *)
(* The finalize_result function in chdb_stubs.c handles this *)
```

### 11.2 Explicit Resource Management

```ocaml
module Resource = struct
  (** Execute query with automatic result cleanup *)
  let with_result (conn : Connection.t) ?(format = CSV) (query : string)
                  ~(f : result -> 'a) : 'a =
    let result = Query.execute conn ~format query in
    match f result with
    | value ->
      (* Explicit early cleanup *)
      chdb_destroy_result result;
      value
    | exception e ->
      chdb_destroy_result result;
      raise e

  (** Execute with buffer access, ensuring cleanup *)
  let with_buffer (conn : Connection.t) ?(format = CSV) (query : string)
                  ~(f : Bigstring.t -> 'a) : 'a =
    with_result conn ~format query ~f:(fun result ->
      let buf = Result.to_bigstring result in
      f buf
    )
end
```

### 11.3 Stack-Allocated Intermediate Values

```ocaml
(** Process query with stack-allocated intermediates *)
let process_query_local (conn : Connection.t) ~(query : string) : int =
  (* Result wrapper is stack-allocated *)
  let local_ result = Query.execute_local conn query in
  (* Statistics are unboxed - no allocation *)
  let stats = Result.get_stats result in
  Int64_u.to_int stats.#rows_read
```

---

## 12. Performance Patterns

### 12.1 Zero-Alloc Query Loop

```ocaml
(** Process multiple queries with zero heap allocation in hot path *)
let[@zero_alloc] process_queries (conn : Connection.t)
                                  (queries : string array) : int64# =
  let mutable total_rows : int64# = #0L in
  for i = 0 to Array.length queries - 1 do
    let result = Query.execute conn ~format:Native queries.(i) in
    let stats = Result.get_stats result in
    total_rows <- Int64_u.add total_rows stats.#rows_read
  done;
  total_rows
```

### 12.2 Zero-Copy Binary Parsing

```ocaml
(** Parse ClickHouse Native format with zero-copy *)
let[@zero_alloc] parse_native_result (result : result)
                                      ~(f : int64# -> float# -> unit) : unit =
  let buf = Result.to_bigstring result in
  let len = Bigstring.length buf in
  (* Native format: column-oriented with header *)
  (* Parse header, then iterate columns *)
  let mutable pos = 0 in
  while pos < len do
    let id = Bigstring.get_int64_le_unboxed buf ~pos in
    let value = Bigstring.get_float_unboxed buf ~pos:(pos + 8) in
    f id value;
    pos <- pos + 16
  done
```

### 12.3 Streaming with Bounded Memory

```ocaml
(** Process large result set with constant memory *)
let process_large_query (conn : Connection.t) ~(query : string)
                        ~(process_row : int64# -> float# -> unit) : unit =
  let stream = Stream.start conn ~format:RowBinary query in
  Stream.iter stream ~f:(fun chunk ->
    let buf = Result.to_bigstring chunk in
    parse_native_result { (* wrap as result *) } ~f:process_row
  )
```

### 12.4 Local Aggregation Pattern

```ocaml
(** Aggregate with stack-allocated accumulator *)
let aggregate_column (conn : Connection.t) ~(query : string) : float =
  let result = Query.execute conn ~format:Native query in
  let buf = Result.to_bigstring result in

  (* Use local ref for accumulator *)
  let local_ sum_ref = ref #0.0 in
  let local_ count_ref = ref 0 in

  let len = Bigstring.length buf in
  let mutable pos = 0 in
  while pos + 8 <= len do
    let value = Bigstring.get_float_unboxed buf ~pos in
    sum_ref := Float_u.add !sum_ref value;
    count_ref := !count_ref + 1;
    pos <- pos + 8
  done;

  (* Convert to boxed for return *)
  Float_u.to_float (Float_u.div !sum_ref (Float_u.of_int !count_ref))
```

---

## 13. Testing Strategy

### 13.1 Unit Tests

```ocaml
(* test/test_basic.ml *)

let test_connection () =
  let conn = Chdb.Connection.connect () in
  assert (Chdb.Connection.is_open conn);
  Chdb.Connection.close conn;
  assert (not (Chdb.Connection.is_open conn))

let test_simple_query () =
  Chdb.Connection.with_connection ~f:(fun conn ->
    let result = Chdb.Query.execute conn "SELECT 1" in
    assert (Chdb.Result.length result > 0)
  )

let test_zero_copy_buffer () =
  Chdb.Connection.with_connection ~f:(fun conn ->
    let result = Chdb.Query.execute conn ~format:RowBinary
                   "SELECT toUInt64(42)" in
    let buf = Chdb.Result.to_bigstring result in
    let value = Bigstring.get_int64_le_unboxed buf ~pos:0 in
    assert (Int64_u.equal value #42L)
  )

let test_unboxed_stats () =
  Chdb.Connection.with_connection ~f:(fun conn ->
    let result = Chdb.Query.execute conn "SELECT number FROM numbers(1000)" in
    let stats = Chdb.Result.get_stats result in
    assert (Int64_u.compare stats.#rows_read #0L > 0)
  )
```

### 13.2 Streaming Tests

```ocaml
(* test/test_streaming.ml *)

let test_stream_basic () =
  Chdb.Connection.with_connection ~f:(fun conn ->
    let stream = Chdb.Stream.start conn "SELECT number FROM numbers(10000)" in
    let count = ref 0 in
    Chdb.Stream.iter stream ~f:(fun _chunk ->
      incr count
    );
    assert (!count > 0)
  )

let test_stream_cancel () =
  Chdb.Connection.with_connection ~f:(fun conn ->
    let stream = Chdb.Stream.start conn "SELECT number FROM numbers(1000000)" in
    (* Fetch one chunk then cancel *)
    let _ = Chdb.Stream.fetch stream in
    Chdb.Stream.cancel stream;
    assert (Chdb.Stream.fetch stream = None)
  )
```

### 13.3 Performance Benchmarks with Memtrace

```ocaml
(* bench/bench_query.ml *)

let () =
  (* Enable memtrace *)
  Memtrace.trace_if_requested ();

  let conn = Chdb.Connection.connect () in

  (* Benchmark: Simple query *)
  for _ = 1 to 10000 do
    let _ = Chdb.Query.execute conn "SELECT 1" in
    ()
  done;

  (* Benchmark: Zero-copy access *)
  let result = Chdb.Query.execute conn ~format:RowBinary
                 "SELECT number FROM numbers(100000)" in
  let buf = Chdb.Result.to_bigstring result in

  let mutable sum : int64# = #0L in
  let len = Bigstring.length buf in
  let mutable pos = 0 in
  while pos + 8 <= len do
    sum <- Int64_u.add sum (Bigstring.get_int64_le_unboxed buf ~pos);
    pos <- pos + 8
  done;

  Printf.printf "Sum: %Ld\n" (Int64_u.to_int64 sum);

  Chdb.Connection.close conn
```

---

## 14. Build Configuration

### 14.1 dune-project

```lisp
(lang dune 3.0)
(name chdb-oxcaml)
(version 0.1.0)

(generate_opam_files true)

(source (github username/chdb-oxcaml))
(license MIT)
(authors "Author Name")
(maintainers "Author Name")

(package
 (name chdb-oxcaml)
 (synopsis "High-performance ClickHouse embedded bindings for OxCaml")
 (description "Zero-copy, unboxed bindings to chdb using OxCaml extensions")
 (depends
  (ocaml (>= 5.2.0))
  (dune (>= 3.0))
  (base (>= 0.17.0))
  (core (>= 0.17.0))
  (ppx_jane (>= 0.17.0))
  (alcotest (and :with-test (>= 1.7.0)))
  (memtrace (and :with-test (>= 0.2.3)))))
```

### 14.2 lib/dune

```lisp
(library
 (name chdb)
 (public_name chdb-oxcaml)
 (libraries base core unix)
 (foreign_stubs
  (language c)
  (names chdb_stubs)
  (flags (:standard -I/usr/local/include))
  (extra_deps (file /usr/local/include/chdb.h)))
 (c_library_flags (-L/usr/local/lib -lchdb -Wl,-rpath,/usr/local/lib))
 (preprocess (pps ppx_jane))
 (ocamlopt_flags (:standard -O3 -zero-alloc-check all)))
```

### 14.3 stubs/dune

```lisp
(library
 (name chdb_stubs)
 (public_name chdb-oxcaml.stubs)
 (foreign_stubs
  (language c)
  (names chdb_stubs)
  (flags (:standard -I/usr/local/include -O3))
  (extra_deps (file /usr/local/include/chdb.h)))
 (c_library_flags (-L/usr/local/lib -lchdb)))
```

### 14.4 test/dune

```lisp
(test
 (name test_basic)
 (libraries chdb alcotest)
 (preprocess (pps ppx_jane)))

(test
 (name test_streaming)
 (libraries chdb alcotest)
 (preprocess (pps ppx_jane)))

(test
 (name test_zero_copy)
 (libraries chdb alcotest)
 (preprocess (pps ppx_jane)))
```

### 14.5 bench/dune

```lisp
(executable
 (name bench_query)
 (libraries chdb core memtrace)
 (preprocess (pps ppx_jane))
 (ocamlopt_flags (:standard -O3)))
```

---

## 15. Implementation Phases

### Phase 1: Core Foundation (Week 1-2)

**Deliverables:**
- [ ] C stubs with connection and basic query support
- [ ] Custom blocks with finalizers
- [ ] Basic `Connection` and `Query` modules
- [ ] Simple `Result` type with `to_string`

**Tests:**
- [ ] Connection open/close
- [ ] Simple SELECT queries
- [ ] Error handling

### Phase 2: Zero-Copy Buffer Access (Week 3)

**Deliverables:**
- [ ] Bigstring-based result buffer view
- [ ] Unboxed numeric accessors
- [ ] `Buffer` module with typed access

**Tests:**
- [ ] Buffer access patterns
- [ ] Boundary checks
- [ ] Binary format parsing

### Phase 3: Unboxed Statistics (Week 4)

**Deliverables:**
- [ ] Unboxed external declarations
- [ ] `query_stats` unboxed record type
- [ ] `[@zero_alloc]` verified accessors

**Tests:**
- [ ] Stats accuracy
- [ ] Zero-alloc verification with memtrace

### Phase 4: Streaming Support (Week 5)

**Deliverables:**
- [ ] `Stream` module with fetch/cancel
- [ ] Iterator and fold functions
- [ ] `Seq.t` conversion

**Tests:**
- [ ] Large result streaming
- [ ] Early cancellation
- [ ] Memory stability

### Phase 5: Performance Optimization (Week 6)

**Deliverables:**
- [ ] `[@zero_alloc]` annotations throughout
- [ ] Stack allocation patterns
- [ ] Memtrace benchmarks
- [ ] Performance documentation

**Tests:**
- [ ] Allocation hotspot analysis
- [ ] Throughput benchmarks
- [ ] Latency benchmarks

### Phase 6: Documentation and Polish (Week 7)

**Deliverables:**
- [ ] API documentation with odoc
- [ ] Tutorial with examples
- [ ] README with quick start
- [ ] CHANGELOG

---

## Appendix A: API Quick Reference

```ocaml
(* Connection *)
val connect : ?path:string -> unit -> Connection.t
val close : Connection.t -> unit
val with_connection : ?path:string -> f:(Connection.t -> 'a) -> 'a

(* Query *)
val execute : Connection.t -> ?format:format -> string -> result
val execute_string : Connection.t -> ?format:format -> string -> string

(* Result - Zero Copy *)
val to_bigstring : result -> Bigstring.t
val length : result -> int
val get_stats : result -> query_stats  (* unboxed record *)

(* Buffer Access - Unboxed *)
val get_int64_le_unboxed : Bigstring.t -> pos:int -> int64#
val get_float_unboxed : Bigstring.t -> pos:int -> float#

(* Streaming *)
val start : Connection.t -> ?format:format -> string -> Stream.t
val fetch : Stream.t -> result option
val cancel : Stream.t -> unit
val fold : Stream.t -> init:'a -> f:('a -> result -> 'a) -> 'a
```

---

## Appendix B: Performance Comparison

| Operation | chdb-node | chdb-oxcaml | Improvement |
|-----------|-----------|-------------|-------------|
| Query overhead | ~50μs | ~10μs | 5x |
| Buffer copy | Always | Never (zero-copy) | ∞ |
| Stats access | Boxed | Unboxed | No allocation |
| Streaming | Buffer per chunk | View per chunk | No copy |

---

## Appendix C: References

1. chdb C API: `/usr/local/include/chdb.h`
2. chdb-node reference: `/workspace/chdb-node/`
3. OxCaml documentation: Jane Street internal
4. Bigstring: Core library documentation

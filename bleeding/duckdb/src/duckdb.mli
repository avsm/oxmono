(** DuckDB bindings for OxCaml.

    All errors are raised as {!Error} exceptions — no [Result.t] allocations
    on the success path. Database, connection, and result handles are
    GC-managed via custom blocks with finalizers. *)

exception Error of string

(** {1 Handles} *)

type database
type connection
type result

(** {1 Lifecycle} *)

val open_database : ?path:string -> unit -> database
(** Open a DuckDB database.  [?path] defaults to in-memory ([":memory:"]). *)

val connect : database -> connection
(** Create a connection to a database. *)

val close : database -> unit
(** Explicitly close a database.  Also called by the GC finalizer. *)

(** {1 Query execution} *)

val query : connection -> string -> result
(** Execute a SQL query.  Raises {!Error} on failure. *)

(** {1 Result metadata — zero-alloc} *)

external column_count : result -> (int[@untagged])
  = "caml_duckdb_column_count_bc" "caml_duckdb_column_count"
[@@noalloc]

external row_count : result -> (int[@untagged])
  = "caml_duckdb_row_count_bc" "caml_duckdb_row_count"
[@@noalloc]

external rows_changed : result -> (int[@untagged])
  = "caml_duckdb_rows_changed_bc" "caml_duckdb_rows_changed"
[@@noalloc]

val column_name : result -> int -> string

(** {1 Typed column access — unboxed, zero-alloc for numerics} *)

external value_int64 : result -> (int[@untagged]) -> (int[@untagged]) -> (int64[@unboxed])
  = "caml_duckdb_value_int64_bc" "caml_duckdb_value_int64"
[@@noalloc]

external value_int32 : result -> (int[@untagged]) -> (int[@untagged]) -> (int32[@unboxed])
  = "caml_duckdb_value_int32_bc" "caml_duckdb_value_int32"
[@@noalloc]

external value_double : result -> (int[@untagged]) -> (int[@untagged]) -> (float[@unboxed])
  = "caml_duckdb_value_double_bc" "caml_duckdb_value_double"
[@@noalloc]

val value_string : result -> col:int -> row:int -> string

external value_is_null : result -> (int[@untagged]) -> (int[@untagged]) -> bool
  = "caml_duckdb_value_is_null_bc" "caml_duckdb_value_is_null"
[@@noalloc]

(** {1 Library info} *)

val library_version : unit -> string

(** DuckDB bindings for OxCaml.

    All errors are raised as {!Error} exceptions — no [Result.t] allocations
    on the success path.  Database, connection, and result handles are
    GC-managed via custom blocks with finalizers; they may also be closed
    explicitly.

    {2 Architecture}

    The C stubs use direct FFI (no ctypes).  Numeric column accessors are
    unboxed/untagged with [@@noalloc] for zero-allocation hot paths.
    Handles use [Custom_tag] blocks with finalizers and atomic reference
    counting so that, e.g., a database is not freed while connections or
    prepared statements still reference it.

    {2 Thread safety}

    Long-running operations ([open_database], [query], [execute]) release
    the OCaml runtime lock via [caml_enter_blocking_section] so other
    OCaml threads can run concurrently.  Individual handles are {b not}
    safe to share across OCaml domains without external synchronisation. *)

exception Error of string
(** Raised on all DuckDB errors: failed queries, invalid parameters,
    closed handles, bind failures, etc. *)

(** {1 Handles} *)

type database
(** A DuckDB database.  GC-finalised; may also be closed explicitly
    with {!close}. *)

type connection
(** A connection to a {!database}.  Holds a reference to the database
    to prevent premature collection.  GC-finalised. *)

type result
(** The result of a query or prepared-statement execution.  GC-finalised.
    Access rows via the {!val:value_int64}/{!val:value_string}/… accessors
    or via the columnar {!Data_chunk} API. *)

(** {1 Lifecycle} *)

val open_database : ?path:string -> unit -> database
(** Open a DuckDB database.  [?path] defaults to in-memory ([":memory:"]).
    Raises {!Error} on failure. *)

val connect : database -> connection
(** Create a connection to a database.  Raises {!Error} if the database
    has been closed. *)

val close : database -> unit
(** Explicitly close a database.  Idempotent — also called by the GC
    finalizer.  Outstanding connections and prepared statements that
    reference this database continue to prevent the underlying storage
    from being freed until they are themselves collected. *)

(** {1 Query execution} *)

val query : connection -> string -> result
(** Execute a SQL query.  Raises {!Error} on failure. *)

(** {1 Prepared statements} *)

type stmt
(** A prepared statement.  Holds a reference to the database (not the
    connection) to prevent premature collection.  GC-finalised. *)

val prepare : connection -> string -> stmt
(** Prepare a SQL statement.  Raises {!Error} on failure. *)

val execute : stmt -> result
(** Execute a prepared statement.  The statement can be re-executed
    after calling {!clear_bindings} and binding new values.
    Raises {!Error} on failure. *)

(** {2 Parameter binding}

    Bind parameters by 1-based index.  All raise {!Error} if the
    statement has been destroyed or the bind operation fails. *)

val bind_bool : stmt -> int -> bool -> unit
(** [bind_bool stmt idx v] binds a boolean at parameter [idx]. *)

val bind_int32 : stmt -> int -> int32 -> unit
(** [bind_int32 stmt idx v] binds a 32-bit integer at parameter [idx]. *)

val bind_int64 : stmt -> int -> int64 -> unit
(** [bind_int64 stmt idx v] binds a 64-bit integer at parameter [idx]. *)

val bind_double : stmt -> int -> float -> unit
(** [bind_double stmt idx v] binds a double-precision float at parameter [idx]. *)

val bind_string : stmt -> int -> string -> unit
(** [bind_string stmt idx v] binds a VARCHAR string at parameter [idx].
    The string is copied by DuckDB. *)

val bind_blob : stmt -> int -> bytes -> unit
(** [bind_blob stmt idx v] binds a BLOB at parameter [idx].
    The bytes are copied by DuckDB. *)

val bind_null : stmt -> int -> unit
(** [bind_null stmt idx] binds NULL at parameter [idx]. *)

val clear_bindings : stmt -> unit
(** Reset all parameter bindings.  Raises {!Error} if the statement
    has been destroyed. *)

external param_count : stmt -> (int[@untagged])
  = "caml_duckdb_nparams_bc" "caml_duckdb_nparams"
[@@noalloc]
(** Number of parameters in the prepared statement.  Zero-alloc. *)

(** {1 Result metadata — zero-alloc} *)

external column_count : result -> (int[@untagged])
  = "caml_duckdb_column_count_bc" "caml_duckdb_column_count"
[@@noalloc]
(** Number of columns in the result. *)

external row_count : result -> (int[@untagged])
  = "caml_duckdb_row_count_bc" "caml_duckdb_row_count"
[@@noalloc]
(** Number of rows in the result. *)

external rows_changed : result -> (int[@untagged])
  = "caml_duckdb_rows_changed_bc" "caml_duckdb_rows_changed"
[@@noalloc]
(** Number of rows changed by an INSERT/UPDATE/DELETE statement. *)

val column_name : result -> int -> string
(** [column_name result col] returns the name of column [col].
    Raises {!Error} if [col] is out of range. *)

(** {1 Column types} *)

module Type : sig
  (** DuckDB logical column types.

      {b Note:} The C API does not yet expose all internal types.
      In particular, [GEOMETRY] columns currently report as [Invalid]
      (see {!Geometry} for the workaround). *)

  type t =
    | Invalid       (** Unknown or unsupported type *)
    | Boolean       (** 1-byte boolean *)
    | Tinyint       (** 8-bit signed integer *)
    | Smallint      (** 16-bit signed integer *)
    | Integer       (** 32-bit signed integer *)
    | Bigint        (** 64-bit signed integer *)
    | UTinyint      (** 8-bit unsigned integer *)
    | USmallint     (** 16-bit unsigned integer *)
    | UInteger      (** 32-bit unsigned integer *)
    | UBigint       (** 64-bit unsigned integer *)
    | Float         (** 32-bit IEEE 754 float *)
    | Double        (** 64-bit IEEE 754 double *)
    | Timestamp     (** Microsecond timestamp *)
    | Date          (** Calendar date *)
    | Time          (** Time of day *)
    | Interval      (** Date/time interval *)
    | Hugeint       (** 128-bit signed integer *)
    | Varchar       (** Variable-length string *)
    | Blob          (** Variable-length binary data *)
    | Decimal       (** Fixed-point decimal *)
    | Timestamp_s   (** Second-precision timestamp *)
    | Timestamp_ms  (** Millisecond-precision timestamp *)
    | Timestamp_ns  (** Nanosecond-precision timestamp *)
    | Enum          (** Enumeration type *)
    | List          (** Variable-length list *)
    | Struct        (** Named struct *)
    | Map           (** Key-value map *)
    | UUID          (** 128-bit UUID *)
    | Union         (** Tagged union *)
    | Bit           (** Bit string *)
    | Time_tz       (** Time with time zone *)
    | Timestamp_tz  (** Timestamp with time zone *)
    | UHugeint      (** 128-bit unsigned integer *)
    | Array         (** Fixed-length array *)
    | Geometry      (** Geometry (internal id 60, not yet in C API) *)
    | Unknown of int (** Unrecognised type id *)

  val to_string : t -> string
  (** SQL-style name, e.g. ["INTEGER"], ["VARCHAR"], ["TIMESTAMP WITH TIME ZONE"]. *)
end

val column_type : result -> int -> Type.t
(** [column_type result col] returns the type of column [col]. *)

(** {1 Typed column access}

    Row/column accessors for materialised results.  The numeric accessors
    are unboxed and [{@@noalloc}] — they do not allocate on the OCaml heap.
    Column and row indices are 0-based. *)

external value_int64 : result -> (int[@untagged]) -> (int[@untagged]) -> (int64[@unboxed])
  = "caml_duckdb_value_int64_bc" "caml_duckdb_value_int64"
[@@noalloc]
(** [value_int64 result col row] reads a 64-bit integer.  Works for any
    integer column type (DuckDB widens automatically). *)

external value_int32 : result -> (int[@untagged]) -> (int[@untagged]) -> (int32[@unboxed])
  = "caml_duckdb_value_int32_bc" "caml_duckdb_value_int32"
[@@noalloc]
(** [value_int32 result col row] reads a 32-bit integer. *)

external value_double : result -> (int[@untagged]) -> (int[@untagged]) -> (float[@unboxed])
  = "caml_duckdb_value_double_bc" "caml_duckdb_value_double"
[@@noalloc]
(** [value_double result col row] reads a double-precision float. *)

val value_string : result -> col:int -> row:int -> string
(** [value_string result ~col ~row] reads a string value.  Returns [""]
    for NULL.  Allocates a fresh OCaml string (DuckDB's internal string
    is freed). *)

external value_is_null : result -> (int[@untagged]) -> (int[@untagged]) -> bool
  = "caml_duckdb_value_is_null_bc" "caml_duckdb_value_is_null"
[@@noalloc]
(** [value_is_null result col row] checks whether a cell is NULL.
    Zero-alloc. *)

(** {1 Data chunk API — columnar zero-copy access}

    DuckDB stores query results as columnar chunks of up to
    {!vector_size} rows (typically 2048).  The chunk API provides
    zero-copy access to the underlying memory via Bigarray views.

    The typical iteration pattern is:
    {[
      let nchunks = Data_chunk.chunk_count result in
      for i = 0 to nchunks - 1 do
        let chunk = Data_chunk.get_chunk result i in
        for row = 0 to Data_chunk.size chunk - 1 do
          if Vector.is_valid chunk col row then
            (* process element *)
        done
      done
    ]} *)

module Data_chunk : sig
  type t
  (** A data chunk.  GC-managed; destroyed by the finalizer. *)

  val chunk_count : result -> int
  (** Number of chunks in a result. *)

  val get_chunk : result -> int -> t
  (** [get_chunk result i] returns chunk [i].
      Raises {!Error} on invalid index. *)

  external size : t -> (int[@untagged])
    = "caml_duckdb_chunk_get_size_bc" "caml_duckdb_chunk_get_size"
  [@@noalloc]
  (** Number of rows in this chunk. *)

  external column_count : t -> (int[@untagged])
    = "caml_duckdb_chunk_get_column_count_bc" "caml_duckdb_chunk_get_column_count"
  [@@noalloc]
  (** Number of columns in this chunk. *)
end

module Vector : sig
  (** {2 Raw data access}

      Returns a Bigarray view over DuckDB's internal memory.  The view
      is valid only while the parent {!Data_chunk.t} is alive — do not
      let the chunk be collected while using the Bigarray. *)

  val data : Data_chunk.t -> col:int
    -> (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  (** Raw column data as a byte array.  Interpret by element size:
      - [INTEGER]: 4 bytes per element
      - [BIGINT]/[DOUBLE]/[TIMESTAMP]: 8 bytes per element
      - [VARCHAR]: 16 bytes per element ([duckdb_string_t]) *)

  val validity : Data_chunk.t -> col:int
    -> (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t option
  (** Validity bitmap.  [None] means all values are valid.
      Each bit corresponds to a row: 1 = valid, 0 = null. *)

  (** {2 Typed element access} *)

  val get_string : Data_chunk.t -> col:int -> row:int -> string
  (** Extract a string from a [VARCHAR] vector.  Copies the string
      into the OCaml heap. *)

  (** {2 Null checking — zero-alloc} *)

  external is_valid : Data_chunk.t -> (int[@untagged]) -> (int[@untagged]) -> bool
    = "caml_duckdb_vector_is_valid_bc" "caml_duckdb_vector_is_valid"
  [@@noalloc]
  (** [is_valid chunk col row] — [true] if the value is not null. *)
end

(** {1 Constants} *)

external vector_size : unit -> (int[@untagged])
  = "caml_duckdb_vector_size_bc" "caml_duckdb_vector_size"
[@@noalloc]
(** The internal DuckDB vector size (typically 2048).  This is the
    maximum number of rows in a single {!Data_chunk.t}. *)

(** {1 Extension loading} *)

val install_extension : connection -> string -> unit
(** [install_extension conn "spatial"] runs [INSTALL spatial].
    Requires network access.  Raises {!Error} on failure. *)

val load_extension : connection -> string -> unit
(** [load_extension conn "spatial"] runs [LOAD spatial].
    Raises {!Error} if the extension is not installed. *)

(** {1 Geometry}

    OCaml-native WKT (Well-Known Text) parser and serializer for
    DuckDB's [GEOMETRY] type.  [GEOMETRY] is a core DuckDB type
    since v1.5; WKT strings can be cast to/from [GEOMETRY] in SQL
    without any extension:

    {[SELECT 'POINT (1 2)'::GEOMETRY]}

    The [ST_*] spatial functions require the spatial extension:
    {[
      install_extension conn "spatial";
      load_extension conn "spatial"
    ]}

    {b Limitation:} The DuckDB C API does not yet expose the
    [GEOMETRY] type id, so {!column_type} reports geometry columns
    as {!Type.Invalid}. *)

module Geometry : sig
  (** {2 Types} *)

  type geom_type =
    | Point
    | Linestring
    | Polygon
    | Multi_point
    | Multi_linestring
    | Multi_polygon
    | Geometry_collection
  (** Simple enum for classifying a geometry value without
      pattern-matching. *)

  type coord = { x : float; y : float }
  (** A 2D coordinate. *)

  type t =
    | Point of coord
    | Linestring of coord array
    | Polygon of coord array array
        (** Outer ring followed by zero or more holes. *)
    | Multi_point of coord array
    | Multi_linestring of coord array array
    | Multi_polygon of coord array array array
    | Geometry_collection of t list
  (** OGC Simple Features geometry, supporting all seven core types. *)

  (** {2 WKT conversion} *)

  val to_wkt : t -> string
  (** Serialize to Well-Known Text (e.g. ["POINT (1 2)"]). *)

  val of_wkt : string -> t
  (** Parse Well-Known Text.  Case-insensitive.
      Raises {!Error} on invalid input. *)

  (** {2 Constructors} *)

  val point : float -> float -> t
  (** [point x y] creates [Point {x; y}]. *)

  val linestring : coord list -> t
  (** [linestring coords] creates a [Linestring]. *)

  val polygon : coord list list -> t
  (** [polygon rings] creates a [Polygon].  The first element is the
      outer ring; subsequent elements are holes. *)

  (** {2 Inspection} *)

  val geom_type : t -> geom_type
  (** Return the geometry classification. *)

  val geom_type_to_string : geom_type -> string
  (** OGC name, e.g. ["POINT"], ["MULTIPOLYGON"]. *)

  (** {2 SQL integration} *)

  val to_sql_literal : t -> string
  (** Returns e.g. ['POINT (1 2)'::GEOMETRY] — a SQL expression that
      can be interpolated directly into a query string.  The WKT is
      generated from typed data, so there is no injection risk. *)

  val of_result : result -> col:int -> row:int -> t
  (** Extract a geometry from a query result by parsing the [VARCHAR]
      representation.  The query should cast the column:
      [SELECT g::VARCHAR FROM ...].  Raises {!Error} if parsing fails. *)
end

(** {1 Library info} *)

val library_version : unit -> string
(** The DuckDB library version string, e.g. ["v1.5.0-dev8631"]. *)

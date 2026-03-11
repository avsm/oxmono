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

(** {1 Prepared statements} *)

type stmt

val prepare : connection -> string -> stmt
(** Prepare a SQL statement.  Raises {!Error} on failure. *)

val execute : stmt -> result
(** Execute a prepared statement.  Raises {!Error} on failure. *)

val bind_bool : stmt -> int -> bool -> unit
val bind_int32 : stmt -> int -> int32 -> unit
val bind_int64 : stmt -> int -> int64 -> unit
val bind_double : stmt -> int -> float -> unit
val bind_string : stmt -> int -> string -> unit
val bind_blob : stmt -> int -> bytes -> unit
val bind_null : stmt -> int -> unit
val clear_bindings : stmt -> unit

external param_count : stmt -> (int[@untagged])
  = "caml_duckdb_nparams_bc" "caml_duckdb_nparams"
[@@noalloc]

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

(** {1 Column types} *)

module Type : sig
  type t =
    | Invalid
    | Boolean
    | Tinyint
    | Smallint
    | Integer
    | Bigint
    | UTinyint
    | USmallint
    | UInteger
    | UBigint
    | Float
    | Double
    | Timestamp
    | Date
    | Time
    | Interval
    | Hugeint
    | Varchar
    | Blob
    | Decimal
    | Timestamp_s
    | Timestamp_ms
    | Timestamp_ns
    | Enum
    | List
    | Struct
    | Map
    | UUID
    | Union
    | Bit
    | Time_tz
    | Timestamp_tz
    | UHugeint
    | Array
    | Geometry
    | Unknown of int

  val to_string : t -> string
end

val column_type : result -> int -> Type.t

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

(** {1 Data chunk API — columnar zero-copy access}

    DuckDB stores query results as columnar chunks of up to 2048 rows.
    The chunk API provides zero-copy access to the underlying memory
    via Bigarray views. *)

module Data_chunk : sig
  type t
  (** A data chunk.  GC-managed; destroyed by the finalizer. *)

  val chunk_count : result -> int
  (** Number of chunks in a result. *)

  val get_chunk : result -> int -> t
  (** Get a chunk by index.  Raises {!Error} on invalid index. *)

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
      is valid only while the parent {!Data_chunk.t} is alive. *)

  val data : Data_chunk.t -> col:int
    -> (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  (** Raw column data as a byte array.  Interpret by element size:
      - INTEGER: 4 bytes per element
      - BIGINT/DOUBLE/TIMESTAMP: 8 bytes per element
      - VARCHAR: 16 bytes per element (duckdb_string_t) *)

  val validity : Data_chunk.t -> col:int
    -> (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t option
  (** Validity bitmap.  [None] means all values are valid.
      Each bit corresponds to a row: 1 = valid, 0 = null. *)

  (** {2 Typed element access} *)

  val get_string : Data_chunk.t -> col:int -> row:int -> string
  (** Extract a string from a VARCHAR vector.  Copies the string. *)

  (** {2 Null checking — zero-alloc} *)

  external is_valid : Data_chunk.t -> (int[@untagged]) -> (int[@untagged]) -> bool
    = "caml_duckdb_vector_is_valid_bc" "caml_duckdb_vector_is_valid"
  [@@noalloc]
  (** Check whether a row is valid (not null).  Zero-alloc. *)
end

(** {1 Constants} *)

external vector_size : unit -> (int[@untagged])
  = "caml_duckdb_vector_size_bc" "caml_duckdb_vector_size"
[@@noalloc]
(** The internal DuckDB vector size (typically 2048). *)

(** {1 Extension loading} *)

val install_extension : connection -> string -> unit
(** [install_extension conn "spatial"] runs [INSTALL spatial]. *)

val load_extension : connection -> string -> unit
(** [load_extension conn "spatial"] runs [LOAD spatial]. *)

(** {1 Geometry}

    OCaml-native WKT parser/serializer for DuckDB's GEOMETRY type.
    GEOMETRY is a core DuckDB type since v1.5.  WKT strings can be
    cast to/from GEOMETRY in SQL.  The ST_* functions require the
    spatial extension ([install_extension conn "spatial"]). *)

module Geometry : sig
  type geom_type =
    | Point
    | Linestring
    | Polygon
    | Multi_point
    | Multi_linestring
    | Multi_polygon
    | Geometry_collection

  type coord = { x : float; y : float }

  type t =
    | Point of coord
    | Linestring of coord array
    | Polygon of coord array array
    | Multi_point of coord array
    | Multi_linestring of coord array array
    | Multi_polygon of coord array array array
    | Geometry_collection of t list

  (** {2 WKT conversion} *)

  val to_wkt : t -> string
  (** Serialize to Well-Known Text. *)

  val of_wkt : string -> t
  (** Parse Well-Known Text.  Raises {!Error} on invalid input. *)

  (** {2 Constructors} *)

  val point : float -> float -> t
  val linestring : coord list -> t
  val polygon : coord list list -> t

  (** {2 Inspection} *)

  val geom_type : t -> geom_type
  val geom_type_to_string : geom_type -> string

  (** {2 SQL integration} *)

  val to_sql_literal : t -> string
  (** Returns e.g. ['POINT (1 2)'::GEOMETRY] for use in SQL. *)

  val of_result : result -> col:int -> row:int -> t
  (** Extract a GEOMETRY value from a query result by parsing
      the VARCHAR representation.  The query should cast the
      column to VARCHAR: [SELECT g::VARCHAR FROM ...]. *)
end

(** {1 Library info} *)

val library_version : unit -> string

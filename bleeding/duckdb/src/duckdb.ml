exception Error of string

let () = Callback.Safe.register_exception "Duckdb.Error" (Error "")

type database
type connection
type result

(* ── Init ────────────────────────────────────────────────────────── *)

external init : unit -> unit = "caml_duckdb_init"
let () = init ()

(* ── Lifecycle ───────────────────────────────────────────────────── *)

external open_raw : string option -> database = "caml_duckdb_open"
external close : database -> unit = "caml_duckdb_close"
external connect : database -> connection = "caml_duckdb_connect"

let open_database ?path () = open_raw path

(* ── Query ───────────────────────────────────────────────────────── *)

external query : connection -> string -> result = "caml_duckdb_query"

(* ── Prepared statements ─────────────────────────────────────────── *)

type stmt

external prepare : connection -> string -> stmt = "caml_duckdb_prepare"
external execute : stmt -> result = "caml_duckdb_execute_prepared"

external bind_bool_raw : stmt -> (int[@untagged]) -> bool -> unit
  = "caml_duckdb_bind_bool_bc" "caml_duckdb_bind_bool"
[@@noalloc]

external bind_int32_raw : stmt -> (int[@untagged]) -> (int32[@unboxed]) -> unit
  = "caml_duckdb_bind_int32_bc" "caml_duckdb_bind_int32"
[@@noalloc]

external bind_int64_raw : stmt -> (int[@untagged]) -> (int64[@unboxed]) -> unit
  = "caml_duckdb_bind_int64_bc" "caml_duckdb_bind_int64"
[@@noalloc]

external bind_double_raw : stmt -> (int[@untagged]) -> (float[@unboxed]) -> unit
  = "caml_duckdb_bind_double_bc" "caml_duckdb_bind_double"
[@@noalloc]

external bind_string : stmt -> int -> string -> unit
  = "caml_duckdb_bind_string"

external bind_blob : stmt -> int -> bytes -> unit
  = "caml_duckdb_bind_blob"

external bind_null_raw : stmt -> (int[@untagged]) -> unit
  = "caml_duckdb_bind_null_bc" "caml_duckdb_bind_null"
[@@noalloc]

external clear_bindings : stmt -> unit = "caml_duckdb_clear_bindings"

external param_count : stmt -> (int[@untagged])
  = "caml_duckdb_nparams_bc" "caml_duckdb_nparams"
[@@noalloc]

let bind_bool stmt idx v = bind_bool_raw stmt idx v
let bind_int32 stmt idx v = bind_int32_raw stmt idx v
let bind_int64 stmt idx v = bind_int64_raw stmt idx v
let bind_double stmt idx v = bind_double_raw stmt idx v
let bind_null stmt idx = bind_null_raw stmt idx

(* ── Result metadata ─────────────────────────────────────────────── *)

external column_count : result -> (int[@untagged])
  = "caml_duckdb_column_count_bc" "caml_duckdb_column_count"
[@@noalloc]

external row_count : result -> (int[@untagged])
  = "caml_duckdb_row_count_bc" "caml_duckdb_row_count"
[@@noalloc]

external rows_changed : result -> (int[@untagged])
  = "caml_duckdb_rows_changed_bc" "caml_duckdb_rows_changed"
[@@noalloc]

external column_name : result -> int -> string = "caml_duckdb_column_name"

(* ── Column types ────────────────────────────────────────────────── *)

module Type = struct
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
    | Unknown of int

  let of_int = function
    | 0 -> Invalid
    | 1 -> Boolean
    | 2 -> Tinyint
    | 3 -> Smallint
    | 4 -> Integer
    | 5 -> Bigint
    | 6 -> UTinyint
    | 7 -> USmallint
    | 8 -> UInteger
    | 9 -> UBigint
    | 10 -> Float
    | 11 -> Double
    | 12 -> Timestamp
    | 13 -> Date
    | 14 -> Time
    | 15 -> Interval
    | 16 -> Hugeint
    | 17 -> Varchar
    | 18 -> Blob
    | 19 -> Decimal
    | 20 -> Timestamp_s
    | 21 -> Timestamp_ms
    | 22 -> Timestamp_ns
    | 23 -> Enum
    | 24 -> List
    | 25 -> Struct
    | 26 -> Map
    | 27 -> UUID
    | 28 -> Union
    | 29 -> Bit
    | 30 -> Time_tz
    | 31 -> Timestamp_tz
    | 32 -> UHugeint
    | 33 -> Array
    | n -> Unknown n

  let to_string = function
    | Invalid -> "INVALID"
    | Boolean -> "BOOLEAN"
    | Tinyint -> "TINYINT"
    | Smallint -> "SMALLINT"
    | Integer -> "INTEGER"
    | Bigint -> "BIGINT"
    | UTinyint -> "UTINYINT"
    | USmallint -> "USMALLINT"
    | UInteger -> "UINTEGER"
    | UBigint -> "UBIGINT"
    | Float -> "FLOAT"
    | Double -> "DOUBLE"
    | Timestamp -> "TIMESTAMP"
    | Date -> "DATE"
    | Time -> "TIME"
    | Interval -> "INTERVAL"
    | Hugeint -> "HUGEINT"
    | Varchar -> "VARCHAR"
    | Blob -> "BLOB"
    | Decimal -> "DECIMAL"
    | Timestamp_s -> "TIMESTAMP_S"
    | Timestamp_ms -> "TIMESTAMP_MS"
    | Timestamp_ns -> "TIMESTAMP_NS"
    | Enum -> "ENUM"
    | List -> "LIST"
    | Struct -> "STRUCT"
    | Map -> "MAP"
    | UUID -> "UUID"
    | Union -> "UNION"
    | Bit -> "BIT"
    | Time_tz -> "TIME WITH TIME ZONE"
    | Timestamp_tz -> "TIMESTAMP WITH TIME ZONE"
    | UHugeint -> "UHUGEINT"
    | Array -> "ARRAY"
    | Unknown n -> Printf.sprintf "UNKNOWN(%d)" n
end

external column_type_raw : result -> (int[@untagged]) -> (int[@untagged])
  = "caml_duckdb_column_type_bc" "caml_duckdb_column_type"
[@@noalloc]

let column_type result col = Type.of_int (column_type_raw result col)

(* ── Typed column access ─────────────────────────────────────────── *)

external value_int64 : result -> (int[@untagged]) -> (int[@untagged]) -> (int64[@unboxed])
  = "caml_duckdb_value_int64_bc" "caml_duckdb_value_int64"
[@@noalloc]

external value_int32 : result -> (int[@untagged]) -> (int[@untagged]) -> (int32[@unboxed])
  = "caml_duckdb_value_int32_bc" "caml_duckdb_value_int32"
[@@noalloc]

external value_double : result -> (int[@untagged]) -> (int[@untagged]) -> (float[@unboxed])
  = "caml_duckdb_value_double_bc" "caml_duckdb_value_double"
[@@noalloc]

external value_string : result -> col:int -> row:int -> string
  = "caml_duckdb_value_string"

external value_is_null : result -> (int[@untagged]) -> (int[@untagged]) -> bool
  = "caml_duckdb_value_is_null_bc" "caml_duckdb_value_is_null"
[@@noalloc]

(* ── Data chunk API ──────────────────────────────────────────────── *)

module Data_chunk = struct
  type t

  external chunk_count : result -> (int[@untagged])
    = "caml_duckdb_result_chunk_count_bc" "caml_duckdb_result_chunk_count"
  [@@noalloc]

  let chunk_count r = chunk_count r

  external get_chunk : result -> int -> t = "caml_duckdb_result_get_chunk"

  external size : t -> (int[@untagged])
    = "caml_duckdb_chunk_get_size_bc" "caml_duckdb_chunk_get_size"
  [@@noalloc]

  external column_count : t -> (int[@untagged])
    = "caml_duckdb_chunk_get_column_count_bc" "caml_duckdb_chunk_get_column_count"
  [@@noalloc]
end

module Vector = struct
  external data : Data_chunk.t -> col:int
    -> (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
    = "caml_duckdb_vector_data"

  external validity : Data_chunk.t -> col:int
    -> (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t option
    = "caml_duckdb_vector_validity"

  external get_string : Data_chunk.t -> col:int -> row:int -> string
    = "caml_duckdb_vector_get_string"

  external is_valid : Data_chunk.t -> (int[@untagged]) -> (int[@untagged]) -> bool
    = "caml_duckdb_vector_is_valid_bc" "caml_duckdb_vector_is_valid"
  [@@noalloc]
end

external vector_size : unit -> (int[@untagged])
  = "caml_duckdb_vector_size_bc" "caml_duckdb_vector_size"
[@@noalloc]

(* ── Library info ────────────────────────────────────────────────── *)

external library_version : unit -> string = "caml_duckdb_library_version"

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

(* ── Library info ────────────────────────────────────────────────── *)

external library_version : unit -> string = "caml_duckdb_library_version"

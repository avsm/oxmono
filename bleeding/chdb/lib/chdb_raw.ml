type connection_handle
type result_handle

(* -- Connection -- *)

external connect : string array -> connection_handle or_null
  = "caml_chdb_connect"

external close_conn : connection_handle -> unit
  = "caml_chdb_close_conn"

external conn_is_open : connection_handle -> bool
  = "caml_chdb_conn_is_open" [@@noalloc]

(* -- Query -- *)

external query : connection_handle -> string -> string -> result_handle or_null
  = "caml_chdb_query"

(* -- Result data -- *)

external result_to_string : result_handle -> string
  = "caml_chdb_result_to_string"

external result_to_bigstring :
  result_handle ->
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  = "caml_chdb_result_to_bigstring"

external result_length : result_handle -> int
  = "caml_chdb_result_length" [@@noalloc]

external result_error : result_handle -> string
  = "caml_chdb_result_error"

external result_has_error : result_handle -> bool
  = "caml_chdb_result_has_error" [@@noalloc]

(* -- Unboxed statistics -- *)

external result_elapsed : result_handle -> (float[@unboxed])
  = "caml_chdb_result_elapsed_boxed" "caml_chdb_result_elapsed_unboxed"
  [@@noalloc]

external result_rows_read : result_handle -> (int64[@unboxed])
  = "caml_chdb_result_rows_read_boxed" "caml_chdb_result_rows_read_unboxed"
  [@@noalloc]

external result_bytes_read : result_handle -> (int64[@unboxed])
  = "caml_chdb_result_bytes_read_boxed" "caml_chdb_result_bytes_read_unboxed"
  [@@noalloc]

external result_storage_rows_read : result_handle -> (int64[@unboxed])
  = "caml_chdb_result_storage_rows_read_boxed"
    "caml_chdb_result_storage_rows_read_unboxed"
  [@@noalloc]

external result_storage_bytes_read : result_handle -> (int64[@unboxed])
  = "caml_chdb_result_storage_bytes_read_boxed"
    "caml_chdb_result_storage_bytes_read_unboxed"
  [@@noalloc]

(* -- Result lifecycle -- *)

external destroy_result : result_handle -> unit
  = "caml_chdb_destroy_result"

(* -- Streaming -- *)

external stream_query :
  connection_handle -> string -> string -> result_handle or_null
  = "caml_chdb_stream_query"

external stream_fetch :
  connection_handle -> result_handle -> result_handle or_null
  = "caml_chdb_stream_fetch"

external stream_cancel :
  connection_handle -> result_handle -> unit
  = "caml_chdb_stream_cancel"

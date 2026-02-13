(** Raw C FFI bindings to the chdb library.

    {b Internal module.}  Application code should use
    {!Chdb_connection}, {!Chdb_query}, {!Chdb_result}, and
    {!Chdb_stream} instead.  This module is exposed only for use
    by other chdb library modules. *)

(** {2 Opaque handles} *)

type connection_handle
(** A pointer to the C [chdb_connection] struct, wrapped in a
    custom block with a release finalizer. *)

type result_handle
(** A pointer to a C [chdb_result] struct, wrapped in a
    custom block with a destroy finalizer. *)

(** {2 Connection lifecycle} *)

val connect : string array -> connection_handle or_null
val close_conn : connection_handle -> unit
val conn_is_open : connection_handle -> bool

(** {2 Query execution} *)

val query : connection_handle -> string -> string -> result_handle or_null
(** [query handle sql format] executes [sql] with the given
    output format string.  Returns [Null] on failure. *)

(** {2 Result data access} *)

val result_to_string : result_handle -> string
val result_to_bigstring :
  result_handle ->
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
val result_length : result_handle -> int
val result_error : result_handle -> string
val result_has_error : result_handle -> bool

(** {2 Unboxed statistics}

    These accessors use [[@@noalloc]] and unboxed returns so that
    reading hot-path metrics does not trigger GC allocation. *)

val result_elapsed : result_handle -> float
val result_rows_read : result_handle -> int64
val result_bytes_read : result_handle -> int64
val result_storage_rows_read : result_handle -> int64
val result_storage_bytes_read : result_handle -> int64

(** {2 Result lifecycle} *)

val destroy_result : result_handle -> unit

(** {2 Streaming} *)

val stream_query :
  connection_handle -> string -> string -> result_handle or_null
val stream_fetch :
  connection_handle -> result_handle -> result_handle or_null
val stream_cancel :
  connection_handle -> result_handle -> unit

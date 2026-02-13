(** Query results with zero-copy buffer access.

    A {!t} wraps a C [chdb_result] struct managed via custom blocks
    with finalizers.  {!to_bigstring} wraps the C buffer with
    [CAML_BA_EXTERNAL] so no copy occurs; the bigstring is only
    valid while the result is alive.

    Results returned by {!Chdb_query.execute} must eventually be
    destroyed via {!destroy} (or use {!Chdb_query.with_result}
    for automatic cleanup). *)

type t
(** An opaque query result backed by C memory. *)

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
(** Bigstring type alias for zero-copy buffer access. *)

(** {2 Data access} *)

val to_string : t -> string
(** [to_string t] copies the result data into an OCaml string. *)

val to_bigstring : t -> bigstring
(** [to_bigstring t] returns a zero-copy view of the result buffer.
    {b Warning:} the bigstring becomes invalid after {!destroy}. *)

val length : t -> int
(** [length t] is the result data size in bytes. *)

val is_empty : t -> bool
(** [is_empty t] is [true] when the result has no data. *)

(** {2 Error inspection} *)

val error : t -> string option
(** [error t] is [Some msg] if the result carries an error,
    or [None] if it succeeded. *)

val has_error : t -> bool
(** [has_error t] is [true] if the result contains an error. *)

(** {2 Statistics} *)

val elapsed : t -> float
(** [elapsed t] is the query execution time in seconds. *)

val rows_read : t -> int64
(** [rows_read t] is the number of rows in the result. *)

val bytes_read : t -> int64
(** [bytes_read t] is the byte count in ClickHouse internal format. *)

val storage_rows_read : t -> int64
(** [storage_rows_read t] is the total rows scanned from storage. *)

val storage_bytes_read : t -> int64
(** [storage_bytes_read t] is the total bytes scanned from storage. *)

val get_stats : t -> Chdb_stats.t
(** [get_stats t] reads all statistics into a {!Chdb_stats.t} record. *)

val get_stats_local : t -> local_ Chdb_stats.t
(** [get_stats_local t] reads statistics into a locally-allocated
    record, avoiding minor-heap allocation. *)

(** {2 Lifecycle} *)

val destroy : t -> unit
(** [destroy t] explicitly releases the underlying C memory.
    After this call the result and any bigstring views are
    invalid.  Idempotent. *)

(**/**)

val of_raw : Chdb_raw.result_handle -> t
val check_error : t -> unit

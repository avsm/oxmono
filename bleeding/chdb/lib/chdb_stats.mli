(** Query execution statistics.

    All fields are obtained via unboxed C externals ([[@@noalloc]])
    to avoid allocation on the read path.

    {[
      Query.with_result conn sql ~f:(fun r ->
        let stats = Result.get_stats r in
        Printf.printf "elapsed: %.3fs, rows: %Ld\n"
          stats.elapsed stats.rows_read)
    ]} *)

type t = {
  elapsed : float;            (** Wall-clock execution time in seconds. *)
  rows_read : int64;          (** Number of rows in the result set. *)
  bytes_read : int64;         (** Bytes occupied in ClickHouse internal format. *)
  storage_rows_read : int64;  (** Total rows scanned from storage. *)
  storage_bytes_read : int64; (** Total bytes scanned from storage. *)
}

val of_result_handle : Chdb_raw.result_handle -> t
(** [of_result_handle h] reads all statistics from a raw result
    handle into a heap-allocated record. *)

val of_result_handle_local : Chdb_raw.result_handle -> local_ t
(** [of_result_handle_local h] reads statistics into a
    locally-allocated record.  The record shell lives on the
    caller's stack frame, avoiding minor-heap pressure.  Best
    used with immediate destructuring:

    {[let { elapsed; rows_read; _ } = Stats.of_result_handle_local h in ...]} *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] pretty-prints the statistics record. *)

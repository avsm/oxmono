(** Query execution against a chdb connection.

    All functions acquire the connection mutex internally and
    release the OCaml domain lock during the C call, so they
    are safe to call from multiple threads or domains.

    {2 Quick reference}

    {[
      (* Fire-and-forget *)
      Query.execute_ignore conn "CREATE TABLE t (x UInt32) ENGINE = Memory";

      (* Get result as string *)
      let csv = Query.execute_string conn ~format:CSV "SELECT * FROM t" in

      (* Zero-copy binary access *)
      Query.with_buffer conn ~format:RowBinary
        "SELECT toUInt64(x) FROM t"
        ~f:(fun buf -> Binary.sum_uint64 buf)
    ]} *)

val execute :
  Chdb_connection.t -> ?format:Chdb_format.t -> string -> Chdb_result.t
(** [execute conn ?format sql] executes [sql] and returns the raw
    result.  The caller is responsible for calling
    {!Chdb_result.destroy} when done.

    @param format Output format (default: {!Chdb_format.CSV}).
    @raise Chdb_error.E on query failure. *)

val execute_string :
  Chdb_connection.t -> ?format:Chdb_format.t -> string -> string
(** [execute_string conn ?format sql] executes [sql] and returns
    the result as an OCaml string.  The underlying C result is
    destroyed before returning. *)

val execute_bigstring :
  Chdb_connection.t -> ?format:Chdb_format.t -> string ->
  Chdb_result.t * Chdb_result.bigstring
(** [execute_bigstring conn ?format sql] returns both the result
    handle and a zero-copy bigstring view.  The caller must
    eventually call {!Chdb_result.destroy}; the bigstring is
    valid until then. *)

val with_result :
  Chdb_connection.t -> ?format:Chdb_format.t -> string ->
  f:(Chdb_result.t -> 'a) -> 'a
(** [with_result conn ?format sql ~f] executes [sql], passes the
    result to [f], and destroys the result afterwards (even if
    [f] raises). *)

val with_buffer :
  Chdb_connection.t -> ?format:Chdb_format.t -> string ->
  f:(Chdb_result.bigstring -> 'a) -> 'a
(** [with_buffer conn ?format sql ~f] executes [sql] and passes a
    zero-copy bigstring view to [f].  The buffer is valid only for
    the duration of [f]. *)

val execute_ignore :
  Chdb_connection.t -> ?format:Chdb_format.t -> string -> unit
(** [execute_ignore conn ?format sql] executes [sql] and discards
    the result.  Suitable for DDL statements and INSERTs.
    Default format is {!Chdb_format.Null}. *)

(** {2 Non-raising variants}

    Return {!Base.Or_error.t} instead of raising {!Chdb_error.E}. *)

val try_execute :
  Chdb_connection.t -> ?format:Chdb_format.t -> string ->
  Chdb_result.t Base.Or_error.t
(** Like {!execute} but returns [Error] on failure. *)

val try_execute_string :
  Chdb_connection.t -> ?format:Chdb_format.t -> string ->
  string Base.Or_error.t
(** Like {!execute_string} but returns [Error] on failure. *)

(** Streaming query execution.

    A stream delivers result data in chunks, enabling
    bounded-memory processing of arbitrarily large result sets.
    Each chunk is a {!Chdb_result.t} that must be consumed or
    destroyed before fetching the next.

    {[
      let stream = Stream.start conn ~format:TSV
                     "SELECT number FROM numbers(1000000)" in
      Stream.iter stream ~f:(fun chunk ->
        print_string (Result.to_string chunk));
    ]} *)

type t
(** A streaming query cursor.  Not thread-safe: a single stream
    should only be driven from one thread at a time (though the
    underlying connection is mutex-protected). *)

(** {2 Lifecycle} *)

val start :
  Chdb_connection.t -> ?format:Chdb_format.t -> string -> t
(** [start conn ?format sql] begins a streaming query.

    @param format Output format (default: {!Chdb_format.CSV}).
    @raise Chdb_error.E on failure. *)

val fetch : t -> Chdb_result.t option
(** [fetch t] returns the next result chunk, or [None] when the
    stream is exhausted.

    @raise Chdb_error.E if a chunk carries an error. *)

val cancel : t -> unit
(** [cancel t] aborts the streaming query.  Subsequent calls to
    {!fetch} return [None]. *)

(** {2 Iteration} *)

val iter : t -> f:(Chdb_result.t -> unit) -> unit
(** [iter t ~f] calls [f] on each chunk until exhausted. *)

val fold : t -> init:'a -> f:('a -> Chdb_result.t -> 'a) -> 'a
(** [fold t ~init ~f] folds [f] over successive chunks. *)

val to_seq : t -> Chdb_result.t Seq.t
(** [to_seq t] converts the remaining chunks to a lazy {!Seq.t}. *)

(** {2 Convenience} *)

val count : t -> int
(** [count t] consumes the stream and returns the chunk count. *)

val collect : t -> string list
(** [collect t] consumes the stream and returns each chunk as a
    string. *)

val collect_string : t -> string
(** [collect_string t] consumes the stream and concatenates all
    chunks into a single string. *)

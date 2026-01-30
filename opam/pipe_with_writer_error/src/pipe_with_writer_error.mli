open Core
open Async_kernel

(** A [Pipe_with_writer_error.t] holds the read end of a pipe and a
    [(unit, 'error) Deferred.Result.t] that becomes determined when the write end succeeds
    or fails. On writer failure the pipe is closed but the remaining items can still be
    consumed. On writer success the pipe remains open until the writer is explicitly
    closed.

    Motivation: If you had a regular (reader, writer_error) pair and made the mistake of
    consuming the pipe until you reach EOF and not looking at the writer error, you can
    drop the writer error on the floor, or hang if the reader doesn't get an EOF when the
    writer encounters an error. A [Pipe_with_writer_error] forces you to handle the writer
    error when consuming the reader, and is indifferent to whether the underlying reader
    reaches EOF when the writer encounters an error (however, it still waits for the
    reader's [upstream_flushed]).

    Pitfalls: When using this module be sure that no writes are made after [writer_error]
    is filled with an error, or else you will encounter strange behavior. For example, you
    might [peek] and see the writer error but then [read] and read a subsequent write,
    which violates the expectation that peek returns the value that will next be read.
    Writes made in the same async job as the [writer_error] being filled will appear in
    the pipe, but writes made in subsequent Async jobs will only appear if those jobs run
    before the job that closes the pipe due to the error. For writes that are made in the
    same job as the [writer_error] being filled, there is no way to distinguish writes
    made before and after [writer_error] being filled. Writing after filling
    [writer_error] with an ok result is fine.

    See ../README.md for a comparison of the semantics of this library and the
    pipe_with_close_reason library. *)

type ('a, 'error) t [@@deriving sexp_of]

(** [create_reader f] calls [f w] and collects the output [f] writes to [w]. The pipe
    returned by [create_reader] terminates successfully only when [f w] terminates
    successfully.

    If [f w] raises an exception, the exception is not caught here: it gets sent to the
    monitor that was in scope when [create_reader] was called. In this case the writer
    will not be closed and the [writer_error] will not be determined (analogous to the
    behavior of [Pipe.create_reader ~close_on_exception:false]). If you want exceptions to
    be instead be treated as writer errors you need to catch them yourself and return an
    appropriate [(unit, 'error) Result.t]. *)
val create_reader
  :  ?size_budget:int
  -> ('a Pipe.Writer.t -> (unit, 'error) Deferred.Result.t)
  -> ('a, 'error) t

val of_reader
  :  writer_error:(unit, 'error) Deferred.Result.t
  -> 'a Pipe.Reader.t
  -> ('a, 'error) t

(** Any error in the original pipe will cause the read end of the pipe to be closed and
    the resulting pipe to report failure. *)
val of_reader_with_errors
  :  writer_error:unit Deferred.Or_error.t
  -> 'a Or_error.t Pipe.Reader.t
  -> ('a, Error.t) t

val empty : writer_error:(unit, 'error) Deferred.Result.t -> ('a, 'error) t
val of_list : writer_error:(unit, 'error) Deferred.Result.t -> 'a list -> ('a, 'error) t
val singleton : writer_error:(unit, 'error) Deferred.Result.t -> 'a -> ('a, 'error) t

(** [close] indicates that the consumer is not interested in the remaining elements of the
    pipe. *)
val close : ('a, 'error) t -> unit

(** [is_closed t] merely reports if the underlying pipe reader is closed, without checking
    the writer error. *)
val is_closed : ('a, 'error) t -> bool

(** [closed t] waits for the pipe reader to be closed and for the writer result to become
    determined. *)
val closed : ('a, 'error) t -> (unit, 'error) Deferred.Result.t

(** [length t] returns the number of elements currently queued in the underlying pipe of
    [t]. *)
val length : ('a, 'error) t -> int

(** [is_empty t] is true iff there are no values in the underlying pipe of [t]. *)
val is_empty : ('a, 'error) t -> bool

(** [read t] reads an element from the pipe, or returns the writer error if it is filled
    and no elements remain in the pipe.

    [`Eof] is only returned when it's know that the writer terminated successfully. *)
val read : ('a, 'error) t -> ([ `Eof | `Ok of 'a ], 'error) Deferred.Result.t

(** [read' t] is a variant of [read] that reads a batch of elements. *)
val read' : ('a, 'error) t -> ([ `Eof | `Ok of 'a Queue.t ], 'error) Deferred.Result.t

(** See [Pipe.read_exactly]. When there are fewer than [num_values] elements remaining in
    the pipe and a writer error occurs, the remaining elements are returned in a queue
    along with the error. *)
val read_exactly
  :  ('a, 'error) t
  -> num_values:int
  -> ( [ `Eof
       | `Fewer of 'a Queue.t (** [0 < Q.length q < num_values] *)
       | `Exactly of 'a Queue.t (** [Q.length q = num_values] *)
       ]
       , 'error * 'a Queue.t (* [0 <= Q.length q < num_values] *) )
       Deferred.Result.t

(** [peek t] looks at the first element of the pipe that's available right now, without
    waiting. If nothing is available it returns [Ok None]. An error is reported if the
    writer had an error and no elements remain in the pipe. *)
val peek : ('a, 'error) t -> ('a option, 'error) Result.t

(** [read_now] reads a value from the pipe that is immediately available. If the pipe is
    empty, [read_now] returns [`Eof] if reader is closed and [`Nothing_available] if not. *)
val read_now
  :  ('a, 'error) t
  -> ([ `Eof | `Nothing_available | `Ok of 'a ], 'error) Result.t

(** [read_all] keeps reading elements from the pipe until the pipe is closed. *)
val read_all : ('a, 'error) t -> ('a Queue.t, 'error) Deferred.Result.t

(** [values_available t] is determined when values become available or the pipe closes.

    The implementation of [values_available] is currently not sophisticated enough to
    avoid an ivar allocation per call, so calling [values_available] in a loop without
    writing anything to a pipe will result in a memory leak. *)
val values_available : ('a, 'error) t -> ([ `Eof | `Ok ], 'error) Deferred.Result.t

val fold
  :  ('a, 'error) t
  -> init:'accum
  -> f:('accum -> 'a -> 'accum Deferred.t)
  -> ('accum, 'error) Deferred.Result.t

val fold'
  :  ('a, 'error) t
  -> init:'accum
  -> f:('accum -> 'a Queue.t -> 'accum Deferred.t)
  -> ('accum, 'error) Deferred.Result.t

val fold_without_pushback
  :  ('a, 'error) t
  -> init:'accum
  -> f:('accum -> 'a -> 'accum)
  -> ('accum, 'error) Deferred.Result.t

val iter : ('a, 'error) t -> f:('a -> unit Deferred.t) -> (unit, 'error) Deferred.Result.t

val iter'
  :  ('a, 'error) t
  -> f:('a Queue.t -> unit Deferred.t)
  -> (unit, 'error) Deferred.Result.t

val iter_without_pushback
  :  ?max_iterations_per_job:int
  -> ('a, 'error) t
  -> f:('a -> unit)
  -> (unit, 'error) Deferred.Result.t

val iter_parallel
  :  ?continue_on_error:bool
  -> ('a, 'error) t
  -> max_concurrent_jobs:int
  -> f:('a -> unit Deferred.t)
  -> (unit, 'error) result Deferred.t

val transfer
  :  ('a, 'error) t
  -> 'b Pipe.Writer.t
  -> f:('a -> 'b)
  -> (unit, 'error) Deferred.Result.t

val transfer_id : ('a, 'error) t -> 'a Pipe.Writer.t -> (unit, 'error) Deferred.Result.t
val map : ?max_batch_size:int -> ('a, 'error) t -> f:('a -> 'b) -> ('b, 'error) t

val map'
  :  ?max_queue_length:int
  -> ('a, 'error) t
  -> f:('a Queue.t -> 'b Queue.t Deferred.t)
  -> ('b, 'error) t

val map_error : ('a, 'error1) t -> f:('error1 -> 'error2) -> ('a, 'error2) t
val map_error' : ('a, 'error1) t -> f:('error1 -> 'error2 Deferred.t) -> ('a, 'error2) t

val folding_map
  :  ('a, 'error) t
  -> init:'accum
  -> f:('accum -> 'a -> 'accum * 'b)
  -> ('b, 'error) t

val filter_map : ('a, 'error) t -> f:('a -> 'b option) -> ('b, 'error) t
val filter_map' : ('a, 'error) t -> f:('a -> 'b option Deferred.t) -> ('b, 'error) t
val filter : ('a, 'error) t -> f:('a -> bool) -> ('a, 'error) t
val merge : ('a, Error.t) t list -> compare:('a -> 'a -> int) -> ('a, Error.t) t
val concat : ('a, Error.t) t list -> ('a, Error.t) t
val interleave : ('a, Error.t) t list -> ('a, Error.t) t

val merge_custom
  :  ('a, 'error) t list
  -> compare:('a -> 'a -> int)
  -> combine_errors:('error list -> 'combined_error)
  -> ('a, 'combined_error) t

val concat_custom
  :  ('a, 'error) t list
  -> combine_errors:('error list -> 'combined_error)
  -> ('a, 'combined_error) t

val interleave_custom
  :  ('a, 'error) t list
  -> combine_errors:('error list -> 'combined_error)
  -> ('a, 'combined_error) t

(** The writer error here should be thought of as indicating on both the inner and outer
    pipe writers. The inner pipes are handled the same way [of_reader] handles its input:
    if the writer error happens, we expect no further elements will be written, and close
    the read end of the pipe (after waiting for upstream flushed). *)
val concat_plain_pipes : ('a Pipe.Reader.t, 'error) t -> ('a, 'error) t

val fork
  :  ('a, 'error) t
  -> pushback_uses:[ `Both_consumers | `Fast_consumer_only ]
  -> ('a, 'error) t * ('a, 'error) t

val to_list : ('a, 'error) t -> ('a list, 'error) Deferred.Result.t

(** The [Expert] functions expose the underlying pipe reader. If you use them it's your
    responsibility to properly handle the close status. Be sure you understand the comment
    at the top of this file before using these functions. If you are lifting a function
    that is provided by [Async.Pipe] but is not available here, consider adding it
    directly instead. Also note that if the [Pipe_with_writer_error.t] was created from a
    regular [Pipe.Reader.t] the reader exposed here is a different reader. *)
module Expert : sig
  type ('a, 'error) pipe := ('a, 'error) t

  type ('a, 'error) t = private
    { reader : 'a Pipe.Reader.t
    ; writer_error : (unit, 'error) Deferred.Result.t
    }
  [@@deriving sexp_of]

  (** Get the underlying reader and close status of a [Pipe_with_writer_error.t] *)
  val get : ('a, 'error) pipe -> ('a, 'error) t

  (** Lifts a function that maps one reader to another by transferring the close status to
      the new reader. *)
  val lift_map
    :  ('a, 'error) pipe
    -> f:('a Pipe.Reader.t -> 'b Pipe.Reader.t)
    -> ('b, 'error) pipe

  (** Lifts a functions that reads from the pipe without fully consuming it. It waits for
      either the necessary values to be available for reading or for the writer to be
      closed (which can be due to [writer_error] being determined with an error). *)
  val lift_read
    :  ('a, 'error) pipe
    -> f:('a Pipe.Reader.t -> [ `Eof | `Ok of 'b ] Deferred.t)
    -> ([ `Eof | `Ok of 'b ], 'error) Deferred.Result.t

  (** Lifts a function that fully consumes a pipe. Starts the call to [f], but waits for
      the writer to be closed, ensuring we wait to discover whether the writer finished
      successfully. *)
  val lift_consume
    :  ('a, 'error) pipe
    -> f:('a Pipe.Reader.t -> 'b Deferred.t)
    -> ('b, 'error) Deferred.Result.t

  (** Lifts a function that merges readers together by combining their writer errors. *)
  val lift_concat
    :  ('a, 'error) pipe list
    -> f:('a Pipe.Reader.t list -> 'b Pipe.Reader.t)
    -> combine_errors:('error list -> 'combined_error)
    -> ('b, 'combined_error) pipe
end

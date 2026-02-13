(** Error types for chdb operations.

    All chdb errors are represented as variants of {!t} and raised
    wrapped in the {!E} exception.  Use {!to_string} or {!pp} for
    human-readable messages.

    {[
      match Query.execute conn sql with
      | result -> ...
      | exception Error.E err ->
        Printf.eprintf "%s\n" (Error.to_string err)
    ]} *)

(** The type of chdb errors. *)
type t =
  | Connection_failed of string
      (** The connection could not be established.  The payload
          carries the diagnostic message from the C library. *)
  | Query_failed of string
      (** A query returned an error.  The payload is the
          ClickHouse error message. *)
  | Invalid_result
      (** The result handle was unexpectedly invalid. *)
  | Stream_exhausted
      (** A streaming query was read past its end. *)
  | Use_after_close
      (** An operation was attempted on a closed connection. *)

exception E of t
(** Raised by all chdb operations that can fail.
    Registered with {!Printexc} for informative backtraces. *)

val to_string : t -> string
(** [to_string err] returns a human-readable error description. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt err] pretty-prints the error. *)

(**/**)

val raise_connection_failed : string -> 'a
val raise_query_failed : string -> 'a

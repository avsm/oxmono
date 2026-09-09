(** UTF-8 validation (RFC 3629).

    The well-formedness check is the stdlib's allocation-free
    [String.is_valid_utf_8]; only invalid input pays for locating the offending
    byte. *)

val validate : string -> (unit, int) result
(** [validate s] is [Ok ()] when [s] is well-formed UTF-8, or [Error i] with the
    byte offset of the first malformed sequence. *)

(** Validation of a byte stream, a piece at a time.

    {!validate} needs the whole input in one string, which is the one thing a
    parser reading from a {!Bytesrw.Bytes.Reader.t} does not have. This takes
    the input in the pieces the reader delivers -- a multi-byte sequence may
    straddle two of them -- and answers the same question about their
    concatenation. *)
module Stream : sig
  type t
  (** A validator part-way through a byte stream. *)

  val v : unit -> t
  (** [v ()] is a validator at the start of a stream. *)

  val add : t -> bytes -> first:int -> length:int -> unit
  (** [add t b ~first ~length] validates [length] bytes of [b] from [first] as
      the continuation of the stream. *)

  val finish : t -> unit
  (** [finish t] declares the stream ended, which makes a sequence left
      incomplete by the last {!add} malformed. Calling it twice is calling it
      once. *)

  val error : t -> int option
  (** [error t] is the offset, in bytes added so far, of the first byte of the
      first malformed sequence, or [None] while the stream is well-formed. *)
end

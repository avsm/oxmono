(** Incremental IMAP response framing. The decoder consumes every supplied byte
    or returns an error. It never collects literal payloads across [feed] calls.
    [Text] includes the literal marker and following CRLF. *)

type event =
  | Text of string
  | Literal_start of int64
  | Literal_chunk of string
  | Literal_end
  | End_of_response

type error = { offset : int64; message : string }
type t

val create : ?max_control:int -> ?max_literal:int64 -> unit -> t
(** [create ()] is a decoder at the start of a stream. [max_control] bounds
    one control line in bytes and defaults to 1,048,576. [max_literal] bounds
    one literal in bytes and defaults to 1,073,741,824.

    @raise Invalid_argument if [max_control] is below 16 or [max_literal] is
    negative. *)

val feed : t -> string -> (event list, error) result
(** [feed t chunk] frames [chunk], an arbitrary slice of the stream. A
    complete response ends with [End_of_response]. A literal whose length
    exceeds [max_literal] or int64 is an error. Errors are sticky. Once one
    occurs every later [feed] and {!finish} returns it. When the error follows
    events framed earlier in the same chunk, [feed] returns those events and
    the next call returns the error, so [feed t ""] reports a pending error
    without consuming input. *)

val finish : t -> (unit, error) result
(** [finish t] signals end of stream. It fails if a response or literal was
    truncated, or if an earlier [feed] failed. *)

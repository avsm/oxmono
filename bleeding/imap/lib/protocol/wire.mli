(** Incremental IMAP response framing.

    A decoder splits the byte stream from a server into control lines and
    literal payloads without parsing them. It consumes every byte it is
    given and holds only the current control line, so literal payloads are
    passed on in chunks and never collected. Lines must end in CRLF. A
    literal is recognised only at the end of a line of a data response
    whose grammar allows one, which is LIST, LSUB, XLIST, STATUS,
    NAMESPACE, ID, METADATA, QUOTA, QUOTAROOT, ACL, LISTRIGHTS, MYRIGHTS,
    ESEARCH, LANGUAGE, FETCH and UIDFETCH. A marker inside a quoted string
    or in response text is plain text. *)

type event =
  | Text of string
      (** A control line, including its final CRLF and any literal marker
          before it. *)
  | Literal_start of int64  (** A literal of the given length follows. *)
  | Literal_chunk of string  (** The next bytes of the current literal. *)
  | Literal_end  (** The current literal is complete. *)
  | End_of_response  (** The last line of a response has been framed. *)
(** The type for framing events. A response is one or more [Text] events,
    each but the last followed by a literal, and then [End_of_response]. *)

type error = {
  offset : int64;  (** The stream offset in bytes at which framing failed. *)
  message : string;
}
(** The type for framing errors. *)

type t
(** The type for decoders. A decoder is mutable and belongs to one
    stream. *)

val create : ?max_control:int -> ?max_literal:int64 -> unit -> t
(** [create ~max_control ~max_literal ()] is a decoder at the start of a
    stream. [max_control] bounds one control line in bytes, CRLF included,
    and defaults to 1,048,576. [max_literal] bounds one literal in bytes
    and defaults to 1,073,741,824.

    @raise Invalid_argument if [max_control] is below 16 or [max_literal]
    is negative. *)

val feed : t -> string -> (event list, error) result
(** [feed t chunk] is the events framed from [chunk], an arbitrary slice of
    the stream that continues where the previous call stopped. The error
    covers a line longer than [max_control], a bare CR or LF, a
    non-synchronizing literal marker, and a literal longer than
    [max_literal] or than int64 can hold. Errors are sticky, so every later
    [feed] and {!finish} returns the same error. When a chunk fails after
    framing some events, [feed] returns those events and the next call
    returns the error, so [feed t ""] reports a pending error without
    consuming input. *)

val finish : t -> (unit, error) result
(** [finish t] signals the end of the stream of [t]. It is an error if a
    control line, a literal or a response after a literal is incomplete,
    or if an earlier {!feed} failed. *)

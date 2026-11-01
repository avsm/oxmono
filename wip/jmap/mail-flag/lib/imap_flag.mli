@@ portable

(** Lossless IMAP message flags (RFC 9051 §2.3.2).

    This is the wire representation. {!Keyword.of_string} intentionally accepts
    JMAP and IMAP spellings of known flags, so it cannot distinguish an IMAP
    keyword named [Seen] from the IMAP system flag [\Seen]. Keep this value
    until a caller explicitly asks for a shared semantic keyword. *)

type system = Seen | Answered | Flagged | Deleted | Draft

type t = private
  | System of system
  | Recent
  | Keyword of string
  | Extension of string
(** [Keyword s] retains the exact wire spelling of an IMAP atom. [Extension s]
    retains the exact spelling of an unrecognised backslash-prefixed flag. *)

val of_wire : string -> (t, string) result
(** Parse a complete flag atom, rejecting delimiters, controls and bare [\*].
    Standard system flags and [\Recent] match case-insensitively. *)

val system : system -> t
val keyword : string -> (t, string) result
(** Construct a keyword from an atom, without treating familiar spelling as a
    system flag. *)

val to_wire : t -> string
val semantic : t -> Keyword.t option
(** Convert when the meaning is unambiguous. [\Recent], unknown system flags,
    and keywords whose spelling collides with a standard system flag have no
    lossless JMAP equivalent. *)

val equal : t -> t -> bool
val compare : t -> t -> int
val pp : Format.formatter -> t -> unit

val durable : t list -> t list
(** Remove transient [\Recent], sort and deduplicate using IMAP flag identity.
    Retains a representative wire spelling for each flag. *)

val equal_durable : t list -> t list -> bool
(** Compare durable flag sets case-insensitively, ignoring order, duplicates,
    and [\Recent]. *)

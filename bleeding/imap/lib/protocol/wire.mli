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
val feed : t -> string -> (event list, error) result
(** Feed an arbitrary chunk. A complete response ends with [End_of_response]. *)
val finish : t -> (unit, error) result
(** Signal EOF; fails if a response or literal was truncated. *)

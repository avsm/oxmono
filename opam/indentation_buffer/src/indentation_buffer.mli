open! Core

(** A buffer that keeps track of the current indentation level. It abstracts the need for
    formatting code to thread the current indentation everywhere, and also ensures that
    blank lines do not accidentally get filled with the current indentation, thus yielding
    trailing whitespace in the output. *)
type t

(** Appends the input string to a buffer. If the buffer is at the start of a new line,
    then the current indentation is inserted. *)
val string : t -> string -> unit

(** Starts a new line in the buffer, but does not append any indentation. This will only
    happen if a string is appended before the buffer moves to the next line. *)
val newline : t -> unit

(** Increase the current indentation by one level. *)
val indent : t -> unit

(** Decrease the current indentation by one level. *)
val dedent : t -> unit

(** Create a new buffer with the specified initial capacity. *)
val create : int -> t

(** Retrieve the contents of the buffer as a string. *)
val contents : t -> string

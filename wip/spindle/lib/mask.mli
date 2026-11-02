(** Hiding secret values in streamed log output. *)

val patterns : string list -> string list
(** [patterns values] is what is hidden for [values]: each value, each of its
    non-empty lines, and its base64 encoding with and without padding, as the
    reference spindle hides them. *)

val base64 : string -> string
(** [base64 s] is [s] in standard, padded base64. *)

type t

val v : string list -> t
(** [v values] is a masker for one output stream. *)

val feed : t -> string -> string
(** [feed t chunk] is [chunk] with every pattern replaced by [***]. Text that
    may begin a pattern split across chunks is held back until the next call. *)

val finish : t -> string
(** [finish t] is the text still held back, masked, at the end of the stream. *)

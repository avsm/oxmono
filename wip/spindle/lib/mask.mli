(** Hiding secret values in streamed log output. *)

val patterns : string list -> string list
(** [patterns values] is what is hidden for [values]: each value, each of its
    non-empty lines, and its base64 encoding with and without padding, as the
    reference spindle hides them. It also holds the characters that encode
    each value alone at each of the three alignments within a longer base64
    text, where there are at least 8 of them. *)

type t

val v : string list -> t
(** [v values] is a masker for one output stream. *)

val feed : t -> string -> string
(** [feed t chunk] is [chunk] with every pattern replaced by [***]. Text that
    may begin a pattern split across chunks is held back until the next call. *)

val finish : t -> string
(** [finish t] is the text still held back, masked, at the end of the stream. *)

val hide : string list -> string -> string
(** [hide values s] is [s] with every pattern of [values] replaced by [***], as
    one whole stream. *)

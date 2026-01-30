(** HTTP header type and operations. *)

module Name = Header_name

(** Parsed header. Stored in local list - stack allocated, no heap allocation.
    [name_span] is only meaningful when [name = Other]. *)
type t =
  { name : Name.t
  ; name_span : Span.t
  ; value : Span.t
  }

(** Find first header by name. Only matches known headers; use [find_string] for [Other]. *)
val find : t list @ local -> Name.t -> t option @ local

(** Find header by string name (case-insensitive). *)
val find_string : Base_bigstring.t -> t list @ local -> string -> t option @ local

(** Convert header to (name, value) string pair. *)
val to_string_pair : Base_bigstring.t -> t -> string * string

(** Convert header list to (name, value) string pairs. *)
val to_string_pairs : Base_bigstring.t -> t list -> (string * string) list

(** Pretty-print header with buffer (shows actual values). *)
val pp_with_buf : Base_bigstring.t -> Stdlib.Format.formatter -> t -> unit

(** Pretty-print header structure. *)
val pp : Stdlib.Format.formatter -> t -> unit

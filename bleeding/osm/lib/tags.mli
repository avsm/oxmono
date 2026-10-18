@@ portable

type t = string Jsont.String_map.t

val empty : t
(** [empty] is the map with no tags. *)

val find : string -> t -> string option
(** [find key tags] is the value of [key], or [None] when absent. *)

val of_list : (string * string) list -> t
(** [of_list entries] builds a map. The last value for a repeated key wins. *)

val to_list : t -> (string * string) list
(** [to_list tags] returns entries sorted by key. *)

val jsont : t Jsont.t
(** [jsont] maps string-valued JSON objects without fixing the set of keys. *)

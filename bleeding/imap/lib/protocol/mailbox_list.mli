(** RFC 5258 LIST-EXTENDED selection and return options. *)

type selection =
  | Subscribed
  | Remote
  | Recursive_match  (** Requires [Subscribed] or [Special_use] beside it. *)
  | Special_use  (** RFC 6154. *)

type return =
  | Subscribed
  | Children
  | Special_use  (** RFC 6154. *)

val selection_to_wire : selection -> string
(** [selection_to_wire s] is the uppercase option name of [s], such as
    [RECURSIVEMATCH]. *)

val return_to_wire : return -> string
(** [return_to_wire r] is the uppercase option name of [r]. *)

val equal_selection : selection -> selection -> bool
(** [equal_selection a b] holds when [a] and [b] are the same option. *)

val equal_return : return -> return -> bool
(** [equal_return a b] holds when [a] and [b] are the same option. *)

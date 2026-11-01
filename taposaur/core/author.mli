(** Authors as recorded in index metadata. *)
type t = { given : string option; family : string }
val jsont : t Jsont.t @@ portable
val of_correction : string -> t list
(** [of_correction s] reads semicolon-separated [Family, Given] names. *)

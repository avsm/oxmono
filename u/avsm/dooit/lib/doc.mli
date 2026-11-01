(** Validated task documents, retaining their original YAML formatting. *)

type t = private {
  raw : string;
  header : string;
  body : string;
  meta : Common.value;
  prefix : string;
  separator : string;
}

val parse : string -> t
(** [parse raw] validates a task document of at most 1 MiB. *)

val create :
  ?id:string ->
  ?tags:string list ->
  ?links:Common.value list ->
  ?body:string ->
  ?due:string ->
  title:string ->
  unit ->
  t

val id : t -> string
val title : t -> string
val status : t -> string
val statuses : string list
val tags : t -> string list
val due : t -> string option
val deleted : t -> bool
val revision : t -> string
val update : t -> meta:Common.value -> body:string -> t
val change : string -> Common.value option -> t -> t

val patch : t -> Common.value -> t
(** [patch note request] applies a validated [dooit.patch/v1] request, retaining
    immutable identity fields and unmodified YAML. *)

val immutable : t -> t -> unit
val public : t -> Common.value

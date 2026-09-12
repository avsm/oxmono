(** Sortal V2 field mappings for the vCard 3.0 compatibility profile and 4.0.
    The generic YAML tree preserves scalar/list shapes and absent versus
    explicitly empty fields. No serialized contact payload is emitted. Invalid
    or unmapped data raises [Common.Error]. *)

type property = {
  group : string;
  name : string;
  params : (string * string) list;
  value : string;
}

val text : string -> string
val untext : string -> string
val property : string -> property
val parse : string -> property list
val only : property list -> string -> property
val param : property -> string -> string option
val required : property -> string -> string
val header : property -> string
val render : property list -> string
val split_value : char -> string -> string list
val components : string -> string list

val normalized_params :
  ?client:bool -> ?annotations:bool -> property -> (string * string) list

val signatures :
  ?client:bool ->
  property list ->
  (string * string * (string * string) list * string) list

val encode :
  ?version:string ->
  uid:string ->
  store_id:string ->
  originals:string ->
  Common.value ->
  string * string list
(** Encode a complete contact and return its card and projection warnings. Local
    photos are loaded beneath [originals] without resizing. *)

val decode : string -> Common.value * (string * string) list
(** Reconstruct annotated contact fields and original photo bytes. This checks
    the export profile; remote edits require a common baseline and the
    reconciliation functions in [Pull]. *)

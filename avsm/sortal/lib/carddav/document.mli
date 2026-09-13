(** Lossless edits to native Sortal vCards. *)

val value : string -> Common.value
(** [value raw] is the complete annotated field tree in [raw]. *)

val contact : string -> Sortal_schema.Contact.t
(** [contact raw] projects the understood fields and retains [raw] as the
    storage revision. Unknown properties remain in the vCard. *)

val of_contact : Sortal_schema.Contact.t -> Common.value
(** [of_contact contact] is the JSON projection of [contact]. *)

val update : originals:string -> string -> Common.value -> string
(** [update ~originals raw fields] changes annotated fields while retaining
    unrelated properties, groups and parameters. [originals] contains photos. An
    unchanged document is returned byte for byte. *)

val edit : originals:string -> string -> Sortal_schema.Contact.t -> string
(** [edit ~originals raw contact] applies changes to the typed projection,
    preserving fields that the schema does not understand. *)

(** Immutable snapshots of native vCard stores. *)

val contact : string -> Common.value
(** [contact raw] validates a native vCard and returns all annotated fields. *)

val verify : ?source:string -> string -> Common.value
(** [verify bundle] checks inventory, hashes, identity and embedded photos. With
    [source], every archived file must match its current source bytes. *)

val export :
  ?previous:string -> source:string -> output:string -> unit -> Common.value
(** [export ~source ~output ()] snapshots a native store without re-encoding
    cards. Retired store tags are omitted from exported cards, while originals
    retain all bytes. UIDs live in the source cards. [previous] optionally
    verifies an earlier native bundle from this store. Failed exports remove
    their incomplete output. *)

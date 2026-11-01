(** Editable task storage. Filesystem operations are synchronous. Eio consumers
    should use [Client] or run these operations in worker threads. *)

type t = private { root : string; id : string }
type transaction

val open_ : string -> t
val init : ?id:string -> string -> t
val get : t -> string -> Doc.t
val scan : t -> Doc.t list * (string * string) list
val note_path : t -> string -> string
val conflicts : t -> string list
val recover : t -> (string * string) list

val apply :
  t ->
  id:string ->
  expected:string option ->
  operation:string ->
  payload:string ->
  (Doc.t option -> Doc.t) ->
  Doc.t
(** [apply store ... change] applies a revision-checked, journalled operation
    under the store lock. Replaying the operation returns its recorded result. *)

val add : t -> Doc.t -> Doc.t
val modify : t -> string -> (Doc.t -> Doc.t) -> Doc.t

val with_lock : t -> (unit -> 'a) -> 'a
(** [with_lock store f] excludes other threads and processes without blocking on
    contention. Nested locks on the same store are refused. *)

val with_transaction : t -> (transaction -> 'a) -> 'a
(** [with_transaction store f] holds the store lock for [f]. Its transaction
    token expires when [f] returns or raises. *)

val apply_transaction :
  transaction ->
  id:string ->
  expected:string option ->
  operation:string ->
  payload:string ->
  (Doc.t option -> Doc.t) ->
  Doc.t
(** [apply_transaction transaction ... change] applies an operation within an
    active transaction. An expired token is rejected before reading or writing. *)

val state_dir : t -> string
(** Journal and synchronization support. Mutations require [with_lock] or an
    active transaction covering the complete read/modify/write sequence. *)

val state_path : t -> string -> string
val ensure_state : t -> unit
val metadata : string -> Common.value
val parse_metadata : string -> string
val raw : t -> string -> string option
val revision_opt : string option -> string option
val request : id:string -> expected:string option -> payload:string -> string
val load_state : t -> string -> Common.value -> Common.value
val save_state : t -> string -> Common.value -> unit
val keep : t -> string -> string
val object_ : t -> string -> string
val install : t -> id:string -> expected:string option -> string -> unit

val record_conflict :
  t ->
  id:string ->
  base:string option ->
  local:string option ->
  remote:string option ->
  string ->
  unit

val clear_conflict : t -> string -> unit

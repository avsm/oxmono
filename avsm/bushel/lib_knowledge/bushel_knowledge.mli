module C = Sortal_schema.Contact
module I = Retrieval.Index
module E = Retrieval.Embedding
module J = Retrieval.Json

type edge = {
  source : string;
  target : string;
  relation : string;
  evidence : string;
}

type contact = { id : string; value : C.t }
type candidate = { contact : contact; matched_on : string; exact : bool }
type t

val create : I.t -> t
val key : string list -> string
val entry_id : string -> string

val contact_id : C.t -> string
(** [contact_id c] identifies a loaded contact by vCard UID, independently of
    its handle. Contacts without a UID are rejected. *)

val feed_id : string -> string -> string
val url_id : string -> string
val contacts : t -> contact list
val edges : t -> edge list
val generation : t -> string
val get : t -> string -> I.document option
val find_url : t -> string -> I.document list
val related : t -> string -> edge list

val resolve : t -> string -> candidate list
(** [resolve t query] returns all exact identity/name matches, or prefix
    candidates if there are none. Multiple exact matches remain ambiguous. *)

val refresh :
  ?base_url:string ->
  ?links:Bushel.Link.t list ->
  t ->
  entries:Bushel.Entry.t ->
  contacts:C.t list ->
  feeds:(string * Sortal_feed.Entry.t * string list) list ->
  unit
(** [refresh ...] reconciles a snapshot with typed source identities and
    recorded relationship evidence. Feed tuples carry associated contact handles
    and annotated Bushel slugs. Associations do not imply authorship. *)

val load :
  ?base_url:string ->
  t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  bushel_dir:string ->
  sortal_dir:string ->
  unit
(** [load ...] reads local Bushel, vCard and cached feed files. It performs no
    sync or network request. Malformed existing source files fail the refresh. *)

val index : t -> E.provider -> check:(unit -> unit) -> max_batches:int -> int
val coverage : t -> E.provider -> int * int

val search :
  t ->
  ?provider:E.provider ->
  check:(unit -> unit) ->
  mode:string ->
  filter:I.filter ->
  string ->
  I.chunk list

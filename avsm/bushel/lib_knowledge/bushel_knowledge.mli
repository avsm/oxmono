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
val restore : t -> bool
(** [restore t] restores the domain snapshot published with its index. It is
    false for an index that predates persisted snapshots. *)
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

val counts : t -> int * int
(** [counts t] is the document and passage count in the current index. *)

val get : t -> string -> I.document option
val find_url : t -> string -> I.document list
val related : t -> string -> edge list

val resolve : t -> string -> candidate list
(** [resolve t query] returns all exact identity/name matches, or prefix
    candidates if there are none. Multiple exact matches remain ambiguous. *)

val refresh :
  ?metadata:(string * string) list ->
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
  ?metadata:(string * string) list ->
  ?base_url:string ->
  t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  bushel_dir:string ->
  sortal_dir:string ->
  unit
(** [load ...] reads local Bushel, vCard and cached feed files. It performs no
    sync or network request. Malformed existing source files fail the refresh. *)

val index :
  ?on_progress:(int -> unit) ->
  ?filter:I.filter ->
  t ->
  E.provider ->
  check:(unit -> unit) ->
  max_batches:int ->
  int
(** [index t provider ~check ~max_batches] embeds outstanding passages. [filter]
    restricts eligible documents and defaults to the entire corpus.
    [on_progress] receives the cumulative number written after each complete
    batch, as in [Retrieval.Embedding.index]. *)

val coverage : ?filter:I.filter -> t -> E.provider -> int * int

val latest_posts : t -> until:string -> I.document list
(** [latest_posts t ~until] returns up to 100 published Bushel notes in
    descending original publication date order, through [until] (YYYY-MM-DD).
    Drafts and index pages are excluded. Returned dates are publication dates,
    independent of later updates. This needs no embedding provider or feed
    synchronization. *)

val search :
  t ->
  ?provider:E.provider ->
  check:(unit -> unit) ->
  mode:string ->
  filter:I.filter ->
  string ->
  I.chunk list

val image_id : string -> string
(** [image_id name] is the stable source ID of a Srcsetter base output filename.
    Relative subdirectories distinguish images with the same basename. *)

val load_images :
  ?metadata:(string * string) list ->
  t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  output_dir:string ->
  check:(unit -> unit) ->
  unit
(** [load_images t ~fs ~output_dir ~check] replaces an independent image corpus
    from Srcsetter's [index.json]. Each base output filename has one document.
    Sources sharing an output are merged, retaining their metadata. Duplicate
    rows and manifest order do not affect revisions. Pixel SHA256 revisions
    invalidate changed files. Generated variants are not indexed separately.
    Search metadata is clipped to one passage per image. Full metadata and pixel
    hashes affect revisions. Outputs with a width or height below 32 pixels in
    the manifest are skipped. *)

val skipped_images : t -> int
(** [skipped_images t] is the number of distinct undersized image outputs
    omitted by the last successful {!load_images} call. *)

val index_images :
  ?on_progress:(int -> unit) ->
  t ->
  E.provider ->
  check:(unit -> unit) ->
  max_batches:int ->
  int
(** [index_images t provider ~check ~max_batches] embeds image pixels through
    the provider's document task. Inputs carry the pixel path and content hash.
    Changed pixels reject stale writes. *)

val image_coverage : t -> E.provider -> int * int

val search_images :
  t ->
  ?provider:E.provider ->
  check:(unit -> unit) ->
  mode:string ->
  string ->
  I.chunk list
(** [search_images t ~check ~mode query] searches image metadata lexically or
    uses the image provider's query encoder for visual similarity. *)

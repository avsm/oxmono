module Response = Response

type t
(** A session over the published Bushel and Sortal corpus. *)

val sources :
  ?config_file:string -> fs:Eio.Fs.dir_ty Eio.Path.t -> unit -> string * string
(** [sources ~fs ()] is the configured Bushel root and native Sortal root.
    Invalid configuration raises [Failure]. *)

val available :
  ?config_file:string -> fs:Eio.Fs.dir_ty Eio.Path.t -> unit -> bool
(** [available ~fs ()] is false when either source directory is absent. Invalid
    configuration and inaccessible sources raise an exception. *)

val with_store :
  ?config_file:string ->
  ?runtime:Retrieval_mlx.Runtime.t ->
  ?refresh:bool ->
  ?write:bool ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  clock:[> float Eio.Time.clock_ty ] Eio.Resource.t ->
  fetch:_ Fetch.t ->
  check:(unit -> unit) ->
  (t -> 'a) ->
  'a
(** [with_store ~fs ~clock ~fetch ~check f] opens the last published source
    snapshot. A missing index is initialized once. Read sessions pin a SQLite
    generation and do not block concurrent indexing. [refresh] checks source
    manifests and reparses changed sources before opening the session. [write]
    permits embedding writes, which validate each passage revision. Only source
    refreshes hold the per-database filesystem lock. Image refreshes use their
    own lock. [check] enforces authority and cancellation.

    [runtime] reuses models owned by an application switch. When omitted, models
    are released at session end. [f] must not retain any session values. HTTP
    credentials are read lazily from an owned private file and require HTTPS.
    Source files are never modified. *)

val corpus : t -> Bushel_knowledge.t
val provider : t -> Retrieval.Embedding.provider option
val database : t -> string

val clock : t -> float Eio.Time.clock_ty Eio.Resource.t
(** [clock t] is the wall clock supplied to the current store session. *)

val status : t -> Response.status
(** [status t] contains the database path, corpus generation, document and
    passage counts, provider identity and current embedding coverage. It makes
    no provider request. *)

val index :
  ?on_progress:(indexed:int -> total:int -> unit) ->
  t ->
  check:(unit -> unit) ->
  max_batches:int ->
  Response.status
(** [index t ~check ~max_batches] embeds outstanding passages in a [write]
    session and returns status with [newly_indexed]. The budget must be between
    1 and 10000. Repeating the operation reuses current vectors. [on_progress]
    receives initial corpus coverage, then updated counts after each fully
    stored batch. [total] is the number of corpus passages, including those
    outside the batch budget. Callback exceptions propagate. *)

val index_all :
  ?on_progress:(indexed:int -> total:int -> unit) ->
  t ->
  check:(unit -> unit) ->
  Response.status
(** [index_all t ~check] embeds all outstanding passages in a [write] session
    and returns status with the aggregate [newly_indexed] count. Progress
    includes initial coverage and each completed batch. Cancellation and errors
    propagate, retaining completed batches for the next invocation. *)

val image_corpus : t -> Bushel_knowledge.t
(** [image_corpus t] opens the separate image corpus, keyed by the canonical
    Srcsetter output directory. It follows the session refresh policy. Text
    commands do not read images. *)

val image_provider : t -> Retrieval.Embedding.provider option
(** [image_provider t] returns the independently configured vision provider. Its
    model weights are loaded on first inference. *)

val image_status : t -> Response.status
(** [image_status t] is image corpus counts, model identity and coverage. It
    fingerprints files without loading GPU weights. *)

val index_images :
  ?on_progress:(indexed:int -> total:int -> unit) ->
  t ->
  check:(unit -> unit) ->
  max_batches:int ->
  Response.status
(** [index_images t ~check ~max_batches] indexes at most [max_batches] batches
    of image pixels and returns status. The budget is between 1 and 10000.
    Progress receives initial coverage and each completed batch. *)

val index_images_all :
  ?on_progress:(indexed:int -> total:int -> unit) ->
  t ->
  check:(unit -> unit) ->
  Response.status
(** [index_images_all t ~check] indexes all outstanding image pixels and returns
    aggregate status. Text vectors are kept in an independent index. *)

val images_search :
  ?mode:string ->
  ?offset:int ->
  t ->
  check:(unit -> unit) ->
  query:string ->
  Response.search
(** [images_search t ~check ~query] returns image hits. The default mode is
    semantic. Lexical search uses metadata only, without model inference. Hybrid
    mode combines image metadata and visual rankings. *)

val images_get :
  t -> check:(unit -> unit) -> id:string -> offset:int -> Response.source
(** [images_get t ~check ~id ~offset] returns image metadata and pixel paths. *)

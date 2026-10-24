type t
(** A refreshed Bushel and Sortal corpus under an exclusive session lock. *)

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
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  clock:[> float Eio.Time.clock_ty ] Eio.Resource.t ->
  fetch:_ Fetch.t ->
  check:(unit -> unit) ->
  (t -> 'a) ->
  'a
(** [with_store ~fs ~clock ~fetch ~check f] reloads configuration and sources,
    then calls [f] with their shared index. The index is private XDG data keyed
    by canonical source roots. Sessions for the same corpus serialize across
    fibers and processes. Sessions for one corpus cannot be nested. Waiting is
    cancellable. [check] enforces caller authority before refresh, after
    locking, and before returning.

    [f] must not retain the store, its corpus or provider after returning.
    Source files are never modified. Provider requests occur only during
    embedding or semantic search. HTTP credentials are read lazily from an owned
    private file and require HTTPS. Native model files are fingerprinted without
    loading GPU weights. Text and image provider identities are resolved
    independently when their APIs are used. Native models load on first
    inference and are reused across sessions while their contents and settings
    remain unchanged. Concurrent provider and image accessors share one
    initialization. Initialization failures are recorded for the session. *)

val corpus : t -> Bushel_knowledge.t
val provider : t -> Retrieval.Embedding.provider option
val database : t -> string

val clock : t -> float Eio.Time.clock_ty Eio.Resource.t
(** [clock t] is the wall clock supplied to the current store session. *)

val status : t -> string
(** [status t] is JSON with the database path, corpus generation, document and
    passage counts, provider identity and current embedding coverage. It makes
    no provider request. *)

val index :
  ?on_progress:(indexed:int -> total:int -> unit) ->
  t ->
  check:(unit -> unit) ->
  max_batches:int ->
  string
(** [index t ~check ~max_batches] embeds outstanding passages and returns status
    JSON with [newly_indexed]. The budget must be between 1 and 10000. Repeating
    the operation reuses current vectors. [on_progress] receives initial corpus
    coverage, then updated counts after each fully stored batch. [total] is the
    number of corpus passages, including those outside the batch budget.
    Callback exceptions propagate. *)

val index_all :
  ?on_progress:(indexed:int -> total:int -> unit) ->
  t ->
  check:(unit -> unit) ->
  string
(** [index_all t ~check] embeds all outstanding passages in the current session
    and returns status JSON with the aggregate [newly_indexed] count. Progress
    includes initial coverage and each completed batch. Cancellation and errors
    propagate, retaining completed batches for the next invocation. *)

val image_corpus : t -> Bushel_knowledge.t
(** [image_corpus t] refreshes and returns the separate image corpus, keyed by
    the canonical Srcsetter output directory. Text commands do not read images. *)

val image_provider : t -> Retrieval.Embedding.provider option
(** [image_provider t] returns the independently configured vision provider. Its
    model weights are loaded on first inference. *)

val image_status : t -> string
(** [image_status t] is image corpus counts, model identity and coverage JSON.
    It fingerprints files without loading GPU weights. *)

val index_images :
  ?on_progress:(indexed:int -> total:int -> unit) ->
  t ->
  check:(unit -> unit) ->
  max_batches:int ->
  string
(** [index_images t ~check ~max_batches] indexes at most [max_batches] batches
    of image pixels and returns status JSON. The budget is between 1 and 10000.
    Progress receives initial coverage and each completed batch. *)

val index_images_all :
  ?on_progress:(indexed:int -> total:int -> unit) ->
  t ->
  check:(unit -> unit) ->
  string
(** [index_images_all t ~check] indexes all outstanding image pixels and returns
    aggregate status JSON. Text vectors are kept in an independent index. *)

val images_search :
  ?mode:string ->
  ?offset:int ->
  t ->
  check:(unit -> unit) ->
  query:string ->
  string
(** [images_search t ~check ~query] returns image hits. The default mode is
    semantic. Lexical search uses metadata only, without model inference. Hybrid
    mode combines image metadata and visual rankings. *)

val images_get : t -> check:(unit -> unit) -> id:string -> offset:int -> string
(** [images_get t ~check ~id ~offset] returns image metadata and pixel paths. *)

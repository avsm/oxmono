(** Authoritative SQLite corpus. Every mutating operation is atomic. *)

type t

val open_db : sw:Eio.Switch.t -> _ Eio.Path.t -> t
val memory : sw:Eio.Switch.t -> unit -> t
val revision : t -> int
val get : t -> Siftbot_model.source -> Siftbot_model.message option
val messages : t -> Siftbot_model.message list

val message_ids_after :
  t ->
  service:string ->
  account:string ->
  after:string ->
  limit:int ->
  string list
(** [message_ids_after db ~service ~account ~after ~limit] returns at most
    [limit] IDs strictly greater than [after], in bytewise order. *)

val has_thread : t -> service:string -> account:string -> thread:string -> bool
(** [has_thread db ~service ~account ~thread] tests for a live thread member. *)

val search : t -> string -> Siftbot_model.message list
(** [search db query] returns at most 100 live messages matching FTS5. *)

val checkpoint : t -> string -> string option

val commit_batch :
  ?hydrate:Siftbot_model.source list ->
  ?hydrated:Siftbot_model.source * int option ->
  t ->
  job:string ->
  expected:string option ->
  cursor:string ->
  Siftbot_model.message list ->
  Siftbot_model.source list ->
  unit
(** [commit_batch db ~job ~expected ~cursor messages deleted] replaces messages,
    invalidates derived data, and advances a checkpoint in one transaction. A
    changed checkpoint raises [Conflict] before writing. *)

exception Conflict

val snapshot : t -> query:string -> Siftbot_model.snapshot

val propose :
  t -> Siftbot_model.area list -> Siftbot_model.assignment list -> unit
(** [propose db areas assignments] saves suggestions, never human decisions. *)

val edit : t -> revision:int -> Siftbot_model.edit -> explanation:string -> unit
(** [edit db ~revision edit ~explanation] applies a reviewed edit. Undo appends
    a decision revision and restores the prior review state. *)

val cache :
  t ->
  source:Siftbot_model.source ->
  revision:string ->
  kind:string ->
  value:string ->
  unit

val cached : t -> source:Siftbot_model.source -> kind:string -> string option

val audit :
  t ->
  model:string ->
  prompt_version:string ->
  corpus_revision:int ->
  question:string ->
  answer:string ->
  outcome:string ->
  tools:string list ->
  unit
(** [audit db ~model ~prompt_version ~corpus_revision ~question ~answer ~outcome ~tools]
    records a private bounded assistant turn and its outcomes. *)

val vector_index :
  t -> model:string -> dimensions:int -> normalization:string -> string
(** [vector_index db ~model ~dimensions ~normalization] opens a vec0 index whose
    identity pins all three parameters. *)

val vector_current :
  t ->
  index:string ->
  source:Siftbot_model.source ->
  revision:string ->
  input_hash:string ->
  bool

val vector_put :
  t ->
  index:string ->
  source:Siftbot_model.source ->
  revision:string ->
  input_hash:string ->
  float list ->
  unit
(** [vector_put db ~index ~source ~revision ~input_hash vector] saves a vector
    only if its source revision is still current. *)

val vector_search :
  t ->
  index:string ->
  limit:int ->
  float list ->
  (Siftbot_model.message * float) list
(** [vector_search db ~index ~limit vector] returns current sources by L2
    distance. Deleted and superseded sources cannot appear. *)

val freeze : t -> id:string -> revision:int -> payload:string -> unit
(** [freeze db ~id ~revision ~payload] saves immutable evaluation inputs and
    results, refusing a changed corpus or an existing different payload. *)

val evaluations : t -> (string * string) list
(** [evaluations db] returns frozen evaluation IDs and JSON payloads. *)

val reserve_task :
  t ->
  key:string ->
  revision:int ->
  source:Siftbot_model.source ->
  source_revision:string ->
  title:string ->
  Siftbot_model.source * string
(** [reserve_task db ~key ~revision ~source ~source_revision ~title] durably
    pins a capture's source before calling Dooit. Retries use the original
    source. *)

val task_receipt : t -> key:string -> status:string -> receipt:string -> unit

val snooze :
  t ->
  revision:int ->
  target:Siftbot_model.source ->
  until_date:string ->
  explanation:string ->
  unit
(** [snooze db ~revision ~target ~until_date ~explanation] stores a local snooze
    with a source revision. Changed source evidence reopens the item. *)

val snoozed : t -> now:string -> Siftbot_model.source -> bool

val hydration :
  t -> service:string -> account:string -> (Siftbot_model.source * int) option
(** [hydration db ~service ~account] returns a durable pending thread page. *)

val checkpoints : t -> (string * string) list
(** [checkpoints db] returns durable job identifiers and cursors. *)

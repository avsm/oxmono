(** Durable spindle state. Operations serialize transactions across fibers. *)

type t

exception Inbox_full

val set_limits : t -> Operations.t -> unit
(** [set_limits store policy] bounds admission to the durable inbox. *)

val usage : t -> string -> int * int * float
(** [usage store namespace] returns count, payload bytes and oldest update time,
    or zero for the time of an empty namespace. *)

val pending_source : t -> string -> int
(** [pending_source store source] counts pending events from [source]. *)

val pending_repo : t -> string -> bool
(** [pending_repo store repo] reports queued knot events for [repo]. Recovery
    lets these preserve push options and actor metadata before scanning refs. *)

val ref_state : t -> repo:string -> ref_:string -> (string * int64) option
(** [ref_state store ~repo ~ref_] is the commit and stream position of the last
    checkpoint of [ref_] in [repo], if any. *)

val checkpoint_ref :
  t -> repo:string -> ref_:string -> sha:string -> position:int64 -> unit
(** [checkpoint_ref store ~repo ~ref_ ~sha ~position] advances a ref checkpoint
    monotonically. *)

val touch_ref : t -> repo:string -> ref_:string -> unit
(** [touch_ref store ~repo ~ref_] retains a verified live ref. *)

val prune : t -> now:float -> Operations.t -> int * int
(** [prune store ~now policy] removes completed history and expired receipts,
    retaining replay floors, pending events, active jobs and live JWT nonces. It
    returns removed pipeline and receipt counts. *)

val open_ : sw:Eio.Switch.t -> _ Eio.Path.t -> t
(** [open_ ~sw directory] initializes the SQLite database in [directory]. *)

val get : t -> string -> string -> string option
(** [get store namespace key] is the value of [key] in [namespace]. *)

val count : t -> string -> int
(** [count t namespace] is the number of keys in [namespace], counted without
    reading their values. *)

val list : t -> string -> (string * string) list
(** [list store namespace] is every key and value of [namespace], in key order.
    Use {!fold} for a namespace that may be large. *)

val fold :
  t ->
  string ->
  ?descending:bool ->
  init:'a ->
  f:('a -> string * string -> 'a) ->
  unit ->
  'a
(** [fold store namespace ~init ~f ()] visits keys in batches of 128 without
    retaining the namespace in memory. Concurrent writes may be observed. *)

val list_prefix : t -> string -> string -> (string * string) list
(** [list_prefix store namespace prefix] is every key and value of
    [namespace] whose key starts with [prefix], in key order. *)

val fold_views :
  t -> repo:string -> init:'a -> f:('a -> string * string -> 'a) -> 'a
(** [fold_views store ~repo ~init ~f] is {!fold} over the [pipeline-view]
    namespace, newest key first, restricted to views whose [repo] is [repo].
    An index keeps it proportional to that repository's pipelines. *)

val ready : t -> string -> now:float -> limit:int -> (string * string) list
(** [ready store namespace ~now ~limit] reads up to 128 due tasks. *)

val defer : t -> string -> string -> now:float -> int
(** [defer store namespace key ~now] backs off a failed task, doubling from one
    second to one hour, and is the number of times it has failed. Deleting the
    task clears its retry state. *)

val hold : t -> string -> string -> behind:string -> unit
(** [hold store namespace key ~behind] makes task [key] due when task [behind]
    is, without counting a failure, so that it is not handled first. *)

val delay : t -> string -> string -> until:float -> unit
(** [delay store namespace key ~until] makes task [key] due at [until] without
    counting a failure. *)

val waiting : t -> string -> now:float -> string list
(** [waiting store namespace ~now] is the keys, in order, of tasks that are not
    yet due. *)

val advance : t -> source:string -> cursor:string -> unit
(** [advance store ~source ~cursor] moves the stream checkpoint past an event
    that is not stored. Cursors only move forward. *)

val put : t -> string -> string -> string -> unit
(** [put store namespace key value] sets [key] in [namespace]. *)

val delete : t -> string -> string -> unit
(** [delete store namespace key] removes [key] from [namespace], and its task
    state. *)

val schedule : t -> string -> string -> unit
(** [schedule store namespace key] writes a fresh task generation. *)

val complete :
  t ->
  string ->
  string ->
  value:string ->
  puts:(string * string * string) list ->
  deletes:(string * string) list ->
  bool
(** [complete store namespace key ~value ~puts ~deletes] applies mutations and
    deletes a task only if its generation is still [value]. *)

val batch :
  t ->
  puts:(string * string * string) list ->
  deletes:(string * string) list ->
  unit
(** [batch store ~puts ~deletes] applies all mutations atomically. Tuples name
    the namespace, key and, for writes, value. *)

val append_log : t -> pipeline:string -> workflow:string -> string -> int64
(** [append_log store ~pipeline ~workflow event] durably appends [event] and
    returns its sequence. Sequences are never reused. A workflow's journal
    is its log, kept until its pipeline is deleted. *)

val delete_logs : t -> pipeline:string -> workflow:string -> unit
(** [delete_logs t ~pipeline ~workflow] removes every log event of a workflow,
    for one that runs again from the start. *)

val logs : t -> pipeline:string -> workflow:string -> (int64 * string) list
(** [logs store ~pipeline ~workflow] reads journal entries in sequence order.
    Workflow admission bounds their total encoded size to 1 MiB. *)

val consume :
  t -> now:float -> issuer:string -> jti:string -> expires:float -> bool
(** [consume store ~now ~issuer ~jti ~expires] accepts a nonce once, retaining
    it until expiry. Expired rows are removed. It refuses a nonce when
    [issuer] already holds 1024 unexpired ones, or when 262144 are held in
    all, so that one issuer cannot exhaust what the others need. *)

val enqueue :
  t -> source:string -> cursor:string -> key:string -> value:string -> unit
(** [enqueue store ~source ~cursor ~key ~value] checkpoints the stream and
    stores an event unless completed or below a pruned receipt floor, in one
    transaction. Cursors advance monotonically as signed 64-bit integers.
    [Inbox_full] leaves both the event and cursor unchanged. *)

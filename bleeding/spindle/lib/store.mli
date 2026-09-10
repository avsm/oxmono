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

val ref_state : t -> repo:string -> ref_:string -> (string * int64) option

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
val list : t -> string -> (string * string) list

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

val ready : t -> string -> now:float -> limit:int -> (string * string) list
(** [ready store namespace ~now ~limit] reads up to 128 due tasks. *)

val defer : t -> string -> string -> now:float -> unit
(** [defer store namespace key ~now] backs off a failed task from one second to
    one minute. Deleting the task clears its retry state. *)

val put : t -> string -> string -> string -> unit
val delete : t -> string -> string -> unit

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

val consume :
  t -> now:float -> issuer:string -> jti:string -> expires:float -> bool
(** [consume store ~now ~issuer ~jti ~expires] accepts a nonce once, retaining
    it until expiry. Expired rows are removed. Capacity exhaustion fails closed.
*)

val enqueue :
  t -> source:string -> cursor:string -> key:string -> value:string -> unit
(** [enqueue store ~source ~cursor ~key ~value] checkpoints the stream and
    stores an event unless completed or below a pruned receipt floor, in one
    transaction. Cursors advance monotonically as signed 64-bit integers.
    [Inbox_full] leaves both the event and cursor unchanged. *)

(** Durable spindle state. Operations serialize transactions across fibers. *)

type t

val open_ : sw:Eio.Switch.t -> _ Eio.Path.t -> t
(** [open_ ~sw directory] initializes the SQLite database in [directory]. *)

val get : t -> string -> string -> string option
val list : t -> string -> (string * string) list
val put : t -> string -> string -> string -> unit
val delete : t -> string -> string -> unit

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
    stores an event unless it has already completed, in one transaction. Cursors
    advance monotonically as signed 64-bit integers. *)

(** Replay policy and current-ref recovery when event history is unavailable. *)

val dedup : repo:string -> ref_:string -> sha:string -> string

val gap :
  Store.t ->
  source:string ->
  now:float ->
  cursor:string ->
  reason:string ->
  unit
(** [gap store ~source ~now ~cursor ~reason] schedules recovery, preserving the
    first cursor and cause until the pending gap has been reconciled. *)

val resume :
  Store.t ->
  Operations.t ->
  source:string ->
  jetstream:bool ->
  now:float ->
  bool
(** [resume store policy ~source ~jetstream ~now] persists a recovery obligation
    before replacing a cursor beyond the replay window or local receipt floor.
    It returns whether the cursor was reset, so Jetstream can start at its live
    tail without requesting unavailable history. *)

val seed : Engine.t -> unit
val parse_refs : string -> (string * string * bool) list
val repo : Engine.t -> string -> unit
val schedule : Engine.t -> source:string -> unit
val settled : Store.t -> now:float -> unit

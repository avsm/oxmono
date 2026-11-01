(** Persistent key/value storage owned by the authenticated bot.

    Only authenticated bot accounts can use these endpoints. Keys and values are
    strings. Endpoint failures are returned as {!Error.t}. Cancellation
    propagates. *)

val get :
  Client.t ->
  ?keys:string list ->
  unit ->
  ((string * string) list, Error.t) result
(** [get client ~keys ()] is the stored entries selected by [keys]. Omitting
    [keys] selects all entries. Duplicate keys or non-string values in the
    response return {!Error.t.constructor-Json}. *)

val set : Client.t -> (string * string) list -> (unit, Error.t) result
(** [set client entries] stores [entries], replacing values with the same keys.
    Duplicate keys in [entries] return {!Error.t.constructor-Json} before I/O.
*)

val remove : Client.t -> ?keys:string list -> unit -> (unit, Error.t) result
(** [remove client ~keys ()] deletes the selected entries. Omitting [keys]
    deletes all entries owned by the bot. *)

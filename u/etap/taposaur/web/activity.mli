(** Recent website requests, kept in memory for the live activity page.
    Entries carry page names and query parameters, never bodies or headers,
    and are discarded when the process exits. *)
type entry = {
  seq : int; time : float; page : string; query : (string * string) list;
  status : int; elapsed_ms : float;
}
type t
val create : ?capacity:int -> unit -> t
(** [create ()] keeps the newest [capacity] entries, default 500. *)
val record : t -> time:float -> page:string -> query:(string * string) list ->
  status:int -> elapsed_ms:float -> unit
val since : t -> int -> entry list
(** [since t seq] lists entries newer than [seq], oldest first. *)
val cursor : t -> int
(** [cursor t] is the newest sequence number, zero when empty. *)

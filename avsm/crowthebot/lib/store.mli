(** Profile-local authority and bounded conversation persistence. *)

type role = Friend | Bot | Unknown

val role_string : role -> string
val role_of_string : string -> role

type person = { user : string; role : role; allowed : bool }
type message = { role : string; body : string }
type t

val create : Sqlite3_eio.t -> admin:string -> t
(** [create db ~admin] initializes the schema or verifies its saved primary
    admin. A different admin is rejected. The caller owns [db]'s switch. *)

val admin : t -> string
val person : t -> string -> person

val observe : t -> string -> unit
(** [observe t user] records an unknown sender without granting access. At most
    1000 unknown identities are retained. *)

val set_person :
  t -> actor:string -> user:string -> role:role -> allowed:bool -> unit
(** [set_person t ~actor ~user ~role ~allowed] requires the primary admin's
    exact Matrix ID. It cannot modify the admin or allow an unknown identity.
    Revocation also removes the user's conversation history. *)

val people : t -> person list
val add_room : t -> string -> unit
val rooms : t -> string list
val clear : t -> room:string -> user:string -> unit
val history : t -> room:string -> user:string -> message list

val claim : t -> room:string -> event:string -> bool
(** [claim t ~room ~event] records an event before effects. It returns false for
    duplicates. The last 2048 event IDs per room are retained. *)

val append :
  t ->
  room:string ->
  user:string ->
  max_messages:int ->
  max_bytes:int ->
  message list ->
  unit
(** [append t ~room ~user ~max_messages ~max_bytes messages] appends and trims
    the thread atomically. Most recent messages are kept within both bounds. *)

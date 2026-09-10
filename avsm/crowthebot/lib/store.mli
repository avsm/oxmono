(** Profile-local authority and bounded conversation persistence. *)

type role = Friend | Bot | Unknown

val role_string : role -> string
val role_of_string : string -> role

type person = { user : string; role : role; allowed : bool }
type message = { role : string; body : string }
type t

val room_context : t -> Room_context.t
(** [room_context store] accesses bounded observations shared within each room.
*)

val trace : t -> Trace.t
(** [trace store] records model exchanges in the profile database. *)

val compaction : t -> Compaction.t
(** [compaction store] accesses conversation summaries within this profile. *)

val create : ?now:(unit -> float) -> Sqlite3_eio.t -> admin:string -> t
(** [create db ~admin] initializes the schema or verifies its saved primary
    admin. A different admin is rejected. [now] defaults to Unix time. Versions
    1 through 7 are migrated atomically. Unfinished tool calls and reminder runs
    become interrupted on reopening. The caller owns [db]'s switch and must open
    the profile only once at startup. *)

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

val add_direct_room : t -> room:string -> peer:string -> unit
(** [add_direct_room t ~room ~peer] remembers an accepted direct invitation. It
    does not enable the room as a group or grant the peer access. *)

val direct_peer : t -> string -> string option
(** [direct_peer t room] returns the saved invitation's peer. The adapter must
    still check current membership and access before treating [room] as a DM. *)

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
  ?event:string ->
  ?source_event:string ->
  message list ->
  unit
(** [append t ~room ~user ~max_messages ~max_bytes messages] appends and trims
    the thread atomically. Most recent messages are kept within both bounds.
    [event] and [source_event] retain message or scheduler provenance. *)

val timestamp : float -> string
val today : t -> string
val yesterday : t -> string
val now : t -> float

val validate_day : string -> unit
(** UTC timestamps use RFC 3339. Days use [YYYY-MM-DD]. *)

type fact = {
  fact_id : int;
  created_at : string;
  author : string;
  room : string;
  event : string;
  source : string;
  body : string;
}

val add_fact :
  t ->
  actor:string ->
  room:string ->
  event:string ->
  source:string ->
  body:string ->
  int
(** [add_fact t ~actor ~room ~event ~source ~body] stores a shared fact with its
    timestamp and provenance. [source] is [command] or [observation]. Facts are
    limited to 2048 bytes. All fact operations require an allowed friend or the
    admin, and raise [Invalid_argument] when access or input is invalid. *)

val get_fact : t -> actor:string -> int -> fact option

val search_facts : t -> actor:string -> query:string -> fact list
(** [search_facts t ~actor ~query] returns up to 20 FTS5 matches ordered by
    relevance, or the 20 newest facts for an empty query. Queries are limited to
    256 bytes. Malformed FTS5 syntax raises [Invalid_argument]. *)

val erase_fact : t -> actor:string -> int -> bool
(** [erase_fact t ~actor id] removes a fact and its search index entry. Any
    allowed friend may erase any fact. IDs are never reused. *)

type tool_use = {
  log_id : int;
  started_at : string;
  finished_at : string option;
  actor : string;
  room : string;
  event : string;
  source : string;
  call_id : string;
  tool : string;
  arguments : string;
  result : string;
  status : string;
}

val start_tool :
  t ->
  actor:string ->
  room:string ->
  event:string ->
  source:string ->
  call_id:string ->
  tool:string ->
  arguments:string ->
  int

val finish_tool : t -> int -> status:string -> result:string -> unit
(** Tool logs start before effects and finish independently of reply delivery.
    Status is [ok], [rejected], [error], or [cancelled]. Callers bound payloads
    and omit memory content. Raw log access is an operator capability. *)

val tool_uses :
  t -> day:string -> after:int -> through:int -> limit:int -> tool_use list
(** [tool_uses t ~day ~after ~through ~limit] reads an ascending page of at most
    100 records with IDs greater than [after] and at most [through]. *)

val tool_snapshot : t -> day:string -> int * int * bool
(** [tool_snapshot t ~day] is the maximum ID, total count and whether any call
    is still running. *)

type daily_note = {
  day : string;
  generated_at : string;
  model : string;
  last_tool_id : int;
  tool_count : int;
  body : string;
}

val get_note : t -> string -> daily_note option

val save_note :
  t ->
  day:string ->
  model:string ->
  last_tool_id:int ->
  tool_count:int ->
  body:string ->
  unit

val pending_note_days : t -> string list
(** [pending_note_days t] returns up to seven completed UTC days with missing or
    outdated notes, oldest first. Running calls defer their day's note. *)

type reminder_target =
  | Memory of int
  | Tool of { namespace : string; key : int }

type reminder = {
  reminder_id : int;
  target : reminder_target;
  creator : string;
  room : string;
  event : string;
  created_at : string;
  instruction : string;
  cron : string option;
  until_at : float option;
  next_at : float;
  state : string;
}

val add_reminder :
  t ->
  actor:string ->
  room:string ->
  event:string ->
  fact_id:int ->
  instruction:string ->
  cron:string option ->
  until_at:float option ->
  next_at:float ->
  int
(** [add_reminder t ...] links a reminder to an existing fact. The scheduler
    validates the schedule. Erasing the fact deletes its reminders. *)

val reminders : t -> actor:string -> reminder list

val cancel_reminder : t -> actor:string -> int -> bool
(** Shared reminder operations require the admin or an allowed friend. *)

val get_reminder : t -> int -> reminder option
val due_reminders : t -> reminder list

val claim_reminder : t -> reminder -> next_at:float option -> int option
(** [claim_reminder t job ~next_at] atomically claims an occurrence before any
    effects and advances or completes its schedule. Claims survive restarts.
    [None] means another caller already claimed or cancelled the occurrence. *)

val finish_reminder : t -> int -> status:string -> unit

val feeds : t -> Feed_store.t
(** [feeds t] is the typed feed-state capability for this profile. Its SQL
    transactions share the authority store's lock. *)

val locations : t -> Location_store.t
(** [locations t] projects the typed location state capability. *)

val calendars : t -> Calendar_store.t
val caldav : t -> Caldav_store.t
val emails : t -> Email_cache.t

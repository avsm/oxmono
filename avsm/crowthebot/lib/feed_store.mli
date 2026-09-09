(** Typed, profile-wide feed state. Entries and HTTP state are separate from
    conversational memory. Every operation rechecks admin/friend authority. *)

type t

val create :
  db:Sqlite3_eio.t ->
  mutex:Eio.Mutex.t ->
  admin:string ->
  now:(unit -> float) ->
  timestamp:(float -> string) ->
  t
(** [create ...] is an operator constructor. [mutex] must be shared by every
    user of [db]. Feed tools receive [t], never the database handle. *)

val init : Sqlite3_eio.t -> unit
(** [init db] installs the feed schema within the caller's migration
    transaction. *)

val now : t -> float

type source = {
  source_id : int;
  url : string;
  kind : string;
  title : string;
  etag : string option;
  last_modified : string option;
  checked_at : float option;
  success_at : string option;
  error : string option;
  failures : int;
  retry_at : float;
}

type subscription = {
  subscription_id : int;
  source_id : int;
  creator : string;
  room : string;
  event : string;
  created_at : string;
  cron : string;
  until_at : float option;
}

type member = {
  member_id : int;
  subscription_id : int;
  source_id : int;
  job_id : int;
  initialized : bool;
  cursor : int;
}

type entry = {
  entry_id : int;
  source_id : int;
  key : string;
  title : string;
  url : string option;
  published : string option;
  summary : string;
  observed_at : string;
}

val get : t -> actor:string -> int -> subscription * source
val list : t -> actor:string -> after:int -> (subscription * source) list
val members : t -> actor:string -> int -> member list
val poll_context : t -> actor:string -> int -> member * subscription * source

val cancel_member : t -> actor:string -> member_id:int -> unit
(** [cancel_member ...] stops a nested OPML job without changing shared HTTP
    cache state. A directly subscribed OPML root remains usable. *)

val status :
  t ->
  actor:string ->
  subscription_id:int ->
  after:int ->
  (member * source * string) list

val add :
  t ->
  actor:string ->
  room:string ->
  event:string ->
  url:string ->
  cron:string ->
  until_at:float option ->
  next_at:float ->
  subscription * bool
(** [add ...] atomically adds a subscription and its cron job. Repeated URLs in
    the same room return the existing subscription without changing its owner.
*)

val remove : t -> actor:string -> int -> bool
(** [remove t ~actor id] removes a subscription, cancels its jobs and collects
    sources and cached entries that no other subscription uses. *)

val sync_opml : t -> actor:string -> member_id:int -> urls:string list -> unit
(** [sync_opml ...] reconciles an OPML root's memberships and cron jobs
    atomically. Imported feeds cannot import more OPML documents. *)

val opml_urls : t -> actor:string -> member_id:int -> string list

val entries :
  t -> actor:string -> subscription_id:int -> after:int -> entry list

val complete_poll :
  t ->
  actor:string ->
  member_id:int ->
  kind:string ->
  title:string ->
  etag:string option ->
  last_modified:string option ->
  entries:entry list ->
  unit

val not_modified : t -> actor:string -> member_id:int -> unit
val failed : t -> actor:string -> member_id:int -> string -> unit

val pending : t -> actor:string -> member_id:int -> entry list
(** [pending ...] establishes a baseline on the first successful poll. Later
    calls return up to ten undelivered entries, including after a failed send.
*)

val acknowledge : t -> actor:string -> member_id:int -> through:int -> unit
(** [acknowledge ...] advances the cursor after confirmed delivery. *)

val request_poll : t -> actor:string -> int -> unit
(** [request_poll t ~actor id] makes the subscription's active imported-feed
    cron jobs due now. Cancelled jobs stay cancelled. *)

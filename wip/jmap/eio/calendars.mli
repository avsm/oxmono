(** Archival reads for JMAP Calendars (draft-ietf-jmap-calendars-28). Objects
    are validated by the typed protocol codecs and retain their exact JSON
    source. Use {!Jmap.Chain} for ordinary typed calendar reads. *)

type kind = Calendar | Event | Identity

val kinds : kind list
val kind_name : kind -> string
val kind_of_string : string -> kind

type t
type identity = { account : string; username : string; page_size : int }
type receipt = { method_name : string; request : string; response : string }
type item = { remote_id : string; raw : string; ical : string option }

type fetched = {
  state : string;
  items : item list;
  not_found : string list;
  receipts : receipt list;
  ical_supported : bool;
}

type changes = {
  old_state : string;
  new_state : string;
  more : bool;
  created : string list;
  updated : string list;
  destroyed : string list;
  receipt : receipt;
}

type page = {
  query_state : string;
  position : int;
  ids : string list;
  total : int option;
  receipt : receipt;
}

exception Expired_state of receipt
exception Method_error of Jmap.Proto.Error.Method_error.t * receipt

val create : ?account_id:string -> Client.t -> t
(** [create ?account_id client] confines calendar operations to one account. The
    default is the primary calendar account. This API sends only get, changes,
    query and blob download requests. It cannot grant fewer privileges to the
    underlying bearer token. Restrict that token on the server too. *)

val identity : t -> identity
(** [identity t] discovers and pins the calendar account and authenticated user.
*)

val archive :
  ?include_ical:bool -> t -> kind -> ids:string list option -> fetched
(** [archive t kind ~ids] fetches typed objects and retains their source bytes.
    [include_ical] defaults to [false]. When true, events also fetch the
    [iCalendar] property explicitly at the same state. For ordinary typed reads,
    use the calendar builders in {!Jmap.Chain} with {!Client.call}. *)

val changes : t -> kind -> since:string -> changes
(** [changes t kind ~since] returns one bounded change page. *)

val page : t -> position:int -> page
(** [page t ~position] queries base events without filters or recurrence
    expansion. *)

val download : t -> blob:string -> string
(** [download t ~blob] downloads one account-scoped blob within the response
    cap. *)

val mirror_source : t -> kind -> (item, receipt) Jmap.Mirror.source
(** [mirror_source t kind] supplies account-scoped reads for a durable mirror.
    Events use a paginated base-event snapshot. Calendars and participant
    identities use an unfiltered get. *)

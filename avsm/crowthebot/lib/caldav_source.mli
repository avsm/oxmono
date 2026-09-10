type t
(** Private credentials and a read-only CalDAV capability. *)

val configuration : Tool_config.t

val initialize :
  sw:Eio.Switch.t ->
  fetch:_ Fetch.t ->
  clock:[> float Eio.Time.clock_ty ] Eio.Resource.t ->
  Jsont.json ->
  t

type identity = { principal : string; key : string }

type collection = Caldav_data.collection = {
  href : string;
  title : string;
  properties : string;
  sync : bool;
}

type change = Caldav_data.change = {
  href : string;
  etag : string option;
  removed : bool;
}

type page = Caldav_data.page = {
  token : string option;
  more : bool;
  inventory : bool;
  changes : change list;
}

type item = Caldav_data.item = {
  href : string;
  etag : string option;
  raw : string;
  search : string;
  parsed : bool;
}

exception Invalid_sync_token

val error : exn -> string
val identity : t -> identity
val discover : t -> collection list

val next : t -> collection -> token:string option -> page
(** [next t collection ~token] reads a sync report or a complete ETag inventory.
    It never fetches arbitrary links or issues calendar writes. *)

val get : t -> collection:string -> string -> item option
(** [get t ~collection href] retains the original GET body, including unknown
    properties, recurrence exceptions and embedded attachments. External
    attachment URLs are retained without fetching them. *)

val agenda :
  t -> collection -> Caldav_agenda.window -> Caldav_agenda.resource list
(** [agenda t collection window] issues one bounded read-only calendar-query
    with recurrence expansion. Truncation, unreadable members and unexpanded
    rules are reported as errors rather than an apparently complete agenda. *)

(** Durable review state, independent of the rebuildable catalogue.
    Inputs, observations and exports are deduplicated in this database. *)
module R = Taposaur_review
type read
type write
type 'access t
exception Not_found
val open_read : sw:Eio.Switch.t -> _ Eio.Path.t -> read t
val open_write : ?audit:Taposaur_audit.session -> sw:Eio.Switch.t -> _ Eio.Path.t -> write t
type review = {
  slug : string; title : string; requester : string; owner : string;
  created : float; revision : int;
}
type revision = {
  review : review; number : int; definition : R.Definition.t;
  actor : string; reason : string; created : float;
}
type input_result = {
  key : string; blob : string; partial : bool;
  format : R.Definition.format;
  occurrences : int; diagnostics : R.diagnostic list;
}
type run = {
  id : int; slug : string; revision : int; actor : string; created : float;
  partial : bool; members : int; inputs : input_result list;
}
type snapshot = {
  id : int; run_id : int; slug : string; revision : int;
  parent : int option; label : string; actor : string; created : float;
  title : string; requester : string; owner : string;
  members : int; titles : int; abstracts : int; held : int;
  partial : bool; manifest : string;
}
val put_blob : write t -> string -> string
val blob : 'a t -> string -> string
val list : 'a t -> review list
val find : 'a t -> string -> review option
val define : write t -> slug:string -> title:string -> requester:string ->
  owner:string -> actor:string -> reason:string -> expected:int ->
  R.Definition.t -> int
(** [define store ... ~expected definition] appends an immutable revision.
    [expected] is zero for a new review or its current revision number.
    Conflicts and missing list blobs are refused atomically. Review identity
    fields cannot be changed by appending a definition. *)
val revision : 'a t -> slug:string -> number:int option -> revision
val revisions : 'a t -> slug:string -> revision list
val publish_run : write t -> revision -> actor:string ->
  input_result list -> int
(** [publish_run store revision ...] publishes complete materialized results
    atomically. Membership is derived from the retained input blobs. Partial
    inputs remain explicit. No running state is exposed. *)
val run : 'a t -> int -> run
val runs : 'a t -> slug:string -> run list
val members : 'a t -> int -> R.member list
val publish_snapshot : ?max_bytes:int -> write t -> run_id:int -> parent:int option ->
  label:string -> actor:string -> R.observation list -> int
(** [publish_snapshot store ... rows] verifies exact run membership, freezes
    review context and publishes rows, manifest and three cached exports in
    one transaction. [parent] must belong to the same run. *)
val snapshot : 'a t -> int -> snapshot
val snapshots : 'a t -> slug:string -> snapshot list
val rows : ?after:string -> ?limit:int -> 'a t -> int -> R.observation list
(** [rows store snapshot] uses an exclusive DOI cursor and a 1..100 page. *)
val browse : ?after:string -> ?limit:int -> ?publisher:string ->
  ?filter:(R.Statistics.field * bool) -> 'a t -> int -> R.observation list
(** [browse store snapshot] filters indexed frozen observations. An empty
    publisher means unknown. The boolean selects present or missing.
    Older snapshots require [cache_statistics] before filtered browsing. *)
val export : 'a t -> int -> R.export_format -> string
val statistics : 'a t -> int -> R.Statistics.t option
(** [statistics store id] reads cached coverage without scanning papers. *)
val cache_statistics : write t -> int -> unit
(** [cache_statistics store id] caches coverage and paper filter indexes from
    frozen observations. Existing manifests and exports remain unchanged. *)
type difference = {
  added : int; removed : int; changed : int; abstracts_added : int;
}
val compare : 'a t -> before:int -> after:int -> difference
(** [compare store ~before ~after] compares snapshots of the same review. *)

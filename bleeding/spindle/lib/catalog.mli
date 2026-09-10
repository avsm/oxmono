type repo = {
  did : string;
  owner : string;
  rkey : string;
  knot : string;
  source : string;
}
(** PDS assignments checked against canonical repository identities. *)

type t

exception Pending
(** [Pending] means current assignment or membership reconciliation must finish
    before a repository can be used. Callers should retry. *)

val v :
  store:Store.t ->
  network:Network.t ->
  owner:string ->
  hostname:string ->
  static:repo option ->
  t

val members : t -> string list
val repositories : t -> string list
val managed : t -> string -> repo option

val current : t -> repo -> bool
(** [current catalog repo] rechecks local membership and assignment after remote
    authorization or source resolution. Pending refreshes raise [Pending]. *)

val verified : t -> string -> repo
val knots : t -> string list
val authorized : t -> repo -> string -> bool
val bootstrap : t -> unit

val refresh : t -> string -> value:string -> unit
(** [refresh catalog key ~value] replaces a queued collection from its current
    PDS state if the task generation remains [value]. Newer notices invalidate
    an in-flight snapshot. Successful grants schedule member collections. *)

val notice : t -> owner:string -> collection:string -> rkey:string -> unit
(** [notice catalog ~owner ~collection ~rkey] queues a durable refresh for an
    authorized publisher. Event payloads never grant catalog authority. *)

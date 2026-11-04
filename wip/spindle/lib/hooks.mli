(** Webhooks: notifying other services of a repository's events, as the
    reference spindle does. *)

type t

val v :
  sw:Eio.Switch.t ->
  store:Store.t ->
  settings:Settings.t ->
  allow_private:bool ->
  < net : _ Eio.Net.t
  ; mono_clock : _ Eio.Time.Mono.t
  ; clock : float Eio.Time.clock_ty Eio.Resource.t
  ; .. > ->
  t
(** [v ~sw ~store ~settings ~allow_private system] delivers in fibers under
    [sw]. Unless [allow_private], webhook URLs may not reach loopback, private
    or link-local addresses. *)

(** {1 Methods} *)

(** Each answers the [org.tangled.webhook] method of its name, given the
    method's input, for a repository its caller may change. Errors are XRPC
    errors. *)

val list : t -> repo:string -> Jsont.json
(** [list t ~repo] is [repo]'s webhooks, without their secrets. *)

val create : t -> repo:Catalog.repo -> Jsont.json -> Jsont.json
(** [create t ~repo input] adds a webhook, within the repository's and owner's
    [webhooks] limits. *)

val update : t -> repo:string -> Jsont.json -> unit
(** [update t ~repo input] changes a webhook's URL, secret, events or activity.
*)

val delete : t -> repo:string -> string -> unit
(** [delete t ~repo id] removes webhook [id] and its deliveries. *)

val deliveries : t -> repo:string -> ids:string list -> limit:int -> Jsont.json
(** [deliveries t ~repo ~ids ~limit] is the newest [limit] deliveries of each of
    the webhooks [ids], at most 50. *)

val retry : t -> repo:string -> Jsont.json -> unit
(** [retry t ~repo input] sends a delivery again. *)

val counts : t -> (string * int) list
(** [counts t] is how many webhooks each repository has. *)

val wipe : t -> string -> unit
(** [wipe t repo] removes [repo]'s webhooks and their deliveries. *)

(** {1 Events} *)

val push :
  t ->
  Catalog.repo ->
  actor:string ->
  ref_:string ->
  before:string ->
  after:string ->
  unit
(** [push t repo ~actor ~ref_ ~before ~after] sends the [push] event of [actor]
    moving [ref_] from [before] to [after]. *)

val renamed : t -> Catalog.repo -> old_name:string -> unit
(** [renamed t repo ~old_name] sends the [repository:renamed] event. *)

val pull_request :
  t -> Catalog.repo -> event:string -> sender:string -> Jsont.json -> unit
(** [pull_request t repo ~event ~sender pull] sends [pull_request:*] [event]
    with the [pull_request] object [pull]. *)

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

val list : t -> repo:string -> Jsont.json
val create : t -> repo:Catalog.repo -> Jsont.json -> Jsont.json
val update : t -> repo:string -> Jsont.json -> unit
val delete : t -> repo:string -> string -> unit
val deliveries : t -> repo:string -> ids:string list -> limit:int -> Jsont.json
val retry : t -> repo:string -> Jsont.json -> unit

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

val renamed : t -> Catalog.repo -> old_name:string -> unit

val pull_request :
  t -> Catalog.repo -> event:string -> sender:string -> Jsont.json -> unit
(** [pull_request t repo ~event ~sender pull] sends [pull_request:*] [event]
    with the [pull_request] object [pull]. *)

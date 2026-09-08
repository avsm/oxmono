(** Private context state shared by the bot runtime and public context facade.
*)

type clock = float Eio.Time.clock_ty Eio.Resource.t

type identity = {
  user_id : Zulip.Id.User.t;
  email : string;
  full_name : string;
}

type send =
  destination:Zulip.Message.destination ->
  content:string ->
  (Zulip.Id.Message.t, Zulip_eio.Error.t) result

type t

val v :
  sw:Eio.Switch.t ->
  client:Zulip_eio.Client.t ->
  identity:identity ->
  ?clock:[> float Eio.Time.clock_ty ] Eio.Resource.t ->
  ?plugin_store:Plugin_store.t ->
  ?is_bot:(Zulip.Id.User.t -> bool) ->
  ?send:send ->
  ?send_depth:int ->
  unit ->
  t

val connect :
  sw:Eio.Switch.t ->
  env:
    < net : _ Eio.Net.t
    ; clock : float Eio.Time.clock_ty Eio.Resource.t
    ; mono_clock : _ Eio.Time.Mono.t
    ; secure_random : _ Eio.Flow.source
    ; fs : Eio.Fs.dir_ty Eio.Path.t
    ; .. > ->
  profile:string ->
  ?site:string ->
  ?email:string ->
  ?api_key:string ->
  ?allow_insecure:bool ->
  ?transport:Zulip_eio.Transport.t ->
  unit ->
  (t, Zulip_eio.Error.t) result

val client : t -> Zulip_eio.Client.t
val identity : t -> identity
val user_id : t -> Zulip.Id.User.t
val clock : t -> clock
val plugin_store : t -> Plugin_store.t
val is_bot : t -> Zulip.Id.User.t -> bool
val remember_user : t -> Zulip.User.t -> unit

val apply_initial_state :
  t -> Zulip_eio.Initial_state.t -> (unit, Zulip_eio.Error.t) result

val observe_event : t -> Zulip.Event.t -> unit
val remember_message : t -> Zulip.Message.t -> unit

val remember_destination :
  t -> Zulip.Id.Message.t -> Zulip.Message.destination -> unit

val find_destination :
  t -> Zulip.Id.Message.t -> Zulip.Message.destination option

val enqueue :
  t -> destination:Zulip.Message.destination -> content:string -> Sent.t

val observe_payload : t -> Zulip.Event_payload.t -> unit

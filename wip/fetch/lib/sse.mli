(** Event-stream decoding and subscriptions. *)

type event = {
  name : string;
  data : string;
  id : string option;
  retry : int option;
}

val media_type : string

val decode : ?max_event:int -> Middleware.response -> event Seq.t

val connect :
  sw:Eio.Switch.t ->
  ?headers:Header.headers ->
  ?last_event_id:string ->
  ?max_event:int ->
  _ Middleware.t ->
  string ->
  (event Seq.t, Middleware.response) result

type subscription

val subscribe :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.Mono.t ->
  ?headers:Header.headers ->
  ?last_event_id:string ->
  ?max_event:int ->
  ?backoff_initial:Duration.t ->
  ?backoff_max:Duration.t ->
  ?capacity:int ->
  ?retryable:(exn -> bool) ->
  _ Middleware.t ->
  string ->
  subscription

val events : subscription -> [ `Event of event | `End ] Eio.Stream.t

val last_event_id : subscription -> string option

val result : subscription -> (unit, exn) result Eio.Promise.t

val close : subscription -> unit

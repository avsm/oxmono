type t

val with_session :
  clock:_ Eio.Time.Mono.t ->
  room:string ->
  event:string ->
  set:(bool -> unit) ->
  (t -> 'a) ->
  'a
(** [with_session ~clock ~room ~event ~set f] scopes typing notifications to
    [f]. [set] changes typing for this room only. Requests have a five-second
    deadline and failures are logged without server text. Once started, typing
    refreshes every 15 seconds. Exit clears typing even on cancellation. *)

val start : t -> unit
(** [start t] starts typing after request acceptance and waits for the first
    notification attempt. Repeated starts do nothing. *)

val stop : t -> unit
(** [stop t] cancels and joins the refresh fiber before clearing typing. Call
    before delivery. Repeated stops do nothing. *)

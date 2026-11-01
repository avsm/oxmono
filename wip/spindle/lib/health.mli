type t
(** Observer transport, backlog and recovery readiness. *)

val v : store:Store.t -> enabled:bool -> t
val require : t -> string -> unit
val remove : t -> string -> unit
val starting : t -> string -> now:float -> unit
val connected : t -> string -> now:float -> unit
val activity : t -> string -> now:float -> unit
val event : t -> string -> at:float -> now:float -> unit
val failed : t -> string -> now:float -> exn -> unit

val report : t -> now:float -> bool * Jsont.json
(** [report health ~now] returns readiness and diagnostics. Quiet streams stay
    healthy through control-frame traffic. Queued work, pending discovery,
    recovery and disconnected sources determine readiness. *)

(** Readiness of the spindle's event sources and maintenance.

    A source is named, such as ["jetstream"], and reported by the observer as it
    connects, receives traffic and fails. Its state is also kept in the store,
    written at most every 30 seconds. *)

type t

val v : store:Store.t -> enabled:bool -> t
(** [v ~store ~enabled] reports on sources only when [enabled], as when the
    spindle follows Jetstream. Otherwise readiness rests on maintenance. *)

val require : t -> string -> unit
(** [require t name] makes source [name] count towards readiness. *)

val starting : t -> string -> now:float -> unit
(** [starting t name ~now] records that [name] is connecting. *)

val connected : t -> string -> now:float -> unit
(** [connected t name ~now] records that [name] has connected. *)

val activity : t -> string -> now:float -> unit
(** [activity t name ~now] records traffic on [name], control frames included,
    so a quiet stream stays healthy. *)

val event : t -> string -> at:float -> now:float -> unit
(** [event t name ~at ~now] records an event of [name] that happened at [at],
    whose age the report gives. *)

val failed : t -> string -> now:float -> exn -> unit
(** [failed t name ~now exn] records that [name] dropped with [exn], whose text
    is kept to 2 KiB. *)

val report : t -> now:float -> bool * Jsont.json
(** [report t ~now] is whether the spindle is ready, and the report it serves at
    [/] and [/readyz]. It is ready when the last maintenance pass succeeded and,
    with sources enabled, the ["jetstream"] source is required, every source is
    connected with traffic in the last two minutes, and the oldest queued event
    is under a minute old. *)

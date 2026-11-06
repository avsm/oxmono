(** Assertions shared by crowthebot tests. *)

val check : string -> bool -> unit
(** [check name ok] does nothing when [ok] holds and raises [Failure name]
    otherwise. *)

val contains : string -> string -> bool
(** [contains text part] is [true] when [part] occurs in [text]. *)

val json : string -> Jsont.json
(** [json text] is the generic JSON value of [text]. The caller must pass valid
    JSON. *)

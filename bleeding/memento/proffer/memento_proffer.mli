@@ portable

(** Proffer helpers for Memento archives and versioned resources. *)
val accept_datetime :
  Proffer.Req.t @ local -> (Memento.Datetime.t option, string) result
(** [accept_datetime req] reads the requested archival datetime. Absence is
    [Ok None], malformed or repeated input is [Error]. *)
val memento_headers :
  original:string -> Memento.capture -> Proffer.Headers.t
(** [memento_headers ~original c] gives required fields for serving [c].
    Pass these to a Proffer response carrying the archived representation. *)
val timegate :
  Proffer.Resp.respond @ local -> original:string -> Memento.capture -> unit
(** [timegate respond ~original c] sends 302 with Location, Vary and required
    links. Select [c] with {!Memento.nearest} or your archive's own policy. *)
val json_timemap :
  Proffer.Resp.respond @ local -> Memento.Timemap.t -> unit
(** [json_timemap respond t] validates and serves [t] as application/json. *)
val link_timemap :
  Proffer.Resp.respond @ local -> Memento.Link.t list -> unit
(** [link_timemap respond links] serves RFC 7089 application/link-format.
    Requires exactly one original link and valid datetime attributes on every
    memento link. Invalid input raises Invalid_argument before responding. *)

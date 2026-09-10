val src : Logs.src
(** [src] is Crow's metadata-only diagnostic log source. *)

module Log : Logs.LOG

module Tools : Logs.LOG
(** Tool activity, with identifiers and counts but no payloads. *)

val configure : verbose:bool -> unit
(** [configure ~verbose] enables tool activity at info level. [verbose] also
    enables Crow runtime info logs. Other sources remain at warning level. *)

val enabled : unit -> bool
(** [enabled ()] is whether Crow's info logs are enabled. *)

val matrix_error : Matrix_eio.Error.err -> string
(** [matrix_error error] describes [error] without server text or credentials.
*)

val error : exn -> string
(** [error exn] classifies [exn] without exception text or response bodies. *)

val sent : Matrix_bot.Sent.outcome -> string
(** [sent outcome] describes [outcome] without message or server content. *)

val sync : Matrix_ui.Runtime.sync_state -> string
(** [sync state] describes [state] without sync tokens or server text. *)

val timeline :
  self:Matrix_proto.Id.User_id.t ->
  Matrix_ui.Event_cache.event array ->
  int * int * int
(** [timeline ~self events] counts cached events, undecryptable incoming events
    and malformed incoming events in [events]. *)

val watch_room :
  sw:Eio.Switch.t ->
  self:Matrix_proto.Id.User_id.t ->
  cache:Matrix_ui.Event_cache.t ->
  room:Matrix_proto.Id.Room_id.t ->
  unit ->
  unit
(** [watch_room ~sw ~self ~cache ~room] logs changes to the room's cached event
    counts until [sw] closes. The returned function stops the subscription. *)

exception Model_output_limit

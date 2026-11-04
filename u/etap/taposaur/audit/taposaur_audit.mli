(** Append-only operational audit, separate from authoritative domain logs. *)
type read
type write
type 'a t
type session
type phase = Started | Succeeded | Failed | Cancelled | Notice
val phase_name : phase -> string
val phase_of_string : string -> phase
val open_read : sw:Eio.Switch.t -> _ Eio.Path.t -> read t
val open_write : sw:Eio.Switch.t -> _ Eio.Path.t -> write t
val session : ?actor:string -> component:string -> write t -> session
val store : session -> write t
(** [store s] is the database [s] appends to, for correlated queries. *)
val run_id : session -> string
val actor : session -> string
val record : session -> action:string -> subject:string ->
  ?details:(string * string) list -> unit -> unit
(** Details are bounded, caller-selected JSON fields. Do not supply raw
    arguments, URLs, headers, credentials, document bodies or exception text. *)
val with_operation : ?session:session -> ?actor:string ->
  ?details:(string * string) list -> ?success:('a -> bool) ->
  ?result_details:('a -> (string * string) list) ->
  action:string -> subject:string ->
  (session option -> 'a) -> 'a
(** A durable start precedes [f]. Successful completion is logged afterward.
    Failure/cancellation records do not imply rollback. A missing terminal
    event means the result is unknown and must be reconciled with domain state.
    Start-write failure prevents [f]. Terminal-write failure after success
    raises [Completion_unrecorded]. With no session this calls [f None]. *)
exception Completion_unrecorded of string

type event = {
  version : int; id : int64; time : float; run : string; operation : string;
  parent : string option; actor : string; component : string;
  action : string; subject : string; phase : phase; details : string;
}
val query : ?actor:string -> ?action:string -> ?subject:string ->
  ?run:string -> ?operation:string -> ?phase:phase -> ?incomplete:bool -> ?before:int64 ->
  ?limit:int -> 'a t -> event list
(** Indexed exact filters, newest first, exclusive ID cursor. Limit 1..500.
    [incomplete] selects starts with no terminal event, including live work. *)
val to_json : event -> string

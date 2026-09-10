(** Read-only calendar tools backed by a persistent SQLite mirror. *)

type t

val create :
  state:Caldav_store.t ->
  sources:(string * Caldav_source.t) list ->
  default:string option ->
  t

type access

val for_request : t -> actor:string -> room:string -> event:string -> access
val names : string list
val is_tool : string -> bool
val tools : Openrouter.Tool.t list
val system_prompt : string

val context : t -> actor:string -> string
(** [context t ~actor] supplies authorized mirror IDs and calendar time-zone
    identifiers from SQLite without remote requests. *)

val invoke : access -> string -> string -> (string, string) result

val poll : t -> actor:string -> int -> (unit, string) result
(** [poll t ~actor id] commits one bounded page of one collection. Unfinished
    scans schedule another pass. *)

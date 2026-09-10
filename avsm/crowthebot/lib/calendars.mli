(** Read-only calendar tools backed by a persistent SQLite mirror. *)

type t

val create :
  state:Calendar_store.t ->
  sources:(string * Calendar_source.t) list ->
  default:string option ->
  t

type access

val for_request : t -> actor:string -> room:string -> event:string -> access
val names : string list
val is_tool : string -> bool
val tools : Openrouter.Tool.t list
val system_prompt : string
val invoke : access -> string -> string -> (string, string) result

val poll : t -> actor:string -> int -> (unit, string) result
(** [poll t ~actor id] commits one bounded page per resource and downloads up to
    two referenced blobs. Unfinished scans schedule another pass. *)

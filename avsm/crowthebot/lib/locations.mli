type t

val configuration : Tool_config.t
(** [configuration] contributes named Recorder setup to the local CLI. *)

val create :
  state:Location_store.t ->
  sources:(string * Owntracks_source.t) list ->
  default:string option ->
  t

type access

val for_request : t -> actor:string -> room:string -> event:string -> access
val tools : Openrouter.Tool.t list
val is_tool : string -> bool
val system_prompt : string
val invoke : access -> string -> string -> (string, string) result
val help : string
val command : string -> (string * string, string) result

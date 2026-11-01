type t

val configuration : Tool_config.t
(** [configuration] contributes named Recorder setup to the local CLI. *)

val create :
  ?fresh_wait:float ->
  state:Location_store.t ->
  sources:(string * Owntracks_source.t) list ->
  default:string option ->
  unit ->
  t
(** [create ~state ~sources ~default ()] serves the location tools.
    [location_get] with [fresh] waits up to [fresh_wait] seconds, 30 by
    default, for the phone to answer a request. *)

type access

val for_request : t -> actor:string -> room:string -> event:string -> access
val tools : Agentkit.Agent.Tool.t list
val is_tool : string -> bool
val system_prompt : string
val invoke : access -> string -> string -> (string, string) result
val help : string
val command : string -> (string * string, string) result

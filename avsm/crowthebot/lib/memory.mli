(** Shared profile memory for the admin and allowed friends. *)

val names : string list
val is_tool : string -> bool
val tools : Openrouter.Tool.t list
val system_prompt : string
val help : string
val fact_line : Store.fact -> string

type access

val for_request :
  Store.t ->
  actor:string ->
  room:string ->
  event:string ->
  source:string ->
  access
(** [for_request store ...] binds four memory operations to an authenticated
    actor and source. The tool receives no general database capability. *)

val invoke : access -> string -> string -> (string, string) result
(** [invoke access name arguments] executes one authorized memory operation. The
    caller logs the operation and bounds its rendered result. *)

val command : string -> (string * string, string) result
(** [command text] translates a memory command to a tool name and JSON
    arguments. *)

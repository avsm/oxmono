(** Email reads and queries, with a separate mailbox-label write tool. *)

type t

val create :
  state:Email_cache.t ->
  readers:(string * Email_source.reader) list ->
  writers:(string * Email_source.writer) list ->
  default_reader:string option ->
  default_writer:string option ->
  t

type access

val for_request : t -> actor:string -> room:string -> event:string -> access
val names : string list
val is_tool : string -> bool

val tools : t -> Openrouter.Tool.t list
(** [tools t] advertises reads only with RO connections and label updates only
    with RW connections. Credentials are never tool arguments or results. *)

val system_prompt : string
val invoke : access -> string -> string -> (string, string) result

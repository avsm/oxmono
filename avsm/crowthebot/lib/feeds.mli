type t
(** RSS, Atom and OPML subscriptions with typed state and cron polling. *)

val create : state:Feed_store.t -> download:Feed_http.t -> t

type access

val for_request : t -> actor:string -> room:string -> event:string -> access
val names : string list
val is_tool : string -> bool
val tools : Openrouter.Tool.t list
val system_prompt : string
val help : string
val invoke : access -> string -> string -> (string, string) result
val command : string -> (string * string, string) result
val poll_member : t -> actor:string -> int -> unit

type update = { context : string; acknowledge : unit -> unit }

val prepare : t -> actor:string -> member_id:int -> update option
(** [prepare t ~actor ~member_id] polls and returns pending entries for a
    scheduled model action. A quiet poll needs no model request or message. Call
    [acknowledge] only after delivery has been confirmed. *)

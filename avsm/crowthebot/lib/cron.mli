(** UTC schedules and reminder tools with no filesystem or network capability.
*)

val time : string -> float

type schedule

val parse : string -> schedule

val next : schedule -> after:float -> until:float option -> float option
(** [next schedule ~after ~until] is the first matching minute strictly after
    [after], within the optional inclusive end time. It searches eight years.
    Restricted day-of-month and weekday fields use cron's OR rule. *)

type access

val for_request :
  Store.t -> actor:string -> room:string -> event:string -> access
(** [for_request store ...] binds reminder operations to an authenticated
    account and source. The tool receives only these three operations. *)

val names : string list
val is_tool : string -> bool
val tools : Openrouter.Tool.t list
val system_prompt : string
val help : string
val line : Store.reminder -> string
val invoke : access -> string -> string -> (string, string) result
val command : string -> (string * string, string) result

val run_due :
  ?ready:(unit -> bool) ->
  Store.t ->
  fire:(Store.reminder -> run_id:int -> string) ->
  unit
(** [run_due ?ready store ~fire] claims up to 20 due occurrences before effects.
    [ready ()] is checked before each claim, so startup or disconnected
    transports can defer work without consuming an occurrence. Recurrences skip
    missed intervals and advance from now. Expired jobs do not fire. A crash may
    lose a claimed occurrence but cannot replay it. [fire] must recheck current
    authority, linked memory and delivery access. *)

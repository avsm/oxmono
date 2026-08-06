(* Re-declaring the primitive with a portable mode is the bridge until
   [Effect.Safe] lands. *)

external perform : 'a Effect.t -> 'a @@ portable = "%perform"

type _ Effect.t += Ping : unit Effect.t

let f : (unit -> unit) @ portable = fun () -> perform Ping

let () = ignore f

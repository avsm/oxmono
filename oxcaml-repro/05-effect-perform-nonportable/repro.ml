(* [Stdlib.Effect] carries no mode annotations in 5.2.0+ox, so
   [Effect.perform] is nonportable and no effect-based scheduler code
   can be marked portable. Performing an effect is handled by the
   current domain's own handler stack, so it cannot share state across
   domains. The oxcaml development compiler addresses this with
   [Effect.Safe.perform] and a [Handler.t @ local] capability token. *)

type _ Effect.t += Ping : unit Effect.t

let f : (unit -> unit) @ portable = fun () -> Effect.perform Ping

let () = ignore f

(* Portable re-declaration of effect primitives.

   [Stdlib.Effect] predates the mode system and its operations are
   nonportable. Performing an effect is handled by the performing
   domain's own scheduler, so it cannot share state across domains.
   Re-declaring the primitive with a [portable] mode lets the fiber
   core be annotated. *)

external perform : 'a Effect.t -> 'a @@ portable = "%perform"

(* [Domain.DLS] reads and writes only the calling domain's slots, so it
   cannot race. The development compiler types this as [Domain.Safe.DLS],
   restricted to values that cross portability and contention. [int]
   crosses both, so these assertions anticipate that interface. *)

let dls_get_int : int Domain.DLS.key -> int =
  Obj.magic_portable (Domain.DLS.get : int Domain.DLS.key -> int)

let dls_set_int : int Domain.DLS.key -> int -> unit =
  Obj.magic_portable (Domain.DLS.set : int Domain.DLS.key -> int -> unit)

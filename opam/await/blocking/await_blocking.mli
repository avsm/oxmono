@@ portable

open Await_kernel

(** [await terminator] is an implementation of blocking that blocks the current OS thread. *)
val await : Terminator.t -> Await.t

@@ portable

open Await_kernel

(** [with_await terminator ~f] runs [f t] such that
    [Await.await t ~on_terminate ~await_on] will block the domain by spinning until the
    trigger to [await_on] is signalled, and [Await.yield t] does nothing.

    This is a simple spinning implementation of [Await.t] suitable for testing and
    benchmarking purposes. This should not be used outside of said use cases. *)
val with_await : Terminator.t @ local -> f:(Await.t @ local -> 'a) @ local once -> 'a

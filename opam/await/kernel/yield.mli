@@ portable

(** A value of type [t] allows cooperatively yielding to the scheduler. Operations that
    run long computations and might want to yield to a scheduler take a [t] that provides
    an implementation of yielding for them to use.

    Internally, a {!Yield.t} is just an {!Await.t}, but code which only yields and does
    not block can take a {!Yield.t} as a parameter to promise that it will not block. *)
type t : value mod contended portable

(** [of_await await] is an implementation of yielding based on [await] *)
val of_await : Await.t @ local -> t @ local

(** [yield t] uses [t] to yield to the scheduler *)
val yield : t @ local -> unit

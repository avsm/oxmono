open Base
open Await_kernel
open Capsule_blocking_sync [@@alert "-deprecated"]

module Futex = struct
  (** A handle to a linux futex for waiting on a trigger. *)
  type t : immediate

  type count : immediate

  (** Returns a futex for waiting on a trigger. A new futex may or may not be returned
      each time this is called. *)
  external get : unit -> t @@ portable = "await_blocking_futex_get"
  [@@noalloc]

  (** Returns the current count of the futex. *)
  external count : t -> count @@ portable = "await_blocking_futex_count"
  [@@noalloc]

  (** Increments the count of the futex and makes sure that any call to [wait] on the same
      futex will check whether the associated trigger has been signaled before suspending
      the thread of control. *)
  external signal : t -> unit @@ portable = "await_blocking_futex_signal"
  [@@noalloc]

  (** Wait until the count of the futex has changed using the futex to suspend the thread
      until the futex is [signal]ed and return the current count. *)
  external wait : t -> count:count -> count @@ portable = "await_blocking_futex_wait"
end

let await () trigger =
  let futex = Futex.get () in
  match Trigger.on_signal trigger ~f:[%eta1 Futex.signal] futex with
  | Null ->
    let[@inline] rec loop count =
      if not (Trigger.is_signalled trigger)
      then
        (* We might spuriously wakeup even if [Futex.signal] has not been called, so we
           need to loop around again to check. *)
        loop (Futex.wait futex ~count)
    in
    loop (Futex.count futex)
  | This _ ->
    (* One way we might get here is if:

       1. we decide we want to wait on a trigger
       2. some other thread signals the trigger
       3. we enter [await], [get] the futex, then try to register the action for the
          trigger.
    *)
    ()
;;

let yield _ = yield ()
let await terminator = Await.create_global ~terminator ~await ~yield:(This yield) ()

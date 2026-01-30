open Base
open Await_kernel

type 'k t = bool Awaitable.t

let create ?padded () = Awaitable.make ?padded true

let[@inline] wait ~acquire ~release w t ~lock _k =
  let trigger = Trigger.create () in
  let awaiter =
    (Awaitable.Awaiter.(create_and_add [@mode local]) t)
      (Trigger.source trigger)
      ~until_phys_unequal_to:false
  in
  release lock;
  Await.await_until_terminated w trigger;
  Awaitable.Awaiter.cancel_and_remove awaiter;
  acquire (Await.with_terminator w Terminator.never) lock;
  if Await.is_terminated w
  then (
    match raise Await.Terminated with
    | (_ : Nothing.t) -> .)
  else Capsule.Expert.Key.unsafe_mk ()
;;

let signal = Awaitable.signal
let broadcast = Awaitable.broadcast

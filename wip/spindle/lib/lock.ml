(* SPDX-License-Identifier: ISC *)

(* An exception, cancellation included, is caught inside the section, so the
   mutex is released rather than poisoned. [Eio.Condition.await] locks the
   mutex again before it raises. *)
let protect ?(cancellable = false) mutex f =
  let result =
    Eio.Mutex.use_rw ~protect:(not cancellable) mutex (fun () ->
        try Ok (f ()) with exn -> Error (exn, Printexc.get_raw_backtrace ()))
  in
  match result with
  | Ok value -> value
  | Error (exn, backtrace) -> Printexc.raise_with_backtrace exn backtrace

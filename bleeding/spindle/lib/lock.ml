(* SPDX-License-Identifier: ISC *)
let protect mutex f =
  let result =
    Eio.Mutex.use_rw ~protect:true mutex (fun () ->
        try Ok (f ()) with exn -> Error (exn, Printexc.get_raw_backtrace ()))
  in
  match result with
  | Ok value -> value
  | Error (exn, backtrace) -> Printexc.raise_with_backtrace exn backtrace

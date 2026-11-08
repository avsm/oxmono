(* SPDX-License-Identifier: ISC *)

(* [scan.exe DIR] prints what brief finds in [DIR]. *)
let () =
  let dir = if Array.length Sys.argv > 1 then Sys.argv.(1) else "." in
  Eio_main.run @@ fun env ->
  match Brief.scan ~process:env#process_mgr ~clock:env#mono_clock dir with
  | Ok t -> print_string (Brief.summary t)
  | Error e ->
      prerr_endline e;
      exit 1

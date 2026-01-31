(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Eio process execution helpers with logging *)

module Log = (val Logs.src_log (Logs.Src.create "oxmono.process") : Logs.LOG)

type run_result = (unit, [`Exit_code of int | `Signal of int | `Exn of exn]) result

let run ~env ?cwd args : run_result =
  Eio.Switch.run @@ fun sw ->
  let mgr = Eio.Stdenv.process_mgr env in
  Log.debug (fun m -> m "Running: %s" (String.concat " " args));
  try
    let proc = match cwd with
      | Some dir -> Eio.Process.spawn ~sw ~cwd:dir mgr args
      | None -> Eio.Process.spawn ~sw mgr args
    in
    match Eio.Process.await proc with
    | `Exited 0 -> Ok ()
    | `Exited n -> Error (`Exit_code n)
    | `Signaled n -> Error (`Signal n)
  with exn ->
    Error (`Exn exn)

let run_with_output ~env ?cwd args : (string, [`Exit_code of int | `Signal of int | `Exn of exn]) result =
  Eio.Switch.run @@ fun sw ->
  let mgr = Eio.Stdenv.process_mgr env in
  Log.debug (fun m -> m "Running (with output): %s" (String.concat " " args));
  try
    let stdout_r, stdout_w = Eio.Process.pipe ~sw mgr in
    let proc = match cwd with
      | Some dir -> Eio.Process.spawn ~sw ~cwd:dir ~stdout:stdout_w mgr args
      | None -> Eio.Process.spawn ~sw ~stdout:stdout_w mgr args
    in
    Eio.Flow.close stdout_w;
    let output = Eio.Buf_read.of_flow ~max_size:max_int stdout_r |> Eio.Buf_read.take_all in
    match Eio.Process.await proc with
    | `Exited 0 -> Ok output
    | `Exited n -> Error (`Exit_code n)
    | `Signaled n -> Error (`Signal n)
  with exn ->
    Error (`Exn exn)

let run_exn ~env ?cwd args =
  match run ~env ?cwd args with
  | Ok () -> ()
  | Error (`Exit_code n) ->
    failwith (Printf.sprintf "Command failed with exit code %d: %s" n (String.concat " " args))
  | Error (`Signal n) ->
    failwith (Printf.sprintf "Command killed by signal %d: %s" n (String.concat " " args))
  | Error (`Exn exn) ->
    failwith (Printf.sprintf "Command failed with exception: %s" (Printexc.to_string exn))

let error_to_string args = function
  | `Exit_code n -> Printf.sprintf "%s: exit code %d" (String.concat " " args) n
  | `Signal n -> Printf.sprintf "%s: killed by signal %d" (String.concat " " args) n
  | `Exn exn -> Printf.sprintf "%s: %s" (String.concat " " args) (Printexc.to_string exn)

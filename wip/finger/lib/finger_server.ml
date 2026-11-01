(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let src = Logs.Src.create "finger.server" ~doc:"Finger protocol server"
module Log = (val Logs.src_log src : Logs.LOG)

let default_port = 79

let run ~sw ~net ?(port = default_port) ~handler () =
  let addr = `Tcp (Eio.Net.Ipaddr.V4.any, port) in
  let socket = Eio.Net.listen net ~sw ~backlog:16 ~reuse_addr:true addr in
  Log.info (fun m -> m "Finger server listening on port %d" port);
  let on_error exn =
    Log.err (fun m -> m "Finger connection error: %s" (Printexc.to_string exn))
  in
  Eio.Net.run_server socket ~on_error (fun flow _addr ->
    let buf = Eio.Buf_read.of_flow flow ~max_size:1024 in
    let line =
      try Eio.Buf_read.line buf
      with End_of_file -> ""
    in
    Log.info (fun m -> m "Finger query: %S" line);
    let query = Finger_query.parse line in
    let response = handler query in
    Eio.Flow.copy_string response flow;
    Eio.Flow.shutdown flow `Send)

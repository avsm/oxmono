(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let src = Logs.Src.create "finger.client" ~doc:"Finger protocol client"
module Log = (val Logs.src_log src : Logs.LOG)

let query ~net ~host ?(port = Finger_server.default_port) line =
  Eio.Switch.run @@ fun sw ->
  let addrs = Eio.Net.getaddrinfo_stream net host
      ~service:(string_of_int port) in
  match addrs with
  | [] -> failwith (Printf.sprintf "Could not resolve host: %s" host)
  | addr :: _ ->
    let flow = Eio.Net.connect ~sw net addr in
    Log.info (fun m -> m "Connected to %s:%d" host port);
    Eio.Flow.copy_string (line ^ "\r\n") flow;
    Eio.Flow.shutdown flow `Send;
    let buf = Eio.Buf_read.of_flow flow ~max_size:(1024 * 1024) in
    Eio.Buf_read.take_all buf

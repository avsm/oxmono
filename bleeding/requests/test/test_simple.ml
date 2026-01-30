(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Simple test to debug connection issues *)

open Eio.Std

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug);
  Logs.Src.set_level Conpool.src (Some Logs.Debug);

  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->

  traceln "Starting simple server on 127.0.0.1:9000";

  (* Start a simple echo server *)
  let raw_bytes = Bytes.create 4 in
  Bytes.set raw_bytes 0 (Char.chr 127);
  Bytes.set raw_bytes 1 (Char.chr 0);
  Bytes.set raw_bytes 2 (Char.chr 0);
  Bytes.set raw_bytes 3 (Char.chr 1);
  let ipaddr = Eio.Net.Ipaddr.of_raw (Bytes.to_string raw_bytes) in

  let socket = Eio.Net.listen env#net ~sw ~reuse_addr:true ~backlog:10
    (`Tcp (ipaddr, 9000))
  in

  Eio.Fiber.fork ~sw (fun () ->
    Eio.Net.accept_fork socket ~sw ~on_error:raise (fun flow _addr ->
      traceln "Server: accepted connection";
      let buf = Eio.Buf_read.of_flow flow ~max_size:1024 in
      let line = Eio.Buf_read.line buf in
      traceln "Server: received: %s" line;
      Eio.Flow.copy_string (line ^ "\n") flow;
      Eio.Flow.close flow
    )
  );

  Eio.Time.sleep env#clock 0.1;

  traceln "Creating connection pool";
  let pool = Conpool.create_basic ~sw ~net:env#net ~clock:env#clock () in

  traceln "Testing connection";
  let endpoint = Conpool.Endpoint.make ~host:"127.0.0.1" ~port:9000 in

  let response = Conpool.with_connection pool endpoint (fun conn ->
    traceln "Client: sending message";
    Eio.Flow.copy_string "test message\n" conn.Conpool.flow;
    let buf = Eio.Buf_read.of_flow conn.Conpool.flow ~max_size:1024 in
    let resp = Eio.Buf_read.line buf in
    traceln "Client: received: %s" resp;
    resp
  ) in

  traceln "Response: %s" response;
  traceln "Test passed!"

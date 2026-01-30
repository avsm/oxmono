(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** httpz + Eio server adapter for arod routes *)

open Base

let src = Logs.Src.create "arod.server" ~doc:"Arod server adapter"

module Log = (val Logs.src_log src : Logs.LOG)

(** {1 Public API} *)

let run ~sw ~net ~config routes =
  let addr =
    `Tcp (Eio.Net.Ipaddr.V4.any, config.Arod.Config.server.port)
  in
  let socket = Eio.Net.listen net ~sw ~backlog:128 ~reuse_addr:true addr in
  Log.app (fun m ->
      m "Listening on http://%s:%d" config.server.host config.server.port);
  let on_request ~meth ~path ~status =
    Log.info (fun m ->
        m "%s %s - %s"
          (Httpz.Method.to_string meth)
          path
          (Httpz.Res.status_to_string status))
  in
  let on_error exn =
    Log.err (fun m -> m "Connection error: %s" (Exn.to_string exn))
  in
  while true do
    Eio.Net.accept_fork socket ~sw
      ~on_error:(fun exn ->
        Log.err (fun m -> m "Accept error: %s" (Exn.to_string exn)))
      (fun flow addr ->
        Httpz_eio.handle_client ~routes ~on_request ~on_error flow addr)
  done

(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** httpz + Eio server adapter for arod routes *)

open Base

let src = Logs.Src.create "arod.server" ~doc:"Arod server adapter"

module Log = (val Logs.src_log src : Logs.LOG)

(** {1 Public API} *)

let run ~sw:_ ~net ~config ~log routes =
  let addr =
    `Tcp (Eio.Net.Ipaddr.V4.any, config.Arod.Config.server.port)
  in
  Eio.Switch.run @@ fun sw ->
  let socket = Eio.Net.listen net ~sw ~backlog:128 ~reuse_addr:true addr in
  Log.app (fun m ->
      m "Listening on http://%s:%d" config.server.host config.server.port);
  let on_request (local_ info : Httpz_eio.request_info) =
    (* Extract values before Logs closure, which captures globally *)
    let meth_s = Httpz.Method.to_string info.meth in
    let path_s = Arod_log.globalize info.path in
    let status_s = Httpz.Res.status_to_string info.status in
    let dur = info.duration_us in
    let cache_s = match info.cache_status with
      | This s -> " " ^ Arod_log.globalize s | Null -> "" in
    Arod_log.log_request log info;
    Log.info (fun m -> m "%s %s - %s (%dus%s)" meth_s path_s status_s dur cache_s)
  in
  let on_error exn =
    Log.err (fun m -> m "Connection error: %s" (Exn.to_string exn))
  in
  Eio.Net.run_server socket ~on_error (fun flow addr ->
    Httpz_eio.handle_client ~routes ~on_request ~on_error flow addr)

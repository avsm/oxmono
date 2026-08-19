(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Serving the arod site over proffer-httpz. *)

module Site = Arod_site
open Base

let src = Logs.Src.create "arod.server" ~doc:"Arod server adapter"

module Log = (val Logs.src_log src : Logs.LOG)

let run ~sw ~net ~clock ~(config : Arod.Config.t) ~log ~env compiled =
  let addr = `Tcp (Eio.Net.Ipaddr.V4.any, config.server.port) in
  let on_listening _ =
    Log.app (fun m ->
        m "Listening on http://%s:%d" config.server.host config.server.port)
  in
  let on_event (event : Proffer_httpz.event) =
    (* Serving is single-domain, so the insert happens on the domain that
       owns the log database. Serving from several domains would need this
       to hand the event to a fiber on that domain through a queue. *)
    Arod_log.log_request log ~timestamp:(Eio.Time.now clock) event;
    let cache = match event.cache_status with Some s -> " " ^ s | None -> "" in
    Log.info (fun m ->
        m "%s %s - %d (%dus%s)"
          (Proffer.Method.to_string event.meth)
          event.path
          (Proffer.Status.code event.status)
          event.duration_us cache)
  in
  let on_error exn =
    Log.err (fun m -> m "Server error: %s" (Exn.to_string exn))
  in
  Proffer_httpz.run ~sw ~net ~clock ~addr ~on_listening ~on_event ~on_error
    ~env compiled

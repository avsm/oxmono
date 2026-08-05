(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Internal connection representation - not exposed in public API *)

let src =
  Logs.Src.create "conpool.connection"
    ~doc:"Connection pool internal connection management"

module Log = (val Logs.src_log src : Logs.LOG)

type t = {
  flow : [Eio.Resource.close_ty | Eio.Flow.two_way_ty] Eio.Resource.t;
  tls_flow : Tls_eio.t option;
  created_at : float;
  mutable last_used : float;
  mutable use_count : int;
  endpoint : Endpoint.t;
  mutex : Eio.Mutex.t;
}

let flow t = t.flow
let tls_flow t = t.tls_flow
let endpoint t = t.endpoint
let created_at t = t.created_at
let last_used t = t.last_used
let use_count t = t.use_count

let update_usage t ~now =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      t.last_used <- now;
      t.use_count <- t.use_count + 1)

let pp ppf t =
  let uses = t.use_count in
  Fmt.pf ppf "Connection(endpoint=%a, created_at=%.2f, uses=%d)" Endpoint.pp
    t.endpoint t.created_at uses

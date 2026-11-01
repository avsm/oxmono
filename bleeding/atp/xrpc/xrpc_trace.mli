(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Logging of the HTTP exchanges that an AT Protocol client makes.

    {!wrap} logs each exchange on {!src}: at [Info] the method, the target
    and the status with the time to the response's head, and at [Debug] also
    the request and response fields. Secret headers and query parameters are
    redacted, as {!Fetch.Middleware.pp_request} does. Nothing is logged
    unless the source's level lets it through. Each message carries the
    {!tags} below, so a reporter can lay it out without parsing the text. *)

val src : Logs.src
(** [src] is the source ["xrpc.http"]. *)

val wrap : _ Fetch.t -> Fetch.plain
(** [wrap http] is [http] with each exchange logged on {!src}. *)

val exchange :
  meth:string ->
  host:string ->
  target:string ->
  ?status:int ->
  elapsed:float ->
  unit ->
  unit
(** [exchange ~meth ~host ~target ?status ~elapsed ()] logs an exchange made
    outside {!Fetch}, such as a WebSocket upgrade, as {!wrap} logs its own.
    [target] must hold no secret. *)

(** {1:tags Tags} *)

val meth : string Logs.Tag.def
(** [meth] is the request method, such as ["GET"]. *)

val host : string Logs.Tag.def
(** [host] is the host of the target. *)

val nsid : string Logs.Tag.def
(** [nsid] is the XRPC method of a target under [/xrpc/]. *)

val target : string Logs.Tag.def
(** [target] is the path and query of the target, redacted. *)

val status : int Logs.Tag.def
(** [status] is the response status. A failed exchange has none. *)

val elapsed : float Logs.Tag.def
(** [elapsed] is the seconds until the response's head arrived, or until the
    exchange failed. *)

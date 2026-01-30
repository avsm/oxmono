(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** httpz + Eio server adapter for arod routes

    This module adapts the framework-agnostic {!Arod.Route} abstraction to
    work with httpz for HTTP parsing and Eio for async I/O. It provides
    a zero-allocation request/response path using CPS-style handlers
    that write responses directly. *)

val run :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  config:Arod.Config.t ->
  Httpz.Route.t ->
  unit
(** [run ~sw ~net ~config routes] starts the httpz + Eio server with
    the given routes.

    @param sw Eio switch for managing server lifecycle.
    @param net Eio network for creating sockets.
    @param config Server configuration with host, port, and paths.
    @raise exn on server errors (Eio propagates exceptions) *)

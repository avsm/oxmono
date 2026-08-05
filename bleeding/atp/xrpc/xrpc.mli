(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** XRPC client library for AT Protocol.

    This library provides an XRPC client for communicating with AT Protocol
    Personal Data Servers (PDS). It uses the [requests] library for HTTP and
    [jsont] for JSON encoding/decoding.

    {2 Overview}

    XRPC (eXtensible RPC) is AT Protocol's remote procedure call mechanism.
    Methods are identified by namespace identifiers (NSIDs) like
    ["app.bsky.feed.getTimeline"].

    The library provides:
    - {!Client}: Low-level XRPC operations (query, procedure, blob upload)
    - {!Credential}: Session management with automatic token refresh
    - {!module:Error}: Structured error types wrapped as Eio exceptions
    - {!Jwt}: JWT payload decoding for token expiry checking
    - {!Types}: Core data types (session, error payload, etc.)

    {2 Quick Start}

    {[
      open Eio_main

      let () =
        run @@ fun env ->
        Eio.Switch.run @@ fun sw ->
        (* Create credential manager *)
        let cred =
          Xrpc.Credential.create ~sw ~env ~service:"https://bsky.social" ()
        in

        (* Login *)
        let client =
          Xrpc.Credential.login cred ~identifier:"alice.bsky.social"
            ~password:"app-password" ()
        in

        (* Make authenticated request *)
        let profile =
          Xrpc.Client.query client ~nsid:"app.bsky.actor.getProfile"
            ~params:[ ("actor", "alice.bsky.social") ]
            ~decoder:profile_jsont
        in

        print_endline profile.display_name
    ]}

    {2 Error Handling}

    All errors are raised as [Eio.Io] exceptions with {!Error.E} for structured
    access:

    {[
      try
        let result = Xrpc.Client.query client ~nsid ~params ~decoder in
        (* ... *)
      with
      | Eio.Io (Xrpc.Error.E err, _) ->
          Printf.eprintf "XRPC error: %s\n" (Xrpc.Error.to_string err)
    ]}

    @see <https://atproto.com/specs/xrpc> AT Protocol XRPC Specification *)

(** {1 Modules} *)

module Error = Xrpc_error
(** Error types with Eio.Io integration. *)

module Jwt = Xrpc_jwt
(** JWT payload decoding for token management. *)

module Types = Xrpc_types
(** Core XRPC data types with jsont codecs. *)

module Client = Xrpc_client
(** Low-level XRPC client operations. *)

module Credential = Xrpc_cred
(** Credential manager with automatic token refresh. *)

(** {1 Type Aliases} *)

type client = Xrpc_client.t
(** Alias for [Xrpc_client.t]. *)

type session = Xrpc_types.session
(** Alias for [Xrpc_types.session]. *)

type error = Xrpc_error.error
(** Alias for [Xrpc_error.error]. *)

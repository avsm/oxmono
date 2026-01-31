(** Httpz Server - Server-side HTTP utilities.

    This module provides server-specific functionality built on top of
    the core {!Httpz} library. It includes:

    - {!Route}: Zero-allocation routing with type-safe path patterns

    For HTTP protocol parsing and serialization, see {!Httpz}.
    For Eio-based connection handling, see {!Httpz_eio}.

    {2 Quick Start}

    {[
      open Httpz_server

      let routes = Route.of_list [
        Route.get_ [] (fun _ctx respond ->
          Route.html respond "Welcome!");
        Route.get ("users" **> Route.seg) (fun user_id _ctx respond ->
          Route.html respond (Printf.sprintf "User: %s" user_id));
      ]
    ]}

    See {!Route} for comprehensive routing documentation. *)

module Route = Route
(** Zero-allocation HTTP routing with GADT pattern DSL.

    Provides:
    - Type-safe path patterns (segments, captures, wildcards)
    - Per-route header requirements
    - CPS-style handlers for zero-allocation response writing
    - Response helpers for common content types *)

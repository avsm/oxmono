(** Httpz Server - Server-side HTTP utilities.

    This module provides server-specific functionality built on top of
    the core {!Httpz} library. It includes:

    - {!Route}: Segment-based routing with type-safe patterns and trie dispatch

    For HTTP protocol parsing and serialization, see {!Httpz}.
    For Eio-based connection handling, see {!Httpz_eio}.

    {2 Quick Start}

    {[
      open Httpz_server.Route

      let routes = of_list [
        get_ [] (fun _ctx respond -> html respond "Welcome!");
        get ("users" / seg End) (fun (user_id, ()) _ctx respond ->
          html respond (Printf.sprintf "User: %s" user_id));
        get ("static" / tail) (fun path _ctx respond ->
          serve_file (String.concat "/" path) respond);
      ]
    ]}

    See {!Route} for comprehensive routing documentation. *)

module Route = Route
(** Segment-based HTTP routing.

    Provides:
    - Type-safe path patterns matching against segment lists
    - Per-route header requirements
    - Trie-based O(path_depth) dispatch
    - Response helpers for common content types *)

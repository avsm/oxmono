(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Operation context for I/O diagnostics. *)

val describe : ?url:string -> operation:string -> exn -> string
(** [describe ~url ~operation exn] is the diagnostic for [exn] with [operation]
    added to its existing context. [url] defaults to absent. Only its origin is
    included, excluding user information, path, query and fragment. The result
    is passed through [Httpz_media.sanitize_diagnostic], so a byte the peer
    chose cannot break the line it is written on. *)

val with_io : ?url:string -> operation:string -> (unit -> 'a) -> 'a
(** [with_io ~url ~operation f] is [f ()]. An I/O exception gains [operation]
    and the optional URL origin on the terms of {!describe}, preserving its
    backtrace. Other exceptions propagate unchanged. *)

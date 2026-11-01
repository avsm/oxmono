(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Finger protocol client
    ({{:https://www.rfc-editor.org/rfc/rfc1288}RFC 1288}).

    Connects to a Finger server, sends a query, and reads the full response. *)

val query :
  net:_ Eio.Net.t ->
  host:string ->
  ?port:int ->
  string ->
  string
(** [query ~net ~host ?port line] sends a Finger query and returns the
    response.

    Connects to [host]:[port] (default 79), sends [line] followed by CRLF,
    reads the complete response, and returns it.  The [line] should be the
    raw query text (e.g. ["anil"], ["/W anil"], or [""] for a null query);
    CRLF is appended automatically.

    Raises [Failure] if the hostname cannot be resolved. *)

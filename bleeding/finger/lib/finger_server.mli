(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Finger protocol server
    ({{:https://www.rfc-editor.org/rfc/rfc1288}RFC 1288}).

    Listens on a TCP port and dispatches incoming queries to a user-supplied
    handler function.  The handler receives a parsed {!Finger.Query.t} and
    returns a plain-text response string.

    The server handles protocol framing: reading the CRLF-terminated query
    line, invoking the handler, writing the response, and closing the
    connection. *)

val default_port : int
(** The IANA-assigned Finger port (79). *)

val run :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  ?port:int ->
  handler:(Finger_query.t -> string) ->
  unit -> unit
(** [run ~sw ~net ?port ~handler ()] starts a Finger server.

    The server listens on [port] (default {!default_port}) using the switch
    [sw] for the listening socket lifetime.  For each connection it reads a
    single query line, parses it with {!Finger.Query.parse}, calls [handler],
    writes the returned string, and closes the connection.

    A new fiber is spawned per connection via {!Eio.Net.run_server}.  The
    call blocks until [sw] is cancelled. *)

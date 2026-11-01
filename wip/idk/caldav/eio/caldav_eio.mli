(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** A CalDAV client on Eio.

    {!Quirks} is what a server does differently and {!Client} the connection to
    a server, on {!Fetch_dav.Session}. The HTTP stack is any {!Fetch} client,
    such as [Fetch_httpz.std env]. Every call is direct style, and concurrency
    is the caller's to arrange with fibers under the switch the client was
    connected with. *)

module Quirks = Caldav_eio_quirks
module Client = Caldav_eio_client

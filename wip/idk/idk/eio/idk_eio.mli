(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** IDKit clients on Eio.

    The CardDAV and CalDAV clients of {!Idk}, which send the requests the
    protocol libraries describe. Both are built on {!Fetch_dav.Session}, so they
    take any {!Fetch} client as their HTTP stack, such as [Fetch_httpz.std env].
    Every call is direct style, and concurrency is the caller's to arrange with
    fibers under the switch the client was connected with.

    {1 One shape, two protocols}

    The two clients are the same client twice over. {!Carddav.Client.connect}
    and {!Caldav.Client.connect} each find the principal and its home set from
    any URL of the service, and the types they return are shared rather than
    merely alike. A {!Carddav.Client.entry} and a {!Caldav.Client.entry} are
    both {!Dav.Objects.entry}, and their errors are both
    {!Fetch_dav.Session.error}, so a program that mirrors an address book and a
    calendar writes the surrounding code once. {!Dav} is that common ground,
    exposed here for a program that wants to name those types itself.

    {1 Reading and writing}

    A client function that reads or writes an object takes a representation, so
    the program chooses what it sees. This fetches every card of the first
    address book as a {!Idk.Jscontact.Card.t}.

    {[
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let credentials =
      [ Fetch.Credential.basic ~user:"alice" ~password:"secret" ]
    in
    let client =
      Idk_eio.Carddav.Client.connect ~sw ~credentials (Fetch_httpz.std env)
        "https://contacts.example.com/.well-known/carddav"
      |> Result.get_ok
    in
    let books = Idk_eio.Carddav.Client.addressbooks client |> Result.get_ok in
    let book = (List.hd books).href in
    let page =
      Idk_eio.Carddav.Client.query Idk.Carddav_jscontact.card client book
        Idk.Carddav.Filter.all
      |> Result.get_ok
    in
    List.iter
      (fun (e : _ Idk_eio.Carddav.Client.entry) -> print_endline e.value.uid)
      page.entries
    ]}

    {1 Servers that differ}

    Each client has a {e quirks} profile naming what one service does
    differently, which it consults in the functions the difference affects and
    nowhere else, so the protocol layer stays what the RFC says. A profile is
    chosen from the host by default. See {!Carddav.Quirks} and {!Caldav.Quirks}.
*)

module Carddav = Carddav_eio
(** The CardDAV client. {!Carddav_eio.Client} is the connection and
    {!Carddav_eio.Quirks} the profile of a server. *)

module Caldav = Caldav_eio
(** The CalDAV client. {!Caldav_eio.Client} is the connection and
    {!Caldav_eio.Quirks} the profile of a server. *)

module Dav = Fetch_dav
(** The WebDAV client the two are built on. {!Fetch_dav.Session} is the
    connection to a server as one principal, and {!Fetch_dav.Objects} the typed
    reads and writes of the members of a collection, which is where the types
    the two clients share are defined. A program that needs a DAV request
    neither client makes sends it from here. *)

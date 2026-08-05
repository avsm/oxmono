(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** {1 Finger — RFC 1288 User Information Protocol}

    An {{:https://www.rfc-editor.org/rfc/rfc1288}RFC 1288} implementation
    using {{:https://github.com/ocaml-multicore/eio}Eio} for I/O.

    {2 Protocol overview}

    Finger runs on TCP port 79.  A client connects, sends a single query
    line terminated by CRLF, and the server responds with plain text then
    closes the connection.

    {2 Quick start}

    {b Server}
    {[
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      Finger.Server.run ~sw ~net:(Eio.Stdenv.net env) ~port:7979
        ~handler:(fun _q -> "Hello from Finger!\r\n") ()
    ]}

    {b Client}
    {[
      Eio_main.run @@ fun env ->
      let resp = Finger.Client.query ~net:(Eio.Stdenv.net env)
        ~host:"localhost" ~port:7979 "" in
      print_string resp
    ]} *)

module Query = Finger_query
(** {{:https://www.rfc-editor.org/rfc/rfc1288#section-2.3}RFC 1288 §2.3}
    query parsing. *)

module Client = Finger_client
(** Eio-based Finger client. *)

module Server = Finger_server
(** Eio-based Finger server. *)

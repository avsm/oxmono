(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Smoke test for the oracle harness: session, mailboxes, delivery, query. *)

open Jmap.Proto
module H = Oracle_harness
module Auth = Jmap_eio.Auth
module Client = Jmap_eio.Client

let session t =
  let s = Client.session t.H.client in
  Alcotest.(check bool)
    "has core capability" true
    (List.mem_assoc Capability.core s.capabilities);
  Alcotest.(check bool)
    "api url is absolute" true
    (String.length (Client.api_url t.client) > 7
    && String.sub (Client.api_url t.client) 0 4 = "http")

let mailboxes t =
  let inbox = H.mailbox_with_role t `Inbox in
  Alcotest.(check (option string))
    "inbox has no parent" None
    (Option.map Id.to_string inbox.parent_id)

let deliver_and_read t =
  let id, subject = H.deliver_and_wait t () in
  let e =
    H.email t id
      ~properties:
        [
          `Subject;
          `From;
          `Received_at;
          `Sent_at;
          `Keywords;
          `Mailbox_ids;
          `Preview;
        ]
  in
  Alcotest.(check (option string)) "subject" (Some subject) e.subject

(* {1 Credentials} *)

(* What {!Auth.pp} redacts to, per its documentation: the first four
   characters and then three stars, or stars alone for a short secret. *)
let redacted secret =
  if String.length secret <= 4 then "***" else String.sub secret 0 4 ^ "***"

let expected_pp () =
  Printf.sprintf "basic %s:%s" (H.user ()) (redacted (H.password ()))

let check_session client =
  Alcotest.(check bool)
    "has core capability" true
    (List.mem_assoc Capability.core (Client.session client).capabilities);
  Alcotest.(check bool)
    "api url is absolute" true
    (String.starts_with ~prefix:"http" (Client.api_url client))

(* The credential goes in as a scheme, not as a header: Cyrus wants the Basic
   of RFC 7617 and answers 401 to anything else, so a successful session
   fetch is proof that {!Auth.basic} reached the wire intact. *)
let connect_with_auth t =
  Eio.Switch.run @@ fun sw ->
  let auth = Auth.basic ~user:(H.user ()) ~password:(H.password ()) in
  Alcotest.(check string)
    "redacted" (expected_pp ())
    (Fmt.str "%a" Auth.pp auth);
  match
    Client.connect_env ~sw ~auth ~allow_insecure:true t.H.env (H.url ())
  with
  | Error e ->
      Alcotest.failf "connect_env failed: %s" (Client.error_to_string e)
  | Ok client -> check_session client

(* The same credential read from a file, which is how a deployment keeps the
   secret out of the process environment. The file is read when the session
   request needs it, not when the credential is built. *)
let connect_with_key_file t =
  Eio.Switch.run @@ fun sw ->
  let filename = H.unique "oracle-key" ^ ".txt" in
  let path = Eio.Path.(Eio.Stdenv.cwd t.H.env / filename) in
  Eio.Path.save ~create:(`Or_truncate 0o600) path
    (H.user () ^ ":" ^ H.password () ^ "\n");
  Fun.protect ~finally:(fun () -> Eio.Path.unlink path) @@ fun () ->
  let auth = Auth.basic_from_file ~fs:(Eio.Stdenv.cwd t.H.env) filename in
  match
    Client.connect_env ~sw ~auth ~allow_insecure:true t.H.env (H.url ())
  with
  | Error e ->
      Alcotest.failf "connect_env failed: %s" (Client.error_to_string e)
  | Ok client ->
      (* A file credential's [pp] reads nothing, so it prints the path
         rather than a redacted secret; see {!Jmap_eio.Auth.val-pp}. *)
      Alcotest.(check string)
        "file path, not a redacted secret"
        (Printf.sprintf "basic <file:%s>" filename)
        (Fmt.str "%a" Auth.pp auth);
      check_session client

(* A deadline shorter than any round trip cancels the session fetch and is
   reported as {!Client.Timeout}, not as a transport failure. *)
let connect_times_out t =
  Eio.Switch.run @@ fun sw ->
  match H.connect_with ~sw ~timeout:0.001 t.H.env with
  | Ok _ -> Alcotest.fail "expected the session fetch to time out"
  | Error (Client.Timeout seconds) ->
      Alcotest.(check (float 1e-9)) "deadline" 0.001 seconds
  | Error e ->
      Alcotest.failf "expected Timeout, got %s" (Client.error_to_string e)

let () =
  H.run "oracle-smoke"
    [
      ( "smoke",
        [
          H.test_case "session" session;
          H.test_case "mailboxes" mailboxes;
          H.test_case "deliver and read" deliver_and_read;
        ] );
      ( "auth",
        [
          H.test_case "connect with basic auth" connect_with_auth;
          H.test_case "connect with a key file" connect_with_key_file;
          H.test_case "connect times out" connect_times_out;
        ] );
    ]

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Jmap.Proto
module Client = Jmap_eio.Client
module Chain = Jmap.Chain
module Sync = Jmap_eio.Sync

type t = {
  env : Eio_unix.Stdenv.base;
  sw : Eio.Switch.t;
  client : Client.t;
  account_id : Id.t;
  user : string;
  address : string;
}

let getenv name default =
  match Sys.getenv_opt name with Some v when v <> "" -> v | _ -> default

let url_opt () =
  match Sys.getenv_opt "JMAP_ORACLE_URL" with
  | Some u when u <> "" -> Some u
  | _ -> None

let configured () = Option.is_some (url_opt ())

let url () =
  match url_opt () with
  | Some u -> u
  | None -> Alcotest.fail "oracle: JMAP_ORACLE_URL is not set"

let user () = getenv "JMAP_ORACLE_USER" "user1"
let password () = getenv "JMAP_ORACLE_PASSWORD" "x"
let domain () = getenv "JMAP_ORACLE_DOMAIN" "example.com"
let other_user () = getenv "JMAP_ORACLE_USER2" "user2"

(* Cyrus authenticates with HTTP Basic (RFC 7617); it answers 401 to a bearer
   token whatever the token is. *)
let auth () = Jmap_eio.Auth.basic ~user:(user ()) ~password:(password ())

let lmtp () =
  let spec = getenv "JMAP_ORACLE_LMTP" "localhost:18024" in
  match String.rindex_opt spec ':' with
  | Some i ->
      let host = String.sub spec 0 i in
      let port = String.sub spec (i + 1) (String.length spec - i - 1) in
      if host = "" then
        Alcotest.failf "oracle: JMAP_ORACLE_LMTP %S has an empty host" spec;
      let port =
        match int_of_string_opt port with
        | Some port when port >= 1 && port <= 65535 -> port
        | _ ->
            Alcotest.failf
              "oracle: JMAP_ORACLE_LMTP %S requires a port in 1..65535" spec
      in
      (host, port)
  | None -> (spec, 24)

let capabilities = [ Capability.core; Capability.mail; Capability.submission ]

(* RFC 9610 lives behind its own capability, and a server that does not
   implement it answers a request naming it with unknownCapability, so the
   contacts tests send this list rather than widening the one every other test
   uses. *)
let contacts_capabilities = [ Capability.core; Capability.contacts ]

(* The oracle is plain http on localhost, so the credential needs
   [~allow_insecure]; see {!Jmap_eio.Client.connect}. *)
let connect_with ~sw ?auth:credential ?timeout env =
  let auth = match credential with Some a -> a | None -> auth () in
  Client.connect_env ~sw ~auth ?timeout ~allow_insecure:true env (url ())

let connect ~sw env =
  let url = url () in
  let user = user () in
  match connect_with ~sw env with
  | Error e ->
      Alcotest.failf "oracle: cannot connect to %s: %s" url
        (Client.error_to_string e)
  | Ok client ->
      let account_id =
        match
          Session.primary_account_for Capability.mail (Client.session client)
        with
        | Some id -> id
        | None -> Alcotest.fail "oracle: session has no primary mail account"
      in
      { env; sw; client; account_id; user; address = user ^ "@" ^ domain () }

(* RFC 8620 Section 5.4 needs both accounts of a /copy visible to one session,
   which on this oracle means logging in as the user the data was shared with.
   Cyrus accepts any password for its test users. *)
let connect_other ~sw t =
  let user = other_user () in
  let auth = Jmap_eio.Auth.basic ~user ~password:(password ()) in
  match connect_with ~sw ~auth t.env with
  | Error e ->
      Alcotest.failf "oracle: cannot connect as %s: %s" user
        (Client.error_to_string e)
  | Ok client ->
      let account_id =
        match
          Session.primary_account_for Capability.contacts
            (Client.session client)
        with
        | Some id -> id
        | None ->
            Alcotest.failf "oracle: %s has no primary contacts account" user
      in
      (client, account_id)

let test_case name f =
  Alcotest.test_case name `Quick (fun () ->
      if not (configured ()) then Alcotest.skip ()
      else
        Eio_main.run @@ fun env ->
        Eio.Switch.run @@ fun sw -> f (connect ~sw env))

let run name suites = Alcotest.run name suites

let fail e =
  Alcotest.failf "oracle request failed: %s" (Client.error_to_string e)

let request t req =
  match Client.request t.client req with Ok resp -> resp | Error e -> fail e

let call ?client ?(capabilities = capabilities) t chain =
  let client = Option.value client ~default:t.client in
  match Client.call client ~capabilities chain with
  | Ok v -> v
  | Error e -> fail e

(* [run] is the Alcotest entry point, so a chain of several handles is read
   with this. *)
let run_all ?client ?(capabilities = capabilities) t chain =
  let client = Option.value client ~default:t.client in
  match Client.run client ~capabilities chain with
  | Ok vs -> vs
  | Error e -> fail e

(* {1 Test mail} *)

let counter = ref 0

let unique prefix =
  incr counter;
  Printf.sprintf "%s-%d-%d-%d" prefix (Unix.getpid ())
    (int_of_float (Unix.gettimeofday ()))
    !counter

let crlf s =
  let b = Buffer.create (String.length s + 64) in
  String.iter
    (fun c ->
      if c = '\n' then Buffer.add_string b "\r\n" else Buffer.add_char b c)
    s;
  Buffer.contents b

let message ?(from = "Alice <alice@example.org>") ?to_ ?subject
    ?(body = "Hello from the oracle harness.\n") ?(headers = []) () =
  let subject = match subject with Some s -> s | None -> unique "oracle" in
  let to_ = Option.value to_ ~default:(user () ^ "@" ^ domain ()) in
  let id = unique "msg" in
  let date = Unix.gmtime (Unix.gettimeofday ()) in
  let days = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |] in
  let months =
    [|
      "Jan";
      "Feb";
      "Mar";
      "Apr";
      "May";
      "Jun";
      "Jul";
      "Aug";
      "Sep";
      "Oct";
      "Nov";
      "Dec";
    |]
  in
  let date =
    Printf.sprintf "%s, %02d %s %d %02d:%02d:%02d +0000" days.(date.tm_wday)
      date.tm_mday months.(date.tm_mon) (1900 + date.tm_year) date.tm_hour
      date.tm_min date.tm_sec
  in
  let head =
    [
      "From: " ^ from;
      "To: " ^ to_;
      "Subject: " ^ subject;
      "Date: " ^ date;
      "Message-ID: <" ^ id ^ "@example.org>";
      "MIME-Version: 1.0";
    ]
    @ (if
         List.exists
           (fun h ->
             String.length h > 13 && String.sub h 0 13 = "Content-Type:")
           headers
       then []
       else [ "Content-Type: text/plain; charset=utf-8" ])
    @ headers
  in
  (subject, crlf (String.concat "\n" head ^ "\n\n" ^ body))

(* A minimal LMTP client (RFC 2033), enough to inject a message. *)
let deliver t ?(from = "alice@example.org") ?to_ raw =
  let to_ = Option.value to_ ~default:t.address in
  let host, port = lmtp () in
  let net = Eio.Stdenv.net t.env in
  Eio.Net.with_tcp_connect ~host ~service:(string_of_int port) net
  @@ fun flow ->
  let rd = Eio.Buf_read.of_flow ~max_size:(1 lsl 20) flow in
  let expect code what =
    let rec lines () =
      let line = Eio.Buf_read.line rd in
      if String.length line >= 4 && line.[3] = '-' then lines () else line
    in
    let line = lines () in
    if String.length line < 3 || String.sub line 0 3 <> code then
      Alcotest.failf "LMTP %s: expected %s, got %S" what code line
  in
  let send s = Eio.Flow.copy_string (s ^ "\r\n") flow in
  expect "220" "greeting";
  send "LHLO localhost";
  expect "250" "LHLO";
  send (Printf.sprintf "MAIL FROM:<%s>" from);
  expect "250" "MAIL FROM";
  send (Printf.sprintf "RCPT TO:<%s>" to_);
  expect "250" "RCPT TO";
  send "DATA";
  expect "354" "DATA";
  (* dot-stuffing, RFC 5321 4.5.2 *)
  let b = Buffer.create (String.length raw + 16) in
  let at_bol = ref true in
  String.iter
    (fun c ->
      if !at_bol && c = '.' then Buffer.add_char b '.';
      Buffer.add_char b c;
      at_bol := c = '\n')
    raw;
  if not (String.length raw > 0 && raw.[String.length raw - 1] = '\n') then
    Buffer.add_string b "\r\n";
  Eio.Flow.copy_string (Buffer.contents b) flow;
  send ".";
  expect "250" "end of DATA";
  send "QUIT";
  try expect "221" "QUIT" with Alcotest.Test_error -> ()

let query_by_subject ?client ?account_id t subject =
  let account_id = Option.value account_id ~default:t.account_id in
  (call ?client t
     (Chain.email_query ~account_id ~filter:(Email.filter ~subject ())
        ~limit:10L ()))
    .ids

let wait_for_email t ?(timeout = 45.0) ~subject () =
  let clock = Eio.Stdenv.clock t.env in
  let deadline = Eio.Time.now clock +. timeout in
  let rec loop () =
    match query_by_subject t subject with
    | id :: _ -> id
    | [] ->
        if Eio.Time.now clock > deadline then
          Alcotest.failf "no email with subject %S arrived within %.0fs" subject
            timeout
        else (
          Eio.Time.sleep clock 0.25;
          loop ())
  in
  loop ()

let deliver_and_wait t ?from ?subject ?body ?headers () =
  let subject, raw = message ?from ?subject ?body ?headers () in
  deliver t raw;
  (wait_for_email t ~subject (), subject)

let email t ~properties id =
  let g =
    call t
      (Chain.email_get ~account_id:t.account_id ~ids:(Chain.ids [ id ])
         ~properties ())
  in
  match g.list with
  | [ e ] -> e
  | l -> Alcotest.failf "expected one email, got %d" (List.length l)

let mailbox_with_role t role =
  match
    Sync.mailbox_with_role t.client ~capabilities ~account_id:t.account_id role
  with
  | Error e ->
      Alcotest.failf "oracle request failed: %s" (Sync.error_to_string e)
  | Ok (Some mb) -> mb
  | Ok None ->
      Alcotest.failf "no mailbox with role %s" (Mailbox.role_to_string role)

(* Compile-time probes of the kind and mode claims in the Imap_eio facade
   and the core modules behind it. The abbreviation compiles only when its
   kind holds. Each probe is a closure bound at portable mode, as in
   [let (f @ portable) = fun () -> ...], that captures module-level values
   or takes a connection, lease or endpoint as its argument and calls the
   library, so it compiles only when the captured types cross portability
   and contention and the functions called are portable. *)

module Core = Imap_eio_core

module Kinds = struct
  type error : immutable_data = Imap_eio.Error.t
end

let capability = Imap.Capability.of_wire "QRESYNC"
let error = Imap_eio.Error.Not_enabled capability
let username = "alice"

let (credentials @ portable) = fun () ->
  let password = Imap_eio.Auth.password ~username ~password:"secret"
      ~mechanism:`Plain () in
  let bearer = Imap_eio.Auth.bearer ~username ~token:"abc="
      ~allow_insecure_transport:true () in
  let refreshing = Imap_eio.Auth.refreshing ~username (fun () -> "pw") in
  let refreshing_bearer = Imap_eio.Auth.refreshing_bearer ~username
      (fun () -> "abc") in
  List.map (fun auth ->
    Imap_eio.Auth.username auth, Imap_eio.Auth.mechanism auth,
    Imap_eio.Auth.allow_insecure_transport auth)
    [ password; bearer; refreshing; refreshing_bearer ]

let (printer @ portable) = fun () ->
  Imap_eio.Client.error_to_string error,
  Format.asprintf "%a" Imap_eio.Client.pp_error error

let (client_state @ portable) = fun client ->
  let module C = Imap_eio.Client in
  let witnesses = [
    Result.is_ok (C.Acl.require client); Result.is_ok (C.Quota.require client);
    Result.is_ok (C.Metadata.require client);
    Result.is_ok (C.Notify.require client);
    Result.is_ok (C.Multiappend.require client);
    Result.is_ok (C.Compress.require client) ] in
  C.is_open client, C.has client capability, C.is_enabled client capability,
  List.map Imap.Capability.to_wire
    (Imap.Capability.Set.to_list (C.capabilities client)),
  Imap.Capability.Set.is_empty (C.enabled client), C.mailbox_mode client,
  witnesses

let (close_client @ portable) = fun client -> Imap_eio.Client.close client

let (message @ portable) = fun source ->
  Imap_eio.Client.append_message ~length:3L source

let (endpoint @ portable) = fun endpoint ->
  Imap_eio.Transport.(host endpoint, port endpoint, tls endpoint)

let (saved_count @ portable) = fun saved ->
  Imap_eio.Selected.Searchres.saved_search_count saved

let (strategy_view @ portable) = fun selected ->
  Imap_eio.Mailbox.of_selected selected

let (pooled @ portable) = fun ~sw ->
  let pool = Imap_eio.Pool.create ~sw ~max_connections:2
      ~connect:(fun ~sw:_ -> Error Imap_eio.Error.Closed) in
  Imap_eio.Pool.max_connections pool,
  Imap_eio.Pool.use pool (fun _ -> Ok ())

let (session_state @ portable) = fun session ->
  let module S = Core.Session in
  S.check_open session;
  let unsupported = match S.require session capability with
    | () -> false | exception S.Failure _ -> true in
  let rev2 = S.revision_two session in
  let wire = S.mailbox_wire session "INBOX" in
  let parsed = match Imap.Wire.feed (Imap.Wire.create ()) "* 2 EXISTS\r\n"
    with Ok events -> S.parse events | Error e -> failwith e.message in
  S.close session;
  S.has session capability, S.is_enabled session capability, rev2,
  S.mailbox_mode session, wire, parsed, unsupported,
  (match S.require_enabled session capability with
   | () -> false | exception S.Failure _ -> true)

let (session_of_flow @ portable) = fun flow ->
  Core.Session.create (Core.Transport.of_flow flow)

let (transport_flow @ portable) = fun flow ->
  Core.Transport.compressed flow, Core.Transport.close flow

let (transport_endpoint @ portable) = fun ~sw endpoint ->
  let flow = Core.Transport.connect ~sw endpoint in
  Core.Transport.upgrade endpoint flow

let (deflate_close @ portable) = fun flow -> Core.Deflate_flow.close flow

let (responses @ portable) = fun auth bearer ->
  Core.Auth.resolve_password auth, Core.Auth.cram_md5_response auth "<1@x>",
  Core.Auth.plain_response auth, Core.Auth.oauthbearer_response bearer

let (lease @ portable) = fun session info ->
  let selected = Core.Selected.create session 0 info [] in
  Core.Selected.invalidate selected

let with_client f =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow = Eio_mock.Flow.make "modes" in
  Eio_mock.Flow.on_read flow [
    `Return "* PREAUTH ready\r\n";
    `Return "* CAPABILITY IMAP4rev1 QRESYNC ACL\r\nA00000001 OK caps\r\n" ];
  match Imap_eio.Client.of_flow ~sw flow with
  | Ok client -> f ~sw client
  | Error e -> failwith (Imap_eio.Client.error_to_string e)

let test_client () =
  with_client @@ fun ~sw client ->
  let is_open, has, enabled, capabilities, none_enabled, mode, witnesses =
    client_state client in
  Alcotest.(check bool) "open" true is_open;
  Alcotest.(check bool) "has" true has;
  Alcotest.(check bool) "is_enabled" false enabled;
  Alcotest.(check (list string)) "capabilities"
    [ "ACL"; "IMAP4REV1"; "QRESYNC" ] capabilities;
  Alcotest.(check bool) "enabled" true none_enabled;
  Alcotest.(check bool) "mode" true (mode = Imap.Mailbox_name.Rev1);
  Alcotest.(check (list bool)) "witnesses"
    [ true; false; false; false; false; false ] witnesses;
  ignore (message (Eio.Flow.string_source "abc"));
  let size, used = pooled ~sw in
  Alcotest.(check int) "pool size" 2 size;
  Alcotest.(check bool) "pool connect error" true
    (used = Error Imap_eio.Error.Closed);
  close_client client;
  Alcotest.(check bool) "closed" false (Imap_eio.Client.is_open client)

let test_endpoint () =
  Eio_mock.Backend.run @@ fun () ->
  let net = Eio_mock.Net.make "modes-net" in
  let t = Imap_eio.Transport.v ~net ~host:"imap.example" ~tls:`Plain () in
  let host, port, tls = endpoint t in
  Alcotest.(check string) "host" "imap.example" host;
  Alcotest.(check int) "port" 143 port;
  Alcotest.(check bool) "tls" true (tls = `Plain)

let test_session () =
  Eio_mock.Backend.run @@ fun () ->
  let flow = Eio_mock.Flow.make "modes-session" in
  let session = session_of_flow flow in
  let has, enabled, rev2, mode, wire, parsed, unsupported, not_enabled =
    session_state session in
  let compressed, () =
    transport_flow (Core.Transport.of_flow (Eio_mock.Flow.make "modes-raw")) in
  Alcotest.(check bool) "compressed" false compressed;
  Alcotest.(check bool) "require" true unsupported;
  Alcotest.(check bool) "has" false has;
  Alcotest.(check bool) "is_enabled" false enabled;
  Alcotest.(check bool) "revision_two" false rev2;
  Alcotest.(check bool) "mode" true (mode = Imap.Mailbox_name.Rev1);
  Alcotest.(check string) "mailbox_wire" "INBOX" wire;
  Alcotest.(check bool) "parse" true
    (parsed = Imap.Response.Untagged (Imap.Response.Exists 2L));
  Alcotest.(check bool) "require_enabled" true not_enabled

let test_credentials () =
  let mechanisms = List.map (fun (u, m, insecure) ->
    Alcotest.(check string) "username" "alice" u;
    m, insecure) (credentials ()) in
  Alcotest.(check bool) "mechanisms" true
    (mechanisms = [ `Plain, false; `Oauthbearer, true; `Auto, false;
                    `Oauthbearer, false ])

let test_responses () =
  let auth = Core.Auth.password ~username ~password:"secret" () in
  let bearer = Core.Auth.bearer ~username ~token:"abc" () in
  let password, cram, plain, oauth = responses auth bearer in
  Alcotest.(check string) "password" "secret" password;
  Alcotest.(check bool) "CRAM-MD5" true (String.length cram > 0);
  Alcotest.(check string) "PLAIN" (Base64.encode_string "\000alice\000secret")
    plain;
  Alcotest.(check bool) "OAUTHBEARER" true (String.length oauth > 0)

let test_printer () =
  let s, p = printer () in
  Alcotest.(check string) "error_to_string"
    "IMAP extension QRESYNC is not enabled" s;
  Alcotest.(check string) "pp_error" s p

let () =
  Alcotest.run "Imap_eio kinds and modes" [
    "portable", [
      Alcotest.test_case "Auth constructors" `Quick test_credentials;
      Alcotest.test_case "error printers" `Quick test_printer;
      Alcotest.test_case "Auth responses" `Quick test_responses;
      Alcotest.test_case "client state" `Quick test_client;
      Alcotest.test_case "endpoint" `Quick test_endpoint;
      Alcotest.test_case "session state" `Quick test_session ] ]

(* Compile-time probes of the kind and mode claims in the Imap_eio facade.
   The abbreviation compiles only when its kind holds. Each probe is a
   closure bound at portable mode, as in [let (f @ portable) = fun () ->
   ...], that captures module-level values and calls the library, so it
   compiles only when the captured types cross portability and contention
   and the functions called are portable. *)

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

let test_credentials () =
  let mechanisms = List.map (fun (u, m, insecure) ->
    Alcotest.(check string) "username" "alice" u;
    m, insecure) (credentials ()) in
  Alcotest.(check bool) "mechanisms" true
    (mechanisms = [ `Plain, false; `Oauthbearer, true; `Auto, false;
                    `Oauthbearer, false ])

let test_printer () =
  let s, p = printer () in
  Alcotest.(check string) "error_to_string"
    "IMAP extension QRESYNC is not enabled" s;
  Alcotest.(check string) "pp_error" s p

let () =
  Alcotest.run "Imap_eio kinds and modes" [
    "portable", [
      Alcotest.test_case "Auth constructors" `Quick test_credentials;
      Alcotest.test_case "error printers" `Quick test_printer ] ]

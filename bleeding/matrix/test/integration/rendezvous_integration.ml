(** A deliberately separate live MSC4108 rendezvous smoke test.

    This is not part of the account/room integration suite: it exercises the
    unauthenticated rendezvous service itself, so it needs a deployment with
    MSC4108 enabled. Set [MATRIX_TEST_RENDEZVOUS_HOMESERVER] to that
    homeserver's origin before running the [integration-rendezvous] alias. *)

module Qr = Matrix_client.Qr_login.Msc4108
module Secure = Qr.Secure_channel

let rendezvous_homeserver () =
  match Sys.getenv_opt "MATRIX_TEST_RENDEZVOUS_HOMESERVER" with
  | None | Some ("" | " ") -> None
  | Some value -> Some (Uriz.of_string_exn value)

let fail_secure what error =
  failwith (Format.asprintf "%s: %a" what Secure.pp_error error)

let unwrap_secure what = function
  | Ok value -> value
  | Error error -> fail_secure what error

let protect f = try Ok (f ()) with exn -> Error (Printexc.to_string exn)

let await name promise =
  match Eio.Promise.await promise with
  | Ok value -> value
  | Error message -> failwith (Printf.sprintf "%s: %s" name message)

let run env homeserver =
  Eio.Switch.run @@ fun sw ->
  let client =
    Matrix_eio.Client.create ~sw ~env ~homeserver
      ~user_agent:"ocaml-matrix-live-rendezvous" ()
  in
  let clock = Eio.Stdenv.clock env in
  let transport =
    Qr.Rendezvous.transport_of_client
      ~sleep:(fun seconds -> Eio.Time.sleep clock seconds)
      (Matrix_eio.Client.base client)
  in
  let random = Matrix_client.Random.of_env env in
  let rendezvous_server = Uriz.with_path homeserver Qr.rendezvous_path in
  Printf.printf "MSC4108 rendezvous: %s\n%!" (Uriz.to_string rendezvous_server);

  (* The displayed side creates the rendezvous and hands the other side a
     complete QR payload.  Going through Base64 here covers the actual QR
     handoff boundary rather than passing the in-memory record directly. *)
  let displayed =
    unwrap_secure "create rendezvous"
      (Secure.login ~transport ~rendezvous_server ~random ())
  in
  let displayed_ref = ref None in
  let scanner_ref = ref None in
  let almost_ref = ref None in
  Fun.protect
    ~finally:(fun () ->
      Option.iter (fun channel -> ignore (Secure.close channel)) !displayed_ref;
      Option.iter (fun channel -> ignore (Secure.close channel)) !scanner_ref;
      Option.iter
        (fun channel -> ignore (Secure.cancel_almost channel))
        !almost_ref;
      ignore (Secure.cancel_displayed displayed))
    (fun () ->
      let qr =
        match Qr.of_base64 (Secure.qr_code_base64 displayed) with
        | Ok qr -> qr
        | Error error ->
            failwith
              (Format.asprintf "decode handed-off QR code: %a" Qr.pp_codec_error
                 error)
      in
      let connect_promise, connect_resolver = Eio.Promise.create () in
      let scanner_promise, scanner_resolver = Eio.Promise.create () in
      Eio.Fiber.fork ~sw (fun () ->
          Eio.Promise.resolve connect_resolver
            (protect (fun () -> Secure.connect displayed)));
      Eio.Fiber.fork ~sw (fun () ->
          Eio.Promise.resolve scanner_resolver
            (protect (fun () ->
                 Secure.from_qr_code ~transport ~random
                   ~expected_intent:(Qr.Reciprocate homeserver) qr)));
      let almost =
        unwrap_secure "displayed-side ECIES establishment"
          (await "displayed-side ECIES establishment" connect_promise)
      in
      almost_ref := Some almost;
      let scanner =
        unwrap_secure "scanner-side ECIES establishment"
          (await "scanner-side ECIES establishment" scanner_promise)
      in
      scanner_ref := Some scanner;
      let displayed_code = Secure.check_code almost in
      let scanner_code = Secure.check_code_established scanner in
      if displayed_code <> scanner_code then
        failwith
          (Printf.sprintf
             "ECIES check-code mismatch: displayed=%02d scanner=%02d"
             displayed_code scanner_code);
      let displayed_established =
        unwrap_secure "confirm check code"
          (Secure.confirm almost ~check_code:scanner_code)
      in
      displayed_ref := Some displayed_established;
      almost_ref := None;
      Printf.printf "ECIES established (check code %02d)\n%!" displayed_code;

      unwrap_secure "scanner send"
        (Secure.send scanner "MSC4108 live rendezvous scanner -> displayed");
      let received_by_displayed =
        unwrap_secure "displayed receive" (Secure.receive displayed_established)
      in
      if received_by_displayed <> "MSC4108 live rendezvous scanner -> displayed"
      then failwith "displayed side received an unexpected message";
      unwrap_secure "displayed send"
        (Secure.send displayed_established
           "MSC4108 live rendezvous displayed -> scanner");
      let received_by_scanner =
        unwrap_secure "scanner receive" (Secure.receive scanner)
      in
      if received_by_scanner <> "MSC4108 live rendezvous displayed -> scanner"
      then failwith "scanner side received an unexpected message";
      Printf.printf "bidirectional encrypted message delivery passed\n%!";
      unwrap_secure "close displayed channel"
        (Secure.close displayed_established);
      displayed_ref := None;
      unwrap_secure "close scanner channel" (Secure.close scanner);
      scanner_ref := None;

      (* Also exercise the pre-establishment cancellation path. *)
      let cancelled =
        unwrap_secure "create cancellation rendezvous"
          (Secure.login ~transport ~rendezvous_server ~random ())
      in
      unwrap_secure "cancel displayed rendezvous"
        (Secure.cancel_displayed cancelled);
      Printf.printf "cleanup and cancellation passed\n%!");
  print_endline "PASS: live MSC4108 rendezvous integration"

let () =
  match rendezvous_homeserver () with
  | None -> print_endline "SKIP: MATRIX_TEST_RENDEZVOUS_HOMESERVER is not set"
  | Some homeserver -> (
      try Eio_main.run (fun env -> run env homeserver)
      with exn ->
        Printf.eprintf "FAIL: live MSC4108 rendezvous integration: %s\n%!"
          (Printexc.to_string exn);
        exit 1)

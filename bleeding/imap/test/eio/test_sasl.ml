let fail e = failwith (Imap_eio.Client.error_to_string e)
let ok = function Ok x -> x | Error e -> fail e

let test_payloads () =
  let plain = Imap_eio_core.Auth.password ~username:"test"
    ~password:"test" ~mechanism:`Plain () in
  if Imap_eio_core.Auth.plain_response plain <> "AHRlc3QAdGVzdA==" then
    failwith "RFC 4616 PLAIN payload mismatch";
  let calls = ref 0 in
  let bearer = Imap_eio_core.Auth.refreshing_bearer
    ~username:"user,=name" (fun () -> incr calls; "abc._-~+/=") in
  if !calls <> 0 then failwith "bearer token resolved too early";
  let raw = Imap_eio_core.Auth.oauthbearer_response bearer |> Base64.decode_exn in
  if raw <> "n,a=user=2C=3Dname,\001auth=Bearer abc._-~+/=\001\001" then
    failwith "RFC 7628 OAUTHBEARER payload mismatch";
  if !calls <> 1 then failwith "bearer token provider not called once"

let test_plain_sasl_ir () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow = Eio_mock.Flow.make "plain-sasl-ir" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return "* CAPABILITY IMAP4rev1 AUTH=PLAIN SASL-IR LOGINDISABLED\r\nA00000001 OK done\r\n";
    `Return "A00000002 OK authenticated\r\n";
    `Return "* CAPABILITY IMAP4rev1\r\nA00000003 OK done\r\n";
  ];
  let auth = Imap_eio.Auth.password ~username:"test" ~password:"test"
    ~mechanism:`Plain ~allow_insecure_transport:true () in
  let client = ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  Imap_eio.Client.close client

let test_plain_challenge () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow = Eio_mock.Flow.make "plain-challenge" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return "* CAPABILITY IMAP4rev1 AUTH=PLAIN\r\nA00000001 OK done\r\n";
    `Return "+ \r\n";
    `Return "A00000002 OK authenticated\r\n";
    `Return "* CAPABILITY IMAP4rev1\r\nA00000003 OK done\r\n";
  ];
  let auth = Imap_eio.Auth.password ~username:"test" ~password:"test"
    ~mechanism:`Plain ~allow_insecure_transport:true () in
  let client = ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  Imap_eio.Client.close client

let test_oauth_error_exchange () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow = Eio_mock.Flow.make "oauth-error" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return "* CAPABILITY IMAP4rev1 AUTH=OAUTHBEARER SASL-IR\r\nA00000001 OK done\r\n";
    `Return "+ eyJzdGF0dXMiOiJpbnZhbGlkX3Rva2VuIn0=\r\n";
    `Return "A00000002 NO invalid token\r\n";
  ];
  let auth = Imap_eio.Auth.bearer ~username:"user@example.test"
    ~token:"bad-token" ~allow_insecure_transport:true () in
  match Imap_eio.Client.of_flow ~sw ~auth flow with
  | Error (Imap_eio.Error.Rejected {text="authentication rejected"; _}) -> ()
  | Error e -> failwith ("wrong OAUTHBEARER rejection: " ^
      Imap_eio.Client.error_to_string e)
  | Ok _ -> failwith "accepted rejected OAUTHBEARER token"

let test_oauth_challenge () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow = Eio_mock.Flow.make "oauth-challenge" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return "* CAPABILITY IMAP4rev1 AUTH=OAUTHBEARER\r\nA00000001 OK done\r\n";
    `Return "+ \r\n";
    `Return "A00000002 OK authenticated\r\n";
    `Return "* CAPABILITY IMAP4rev1\r\nA00000003 OK done\r\n";
  ];
  let auth = Imap_eio.Auth.bearer ~username:"user@example.test"
    ~token:"good-token" ~allow_insecure_transport:true () in
  let client = ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  Imap_eio.Client.close client

let test_oauth_requires_tls () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow = Eio_mock.Flow.make "oauth-tls" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return "* CAPABILITY IMAP4rev1 AUTH=OAUTHBEARER SASL-IR\r\nA00000001 OK done\r\n";
  ];
  let auth = Imap_eio.Auth.bearer ~username:"user@example.test"
    ~token:"good-token" () in
  match Imap_eio.Client.of_flow ~sw ~auth flow with
  | Error (Imap_eio.Error.State _) -> ()
  | Error e -> failwith ("wrong OAUTHBEARER TLS policy error: " ^
      Imap_eio.Client.error_to_string e)
  | Ok _ -> failwith "sent bearer token without TLS opt-in"

let () =
  test_payloads (); test_plain_sasl_ir (); test_plain_challenge ();
  test_oauth_error_exchange (); test_oauth_challenge ();
  test_oauth_requires_tls ()

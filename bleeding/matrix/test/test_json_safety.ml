(** Regression tests for JSON construction.

    Every request body this library sends is built as a {!Jsont.json} value and
    serialised with {!Jsont_bytesrw.encode_string}, never assembled by string
    interpolation. That is what keeps a password, a session id, a 3PID address
    or a registration token containing a quote, a backslash or a brace from
    mangling the body or injecting members of its own, and it is what these
    tests pin.

    Each site is fed a battery of hostile strings — quotes, backslashes,
    newlines, control characters, brace and comma soup, a member-injection
    payload, non-ASCII text — and the result is asserted to parse as JSON, to
    hold the input byte for byte after the round trip, and to have exactly the
    members it should. The two consumers that thread UIAA auth into a real
    request body, {!Matrix_client.Keys.upload_signing_keys} and
    {!Matrix_client.Auth.get_login_token}, are driven through a [Fetch_mock]
    client so the assertions run on the bytes that leave the library. *)

module Client = Matrix_client.Client
module Error = Matrix_client.Error
module Auth = Matrix_client.Auth
module Backup = Matrix_client.Backup
module Keys = Matrix_client.Keys
module Ck = Matrix_client.Crypto_key
module Olm = Matrix_client.Olm
module Push = Matrix_client.Push
module Rnd = Matrix_client.Random
module Uiaa = Matrix_client.Uiaa
module Id = Matrix_proto.Id

(* Every string here is a value a user could plausibly type (a password
   with a quote in it, a passphrase with a backslash) or an attacker could
   plant in a 3PID address or a server-issued session id. *)

let hostile =
  [
    ("quote", {|say "hello"|});
    ("backslash", {|C:\Users\alice|});
    ("quote and backslash", {|a\"b|});
    ("newline", "first\nsecond");
    ("tab and cr", "a\tb\rc");
    ("control chars", "\001\002\031");
    ("open brace", "{oops");
    ("brace soup", {|},{"x":1}|});
    ("member injection", {|x","admin":true,"y":"|});
    ("object injection", {|"},"admin":{"|});
    ("solidus", "a/b</script>");
    ("non-ascii", "h\xc3\xa9llo w\xc3\xb6rld");
    ("emoji", "\xf0\x9f\x94\x90 locked");
  ]

(* Applied to the payloads that are not free-form user text but still reach
   the encoder (key ids, algorithm names): a shorter list keeps the matrix of
   cases readable. *)
let hostile_short =
  [ ("quote", {|a"b|}); ("backslash", {|a\b|}); ("emoji", "\xf0\x9f\x94\x90") ]

let parse ?(what = "value") s =
  match Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json s with
  | Ok j -> j
  | Error e -> Alcotest.failf "%s is not JSON (%s): %S" what e s

let mems = function
  | Jsont.Object (ms, _) -> ms
  | _ -> Alcotest.fail "expected a JSON object"

(* [find j n] is the value of member [n], or [None]. *)
let find (j : Jsont.json) n =
  match j with
  | Jsont.Object (ms, _) -> Option.map snd (Jsont.Json.find_mem n ms)
  | _ -> None

let rec at (j : Jsont.json) = function
  | [] -> Some j
  | n :: rest -> ( match find j n with Some v -> at v rest | None -> None)

(* [str j path] is the string at slash-separated [path]. *)
let str j path =
  match at j (String.split_on_char '/' path) with
  | Some (Jsont.String (s, _)) -> Some s
  | _ -> None

let names j = List.sort compare (Jsont.Json.object_names (mems j))

let check_str path expected j =
  Alcotest.(check (option string)) path (Some expected) (str j path)

let check_names expected j =
  Alcotest.(check (list string))
    "member names"
    (List.sort compare expected)
    (names j)

let check_names_at path expected j =
  match at j (String.split_on_char '/' path) with
  | Some o -> check_names expected o
  | None -> Alcotest.failf "no object at %s" path

let is_null j = match j with Some (Jsont.Null _) -> true | _ -> false

(* The same shape [test_matrix_client.ml] and [test_encoders.ml] use, copied
   rather than shared so the test files stay independent. *)

let mock_env =
  object
    method secure_random = Eio.Flow.string_source (String.make 32 '\000')
  end

type recorded = { meth : string; url : string; body : string option }

let body_of_request (req : Fetch.Middleware.request) =
  match req.body with
  | Fetch.Empty -> None
  | Fetch.String s -> Some s
  | Fetch.Stream _ -> Some "<stream>"

let mock handler =
  let log = ref [] in
  let client =
    Fetch_mock.client (fun (req : Fetch.Middleware.request) ->
        log :=
          {
            meth = Http.Method.to_string req.meth;
            url = Fetch.Middleware.Url.to_string req.url;
            body = body_of_request req;
          }
          :: !log;
        if
          String.ends_with ~suffix:"/_matrix/client/versions"
            (Fetch.Middleware.Url.to_string req.url)
        then Fetch_mock.respond {|{"versions":["v1.20"]}|} req
        else handler req)
  in
  (log, client)

let default_homeserver = "https://hs.example"

let client_of fetch =
  let config =
    Client.config ~homeserver:(Uriz.of_string_exn default_homeserver) ()
  in
  Client.create ~config ~fetch ~random:(Matrix_client.Random.of_env mock_env)

let uid s = Result.get_ok (Id.User_id.of_string s)
let did s = Result.get_ok (Id.Device_id.of_string s)

let test_session : Client.session =
  {
    user_id = uid "@alice:example.org";
    access_token = "syt_secret_token";
    device_id = did "TESTDEVICE";
    refresh_token = None;
  }

let logged_in fetch = Client.with_session (client_of fetch) test_session
let run f () = Eio_mock.Backend.run f
let json body = Fetch_mock.respond body

let ok = function
  | Ok v -> v
  | Error e -> Alcotest.failf "expected Ok, got error: %s" (Error.to_string e)

let one_body log =
  match List.rev !log with
  | [ { body = Some b; _ } ] -> b
  | [ { body = None; _ } ] -> Alcotest.fail "request carried no body"
  | rs -> Alcotest.failf "expected one request, got %d" (List.length rs)

let latest_body log =
  match !log with
  | { body = Some b; _ } :: _ -> b
  | { body = None; _ } :: _ -> Alcotest.fail "request carried no body"
  | [] -> Alcotest.fail "no request reached the network"

(* [auth_data_to_json] is pure, so these need no scheduler: each case builds
   the auth object with a hostile value in every string slot and reads it
   back. *)

let auth_json auth = parse ~what:"auth data" (Uiaa.auth_data_to_json auth)

let test_password_auth () =
  List.iter
    (fun (_, v) ->
      let j =
        auth_json (Uiaa.password_auth ~user:v ~password:v ~session:v ())
      in
      check_str "type" "m.login.password" j;
      check_str "password" v j;
      check_str "session" v j;
      check_str "identifier/user" v j;
      check_names [ "type"; "identifier"; "password"; "session" ] j;
      check_names_at "identifier" [ "type"; "user" ] j)
    hostile

let test_password_auth_no_session () =
  let j = auth_json (Uiaa.password_auth ~user:{|a"b|} ~password:"p" ()) in
  check_names [ "type"; "identifier"; "password" ] j

let test_token_auth () =
  List.iter
    (fun (_, v) ->
      let j = auth_json (Uiaa.token_auth ~token:v ~session:v ()) in
      check_str "type" "m.login.registration_token" j;
      check_str "token" v j;
      check_str "session" v j;
      check_names [ "type"; "token"; "session" ] j)
    hostile

let test_oauth_auth () =
  Alcotest.(check bool)
    "stable OAuth stage parses distinctly" true
    (match Uiaa.auth_type_of_string "m.oauth" with
    | Uiaa.OAuth -> true
    | _ -> false);
  Alcotest.(check string)
    "stable OAuth stage roundtrips" "m.oauth"
    (Uiaa.auth_type_to_string Uiaa.OAuth);
  Alcotest.(check string)
    "legacy OAuth2 stage remains distinct" "m.login.oauth2"
    (Uiaa.auth_type_to_string Uiaa.OAuth2);
  List.iter
    (fun (_, v) ->
      let j = auth_json (Uiaa.oauth_auth ~session:v ()) in
      check_str "type" "m.oauth" j;
      check_str "session" v j;
      check_names [ "type"; "session" ] j)
    hostile;
  check_names [ "type" ] (auth_json (Uiaa.oauth_auth ()))

let test_recaptcha_auth () =
  List.iter
    (fun (_, v) ->
      let j = auth_json (Uiaa.recaptcha_auth ~response:v ~session:v ()) in
      check_str "type" "m.login.recaptcha" j;
      check_str "response" v j;
      check_str "session" v j;
      check_names [ "type"; "response"; "session" ] j)
    hostile

let test_dummy_auth () =
  List.iter
    (fun (_, v) ->
      let j = auth_json (Uiaa.dummy_auth ~session:v ()) in
      check_str "type" "m.login.dummy" j;
      check_str "session" v j;
      check_names [ "type"; "session" ] j)
    hostile;
  check_names [ "type" ] (auth_json (Uiaa.dummy_auth ()))

let test_terms_auth () =
  List.iter
    (fun (_, v) ->
      let j = auth_json (Uiaa.terms_auth ~session:v ()) in
      check_str "type" "m.login.terms" j;
      check_str "session" v j;
      check_names [ "type"; "session" ] j)
    hostile;
  check_names [ "type" ] (auth_json (Uiaa.terms_auth ()))

let test_email_identity_auth () =
  List.iter
    (fun (_, v) ->
      let j =
        auth_json
          (Uiaa.email_identity_auth ~sid:v ~client_secret:v ~id_server:v
             ~id_access_token:v ~session:v ())
      in
      check_str "type" "m.login.email.identity" j;
      check_str "session" v j;
      check_str "threepid_creds/sid" v j;
      check_str "threepid_creds/client_secret" v j;
      check_str "threepid_creds/id_server" v j;
      check_str "threepid_creds/id_access_token" v j;
      check_names [ "type"; "threepid_creds"; "session" ] j;
      check_names_at "threepid_creds"
        [ "sid"; "client_secret"; "id_server"; "id_access_token" ]
        j)
    hostile;
  (* The optional identity-server members stay absent when unset. *)
  let j = auth_json (Uiaa.email_identity_auth ~sid:"s" ~client_secret:"c" ()) in
  check_names [ "type"; "threepid_creds" ] j;
  check_names_at "threepid_creds" [ "sid"; "client_secret" ] j

let test_msisdn_auth () =
  List.iter
    (fun (_, v) ->
      let j =
        auth_json
          (Uiaa.Msisdn_auth
             {
               threepid_creds =
                 {
                   sid = v;
                   client_secret = v;
                   id_server = Some v;
                   id_access_token = None;
                 };
               session = Some v;
             })
      in
      check_str "type" "m.login.msisdn" j;
      check_str "threepid_creds/sid" v j;
      check_str "threepid_creds/client_secret" v j;
      check_str "threepid_creds/id_server" v j;
      check_names_at "threepid_creds" [ "sid"; "client_secret"; "id_server" ] j)
    hostile

let test_user_identifier () =
  List.iter
    (fun (_, v) ->
      let j =
        parse ~what:"identifier" (Uiaa.user_identifier_to_json (Uiaa.User v))
      in
      check_str "type" "m.id.user" j;
      check_str "user" v j;
      check_names [ "type"; "user" ] j;
      let j =
        parse ~what:"identifier"
          (Uiaa.user_identifier_to_json
             (Uiaa.ThirdParty { medium = v; address = v }))
      in
      check_str "type" "m.id.thirdparty" j;
      check_str "medium" v j;
      check_str "address" v j;
      check_names [ "type"; "medium"; "address" ] j;
      let j =
        parse ~what:"identifier"
          (Uiaa.user_identifier_to_json (Uiaa.Phone { country = v; phone = v }))
      in
      check_str "type" "m.id.phone" j;
      check_str "country" v j;
      check_str "phone" v j;
      check_names [ "type"; "country"; "phone" ] j)
    hostile

(* A password ending in a quote would close the JSON string early and leave
   a trailing brace, so the body would not parse at all. *)
let test_injection_stays_a_string () =
  let payload = {|x","admin":true,"y":"z|} in
  let j = auth_json (Uiaa.password_auth ~user:"@a:b" ~password:payload ()) in
  check_names [ "type"; "identifier"; "password" ] j;
  check_str "password" payload j;
  Alcotest.(check bool) "no injected admin" true (find j "admin" = None)

let auth_for v = Uiaa.auth_data_to_json (Uiaa.dummy_auth ~session:v ())

let added body v =
  match Uiaa.add_auth_to_body ~body ~auth:(auth_for v) with
  | Ok s -> s
  | Error e -> Alcotest.failf "add_auth_to_body: %s" (Error.to_string e)

let add body v = parse ~what:"body" (added body v)

let test_add_auth_empty_body () =
  List.iter
    (fun (_, v) ->
      let j = add "{}" v in
      check_names [ "auth" ] j;
      check_str "auth/session" v j)
    hostile

let test_add_auth_keeps_members () =
  let j = add {|{"a":1,"b":{"c":[1,2,{"d":"e"}]},"f":null}|} "sess" in
  check_names [ "auth"; "a"; "b"; "f" ] j;
  check_names_at "b" [ "c" ] j;
  Alcotest.(check bool) "f still null" true (is_null (find j "f"));
  check_str "auth/session" "sess" j

let test_add_auth_nested_objects () =
  let body = {|{"outer":{"inner":{"deep":"value"}},"n":3}|} in
  let j = add body "sess" in
  check_names [ "auth"; "outer"; "n" ] j;
  check_str "outer/inner/deep" "value" j;
  check_str "auth/session" "sess" j

let test_add_auth_replaces_existing () =
  let body =
    {|{"auth":{"type":"m.login.dummy","session":"stale"},"keep":"x"}|}
  in
  let j = parse ~what:"body" (added body "fresh") in
  check_str "auth/session" "fresh" j;
  check_str "keep" "x" j;
  check_names [ "auth"; "keep" ] j;
  (* Exactly one [auth] member: a duplicate would be legal JSON but the
     server would pick whichever it saw last. *)
  Alcotest.(check int)
    "one auth member" 1
    (List.length
       (List.filter (fun n -> n = "auth") (Jsont.Json.object_names (mems j))))

(* Searching the body as text would be steered by a member value that
   merely contains the word [auth], or a brace, or a comma. *)
let test_add_auth_body_mentions_auth () =
  let note = {|the "auth" field, {like this}|} in
  let body =
    Result.get_ok
      (Jsont_bytesrw.encode_string Matrix_proto.Json.Codec.json
         (Jsont.Json.object'
            [
              Jsont.Json.mem (Jsont.Json.name "note") (Jsont.Json.string note);
              Jsont.Json.mem (Jsont.Json.name "n") (Jsont.Json.number 1.);
            ]))
  in
  let j = add body "sess" in
  check_names [ "auth"; "note"; "n" ] j;
  check_str "note" note j;
  check_str "auth/session" "sess" j

let test_add_auth_body_with_braces_in_strings () =
  let body = {|{"s":"}{,}","t":"\"quoted\""}|} in
  let j = add body "sess" in
  check_names [ "auth"; "s"; "t" ] j;
  check_str "s" "}{,}" j;
  check_str "t" {|"quoted"|} j

let test_add_auth_whitespace_body () =
  let j = add "  {  \"a\" : \"b\"  }  " "sess" in
  check_names [ "auth"; "a" ] j;
  check_str "a" "b" j

(* A body that is not a JSON object is refused rather than turned into
   something malformed. *)
let test_add_auth_non_object_body () =
  let refused what body =
    match Uiaa.add_auth_to_body ~body ~auth:(auth_for "sess") with
    | Error (Error.Json_error _) -> ()
    | Error e -> Alcotest.failf "%s: %s" what (Error.to_string e)
    | Ok _ -> Alcotest.failf "%s was accepted" what
  in
  refused "array body" "[1,2]";
  refused "garbage" "not json"

(* A backup's public key and its signing user are typed, so the only text a
   caller still chooses in the auth data is the key identifier a signature is
   filed under. *)
let backup_random ?(fill = '\x44') () =
  Rnd.of_source (Eio.Flow.string_source (String.make 256 fill))

let encode j =
  match Jsont_bytesrw.encode_string Matrix_proto.Json.Codec.json j with
  | Ok s -> s
  | Error e -> Alcotest.failf "cannot encode: %s" e

let backup_public_key () =
  snd (Ck.Curve25519.generate ~random:(backup_random ()) ())

let hostile_key_id v = Ck.Key_id.v ~algorithm:"ed25519" ~id:v

let test_backup_auth_data_to_json () =
  let user = uid "@alice:example.org" in
  let public_key = backup_public_key () in
  List.iter
    (fun (_, v) ->
      let key_id = hostile_key_id v in
      let signature =
        Ck.Ed25519.Private.sign
          (fst (Ck.Ed25519.generate ~random:(backup_random ()) ()))
          "payload"
      in
      let j =
        parse ~what:"auth data"
          (encode
             (Backup.auth_data_to_json
                { public_key; signatures = [ (user, [ (key_id, signature) ]) ] }))
      in
      check_str "public_key" (Ck.Curve25519.Public.to_base64 public_key) j;
      check_names [ "public_key"; "signatures" ] j;
      match find j "signatures" with
      | Some sigs -> (
          check_names [ Id.User_id.to_string user ] sigs;
          match find sigs (Id.User_id.to_string user) with
          | Some inner ->
              check_names [ Ck.Key_id.to_string key_id ] inner;
              Alcotest.(check (option string))
                "signature"
                (Some (Ck.Signature.to_base64 signature))
                (match find inner (Ck.Key_id.to_string key_id) with
                | Some (Jsont.String (x, _)) -> Some x
                | _ -> None)
          | None -> Alcotest.fail "key id member missing")
      | None -> Alcotest.fail "signatures member missing")
    hostile_short

(* The bytes a backup auth-data signature covers are built by the same
   encoder, so a key identifier needing escaping still signs and verifies. *)
let test_backup_auth_data_signature_bytes () =
  let random = backup_random () in
  let signing_priv, signing_pub = Ck.Ed25519.generate ~random () in
  let user_id = uid "@alice:example.org" in
  let public_key = backup_public_key () in
  let other_key =
    snd (Ck.Curve25519.generate ~random:(backup_random ~fill:'\x55' ()) ())
  in
  List.iter
    (fun (_, v) ->
      let key_id = hostile_key_id v in
      let auth : Backup.megolm_v1_auth_data = { public_key; signatures = [] } in
      let signed =
        Backup.sign_auth_data ~signing_key:signing_priv ~user_id ~key_id auth
      in
      Alcotest.(check bool)
        "verifies" true
        (Backup.verify_auth_data_signature ~verify_key:signing_pub signed
           ~user_id ~key_id
        = Backup.Valid_but_not_trusted);
      (* And the signature is over this key, not some truncated prefix of it. *)
      let other = { signed with Backup.public_key = other_key } in
      Alcotest.(check bool)
        "tamper rejected" true
        (Backup.verify_auth_data_signature ~verify_key:signing_pub other
           ~user_id ~key_id
        = Backup.Invalid))
    hostile_short

let test_delete_pusher () =
  List.iter
    (fun (_, v) ->
      let log, fetch = mock (json "{}") in
      ok (Push.delete_pusher (logged_in fetch) ~pushkey:v ~app_id:v);
      let j = parse ~what:"pusher body" (one_body log) in
      check_names [ "pushkey"; "kind"; "app_id" ] j;
      check_str "pushkey" v j;
      check_str "app_id" v j;
      Alcotest.(check bool) "kind is null" true (is_null (find j "kind")))
    hostile

(* [signed_one_time_keys] signs the canonical JSON [{"key":"<base64>"}].
   The value is always base64, so escaping never bites here; the test pins
   the exact bytes because a change to them silently invalidates every
   signature the library emits. *)
let test_olm_signed_one_time_keys () =
  let random =
    Rnd.of_source (Eio.Flow.string_source (String.make (1 lsl 12) '\x37'))
  in
  let account = Olm.Account.create ~random () in
  Olm.Account.generate_one_time_keys ~random account 3;
  let signed = Olm.Account.signed_one_time_keys account in
  Alcotest.(check int) "three keys" 3 (List.length signed);
  List.iter
    (fun (_key_id, pub, signature) ->
      let pub_b64 = Ck.Curve25519.Public.to_base64 pub in
      let canonical =
        Result.get_ok
          (Jsont_bytesrw.encode_string Matrix_proto.Json.Codec.json
             (Jsont.Json.object'
                [
                  Jsont.Json.mem (Jsont.Json.name "key")
                    (Jsont.Json.string pub_b64);
                ]))
      in
      Alcotest.(check string)
        "canonical form"
        ({|{"key":"|} ^ pub_b64 ^ {|"}|})
        canonical;
      Alcotest.(check bool)
        "signature verifies" true
        (Ck.Ed25519.Public.verify
           (Olm.Account.ed25519_key account)
           ~signature ~data:canonical))
    signed

let master_key : Keys.cross_signing_key =
  {
    user_id = uid "@alice:example.org";
    usage = [ Keys.Master ];
    keys =
      [ (Result.get_ok (Ck.Key_id.of_string "ed25519:MASTERKEY"), "MASTERKEY") ];
    signatures = [];
  }

let test_upload_signing_keys_with_hostile_auth () =
  List.iter
    (fun (_, v) ->
      let log, fetch = mock (json "{}") in
      ok
        (Keys.upload_signing_keys (logged_in fetch) ~master_key
           ~auth:(Uiaa.password_auth ~user:v ~password:v ~session:v ())
           ());
      let j = parse ~what:"signing keys body" (one_body log) in
      check_names [ "auth"; "master_key" ] j;
      check_str "auth/type" "m.login.password" j;
      check_str "auth/password" v j;
      check_str "auth/session" v j;
      check_str "auth/identifier/user" v j;
      check_str "master_key/user_id" "@alice:example.org" j)
    hostile

let test_get_login_token_with_hostile_auth () =
  List.iter
    (fun (_, v) ->
      let log, fetch = mock (json {|{"login_token":"lt","expires_in_ms":1}|}) in
      ignore
        (ok
           (Auth.get_login_token (logged_in fetch)
              ~auth:(Uiaa.token_auth ~token:v ~session:v ())
              ()));
      let j = parse ~what:"login token body" (latest_body log) in
      check_names [ "auth" ] j;
      check_str "auth/type" "m.login.registration_token" j;
      check_str "auth/token" v j;
      check_str "auth/session" v j)
    hostile

let test_request_email_token () =
  List.iter
    (fun (_, v) ->
      let log, fetch = mock (json {|{"sid":"s1"}|}) in
      ignore
        (ok
           (Uiaa.request_email_token (logged_in fetch) ~email:v ~client_secret:v
              ~send_attempt:2 ~next_link:v ()));
      let j = parse ~what:"email token body" (one_body log) in
      check_names [ "client_secret"; "email"; "send_attempt"; "next_link" ] j;
      check_str "email" v j;
      check_str "client_secret" v j;
      check_str "next_link" v j)
    hostile

let test_request_msisdn_token () =
  List.iter
    (fun (_, v) ->
      let log, fetch = mock (json {|{"sid":"s1"}|}) in
      ignore
        (ok
           (Uiaa.request_msisdn_token (logged_in fetch) ~country:v
              ~phone_number:v ~client_secret:v ~send_attempt:1 ()));
      let j = parse ~what:"msisdn token body" (one_body log) in
      check_names
        [ "client_secret"; "country"; "phone_number"; "send_attempt" ]
        j;
      check_str "country" v j;
      check_str "phone_number" v j;
      check_str "client_secret" v j)
    hostile

let test_validate_email_token () =
  List.iter
    (fun (_, v) ->
      let log, fetch = mock (json "{}") in
      ok
        (Uiaa.validate_email_token (logged_in fetch) ~sid:v ~client_secret:v
           ~token:v);
      let j = parse ~what:"validate body" (one_body log) in
      check_names [ "sid"; "client_secret"; "token" ] j;
      check_str "sid" v j;
      check_str "client_secret" v j;
      check_str "token" v j)
    hostile

let pure name f = Alcotest.test_case name `Quick f
let mocked name f = Alcotest.test_case name `Quick (run f)

let () =
  Alcotest.run "json safety"
    [
      ( "uiaa auth data",
        [
          pure "password_auth" test_password_auth;
          pure "password_auth without session" test_password_auth_no_session;
          pure "token_auth" test_token_auth;
          pure "oauth_auth" test_oauth_auth;
          pure "recaptcha_auth" test_recaptcha_auth;
          pure "dummy_auth" test_dummy_auth;
          pure "terms_auth" test_terms_auth;
          pure "email_identity_auth" test_email_identity_auth;
          pure "msisdn_auth" test_msisdn_auth;
          pure "user identifiers" test_user_identifier;
          pure "injection stays a string" test_injection_stays_a_string;
        ] );
      ( "add_auth_to_body",
        [
          pure "empty body" test_add_auth_empty_body;
          pure "keeps members" test_add_auth_keeps_members;
          pure "nested objects" test_add_auth_nested_objects;
          pure "replaces existing auth" test_add_auth_replaces_existing;
          pure "body mentions auth" test_add_auth_body_mentions_auth;
          pure "braces inside strings" test_add_auth_body_with_braces_in_strings;
          pure "whitespace" test_add_auth_whitespace_body;
          pure "non-object body" test_add_auth_non_object_body;
        ] );
      ( "backup",
        [
          pure "auth_data_to_json" test_backup_auth_data_to_json;
          pure "auth data signing bytes" test_backup_auth_data_signature_bytes;
        ] );
      ("olm", [ pure "signed one-time keys" test_olm_signed_one_time_keys ]);
      ("push", [ mocked "delete_pusher" test_delete_pusher ]);
      ( "uiaa consumers",
        [
          mocked "upload_signing_keys"
            test_upload_signing_keys_with_hostile_auth;
          mocked "get_login_token" test_get_login_token_with_hostile_auth;
        ] );
      ( "uiaa 3pid tokens",
        [
          mocked "request_email_token" test_request_email_token;
          mocked "request_msisdn_token" test_request_msisdn_token;
          mocked "validate_email_token" test_validate_email_token;
        ] );
    ]

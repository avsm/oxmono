(** Tests for the E2EE server API, secret storage and recovery keys.

    The HTTP tests build a {!Fetch_mock} client that records what leaves
    {!Matrix_client.Client} and answers from a canned script, so the assertions
    are on the request that actually goes on the wire and on the value the
    library decodes back. The harness is the same shape as
    [test_matrix_client.ml]'s.

    The cryptographic tests pin real vectors: the base58 vectors are the
    Base58Check ones, and the SSSS values were computed independently with
    Python's [hashlib]/[cryptography], so they check this implementation against
    another rather than against itself. *)

module Client = Matrix_client.Client
module Error = Matrix_client.Error
module Backup = Matrix_client.Backup
module Room_key_export = Matrix_client.Room_key_export
module Keys = Matrix_client.Keys
module Room_keys = Matrix_client.Room_keys
module Secret_storage = Matrix_client.Secret_storage
module Secrets = Matrix_client.Secrets
module To_device = Matrix_client.To_device
module Dehydrated_device = Matrix_client.Dehydrated_device
module Rnd = Matrix_client.Random
module Id = Matrix_proto.Id
module Encryption = Matrix_client.Encryption
module Encryption_driver = Matrix_client.Encryption_driver
module Crypto_store = Matrix_client.Crypto_store
module Crypto_key = Matrix_client.Crypto_key
module Cross_signing = Matrix_client.Cross_signing
module Olm = Matrix_client.Olm
module Ev = Matrix_proto.Event

(* {1 Harness} *)

let mock_env =
  object
    method secure_random = Eio.Flow.string_source (String.make 4096 '\000')
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
        handler req)
  in
  (log, client)

let uid s = Result.get_ok (Id.User_id.of_string s)
let did s = Result.get_ok (Id.Device_id.of_string s)
let rid s = Result.get_ok (Id.Room_id.of_string s)
let sid s = Result.get_ok (Id.Session_id.of_string s)

let test_session : Client.session =
  {
    user_id = uid "@alice:example.org";
    access_token = "syt_secret_token";
    device_id = did "TESTDEVICE";
    refresh_token = None;
  }

let client_of fetch =
  let config =
    Client.config ~homeserver:(Uriz.of_string_exn "https://hs.example") ()
  in
  Client.with_session
    (Client.create ~config ~fetch
       ~random:(Matrix_client.Random.of_env mock_env))
    test_session

let requests log = List.rev !log

let one_request log =
  match requests log with
  | [ r ] -> r
  | rs -> Alcotest.failf "expected exactly one request, got %d" (List.length rs)

let check_string = Alcotest.(check string)
let check_str_opt = Alcotest.(check (option string))
let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let run f () = Eio_mock.Backend.run f
let json body = Fetch_mock.respond body
let json_status ~status body req = Fetch_mock.respond ~status body req

let ok_value = function
  | Ok v -> v
  | Error e -> Alcotest.failf "expected Ok, got error: %s" (Error.to_string e)

let contains haystack needle =
  let hn = String.length needle and hh = String.length haystack in
  let rec go i =
    i + hn <= hh && (String.sub haystack i hn = needle || go (i + 1))
  in
  hn = 0 || go 0

let ok_crypto = function
  | Ok v -> v
  | Error (`Msg msg) -> Alcotest.failf "expected Ok, got error: %s" msg

let ok_export = function
  | Ok value -> value
  | Error error ->
      Alcotest.failf "expected key export, got: %a" Room_key_export.pp_error
        error

let ok_str = function
  | Ok v -> v
  | Error msg -> Alcotest.failf "expected Ok, got error: %s" msg

let is_error = function Ok _ -> false | Error _ -> true

let unb64 s =
  match Base64.decode ~pad:false s with
  | Ok v -> v
  | Error (`Msg m) -> Alcotest.failf "bad base64: %s" m

(* A deterministic randomness source, for tests that pin an IV or an
   ephemeral key. *)
let fixed_random bytes = Rnd.of_source (Eio.Flow.string_source bytes)

let hex s =
  String.init
    (String.length s / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub s (i * 2) 2)))

let to_hex s =
  String.concat ""
    (List.map
       (fun c -> Printf.sprintf "%02x" (Char.code c))
       (List.of_seq (String.to_seq s)))

(* {1 1. Base58} *)

(* {1 2. The cryptographic key representation} *)

(* The 32 bytes PBKDF2 derives below, and the recovery key they print as -
   both computed independently. *)
let vector_key_hex =
  "8693a8ddac0dcc157e7c797786358078aa8128d4b81c2d750dc8af8b5bb9ce90"

let vector_recovery_key =
  "EsTg xPBs 6Nb2 J3jz dSc4 Cq6n yc27 P3bh szhs 4PHF SM7h 7UEv"

let key_of_hex h = ok_crypto (Secret_storage.key_of_bytes (hex h))

let test_encode_key () =
  check_string "recovery key" vector_recovery_key
    (Secret_storage.Recovery_key.encode (key_of_hex vector_key_hex));
  (* Four characters per group, single spaces. *)
  List.iter
    (fun g -> check_int "group length" 4 (String.length g))
    (String.split_on_char ' ' vector_recovery_key)

let decode_key s =
  Result.map Secret_storage.to_bytes (Secret_storage.Recovery_key.decode s)

let test_decode_key () =
  check_string "round trip" vector_key_hex
    (to_hex (ok_crypto (decode_key vector_recovery_key)));
  (* Grouping is cosmetic: whitespace anywhere is ignored. *)
  let ungrouped =
    String.concat "" (String.split_on_char ' ' vector_recovery_key)
  in
  check_string "ungrouped" vector_key_hex
    (to_hex (ok_crypto (decode_key ungrouped)));
  check_string "newlines" vector_key_hex
    (to_hex (ok_crypto (decode_key ("\n" ^ ungrouped ^ "\t"))))

let test_decode_key_parity () =
  (* Transposing two characters keeps the key well-formed base58 but breaks
     the parity byte, which is what the parity byte is for. *)
  let b = Bytes.of_string vector_recovery_key in
  let last = Bytes.length b - 1 in
  let a = Bytes.get b (last - 1) and c = Bytes.get b last in
  Bytes.set b (last - 1) c;
  Bytes.set b last a;
  match decode_key (Bytes.to_string b) with
  | Error (`Msg msg) -> check_string "reason" "parity check failed" msg
  | Ok _ -> Alcotest.fail "a transposed recovery key was accepted"

let test_decode_key_header () =
  (* Base58 of 34 zero bytes, which are not headed by 0x8B 0x01. Every
     leading zero byte encodes as a '1'. *)
  match decode_key (String.make 34 '1') with
  | Error (`Msg msg) ->
      check_string "reason" "unexpected recovery key header" msg
  | Ok _ -> Alcotest.fail "a headerless key was accepted"

(* {1 3. Secret storage: key derivation} *)

let vector_passphrase = "It's a secret to everybody"

let vector_passphrase_info : Secret_storage.Passphrase_info.t =
  {
    algorithm = Secret_storage.pbkdf2_algorithm;
    salt = "MmMsAlty";
    iterations = 1000;
    bits = 256;
  }

let vector_key () =
  ok_crypto
    (Secret_storage.key_of_passphrase ~passphrase:vector_passphrase
       vector_passphrase_info)

let test_key_from_passphrase () =
  (* PBKDF2-HMAC-SHA512, salt used as raw bytes. *)
  check_string "derived key" vector_key_hex
    (to_hex (Secret_storage.to_bytes (vector_key ())))

let test_key_from_passphrase_rejects_unknown_kdf () =
  let info =
    {
      vector_passphrase_info with
      Secret_storage.Passphrase_info.algorithm = "m.scrypt";
    }
  in
  check_bool "unknown kdf" true
    (is_error
       (Secret_storage.key_of_passphrase ~passphrase:vector_passphrase info))

let test_recovery_key_round_trip () =
  let key = vector_key () in
  check_string "recovery key" vector_recovery_key
    (Secret_storage.Recovery_key.encode key);
  let back =
    ok_crypto (Secret_storage.Recovery_key.decode vector_recovery_key)
  in
  check_string "round trip"
    (Secret_storage.to_bytes key)
    (Secret_storage.to_bytes back)

(* {1 4. Secret storage: the key check value} *)

(* An IV of 0x00..0x0f, and the MAC of the encrypted zero message under the
   key above - both computed independently. *)
let vector_check_iv = "AAECAwQFBgcICQoLDA0ODw"
let vector_check_mac = "dOTsSZOaneRN5lBribv/0UN8x3NsmDaRhwp4U83X83M"

let test_key_description_check_value () =
  let random = fixed_random (hex "000102030405060708090a0b0c0d0e0f") in
  let d =
    Secret_storage.Key_description.v ~random ~name:"m.default"
      ~passphrase:vector_passphrase_info (vector_key ())
  in
  check_str_opt "name" (Some "m.default") d.name;
  check_string "algorithm" Secret_storage.algorithm d.algorithm;
  check_str_opt "iv" (Some vector_check_iv) d.iv;
  check_str_opt "mac" (Some vector_check_mac) d.mac

let test_iv_bit_63_is_cleared () =
  (* The spec asks for bit 63 of the IV to be zero, to keep every message
     clear of the 64-bit counter boundary that AES-CTR implementations
     disagree about. *)
  let random = fixed_random (String.make 16 '\xff') in
  let d = Secret_storage.Key_description.v ~random (vector_key ()) in
  let iv = unb64 (Option.get d.iv) in
  check_int "byte 8" 0x7f (Char.code iv.[8]);
  check_int "byte 7 untouched" 0xff (Char.code iv.[7])

let test_check_key () =
  let key = vector_key () in
  let d : Secret_storage.Key_description.t =
    {
      name = None;
      algorithm = Secret_storage.algorithm;
      passphrase = Some vector_passphrase_info;
      iv = Some vector_check_iv;
      mac = Some vector_check_mac;
    }
  in
  check_bool "right key" true (Secret_storage.check_key key d = Correct);
  let wrong =
    ok_crypto
      (Secret_storage.key_of_passphrase ~passphrase:"not the passphrase"
         vector_passphrase_info)
  in
  check_bool "wrong key" true (Secret_storage.check_key wrong d = Incorrect);
  (* A description with no check value must be assumed valid. *)
  check_bool "no check value" true
    (Secret_storage.check_key wrong { d with iv = None; mac = None } = Unchecked);
  check_bool "malformed check value" true
    (Secret_storage.check_key key { d with mac = Some "not base64!!" }
    = Incorrect)

(* {1 5. Secret storage: encrypting a secret} *)

let vector_secret = "super secret cross-signing key"

let vector_encrypted : Secret_storage.Encrypted.t =
  {
    iv = "EBESExQVFhcYGRobHB0eHw";
    ciphertext = "4fIClIrlFhoORnwDQ/1UApsgTRL4hLqjNGfxAmDV";
    mac = "YNB0hntJAs5D/qKpwMVzHTsf8mwD1FScH5FPYShG3eQ";
  }

let test_encrypt_matches_vector () =
  let random = fixed_random (hex "101112131415161718191a1b1c1d1e1f") in
  let d =
    Secret_storage.encrypt ~random (vector_key ())
      ~name:Secret_storage.secret_cross_signing_master vector_secret
  in
  check_string "iv" vector_encrypted.iv d.iv;
  check_string "ciphertext" vector_encrypted.ciphertext d.ciphertext;
  check_string "mac" vector_encrypted.mac d.mac

let test_decrypt_vector () =
  check_string "secret" vector_secret
    (ok_crypto
       (Secret_storage.decrypt (vector_key ())
          ~name:Secret_storage.secret_cross_signing_master vector_encrypted))

let test_decrypt_wrong_key_and_name () =
  let wrong =
    ok_crypto
      (Secret_storage.key_of_passphrase ~passphrase:"wrong"
         vector_passphrase_info)
  in
  check_bool "wrong key" true
    (is_error
       (Secret_storage.decrypt wrong
          ~name:Secret_storage.secret_cross_signing_master vector_encrypted));
  (* The secret's name is the HKDF info, so a key derived for one secret
     cannot read another. *)
  check_bool "wrong name" true
    (is_error
       (Secret_storage.decrypt (vector_key ())
          ~name:Secret_storage.secret_megolm_backup_v1 vector_encrypted))

let test_encrypt_decrypt_round_trip () =
  let random = fixed_random (String.make 64 '\x2a') in
  let key = Secret_storage.generate_key ~random in
  let name = Secret_storage.secret_megolm_backup_v1 in
  let secret = String.init 200 (fun i -> Char.chr (i mod 256)) in
  let data = Secret_storage.encrypt ~random key ~name secret in
  check_string "round trip" secret
    (ok_crypto (Secret_storage.decrypt key ~name data))

(* {1 6. Secret storage: account data} *)

let account_data_url ty =
  "https://hs.example/_matrix/client/v3/user/@alice:example.org/account_data/"
  ^ ty

let test_get_default_key_id () =
  let log, fetch = mock (json {|{"key":"bmur3ZtDcv"}|}) in
  let t = client_of fetch in
  check_str_opt "key id" (Some "bmur3ZtDcv")
    (ok_value (Secrets.get_default_key_id t));
  let r = one_request log in
  check_string "method" "GET" r.meth;
  check_string "url" (account_data_url "m.secret_storage.default_key") r.url

let test_get_default_key_id_absent () =
  (* Account data that was never set answers M_NOT_FOUND, which is not an
     error for this question. *)
  let _, fetch =
    mock
      (json_status ~status:404
         {|{"errcode":"M_NOT_FOUND","error":"Account data not found"}|})
  in
  check_str_opt "no default" None
    (ok_value (Secrets.get_default_key_id (client_of fetch)))

let test_put_key_description () =
  let log, fetch = mock (json "{}") in
  let t = client_of fetch in
  let d : Secret_storage.Key_description.t =
    {
      name = Some "my key";
      algorithm = Secret_storage.algorithm;
      passphrase = Some vector_passphrase_info;
      iv = Some vector_check_iv;
      mac = Some vector_check_mac;
    }
  in
  ok_value (Secrets.put_key_description t ~key_id:"abcd" d);
  let r = one_request log in
  check_string "method" "PUT" r.meth;
  check_string "url" (account_data_url "m.secret_storage.key.abcd") r.url;
  check_str_opt "body"
    (Some
       ({|{"name":"my key","algorithm":"m.secret_storage.v1.aes-hmac-sha2",|}
      ^ {|"passphrase":{"algorithm":"m.pbkdf2","salt":"MmMsAlty",|}
      ^ {|"iterations":1000,"bits":256},"iv":"|} ^ vector_check_iv
      ^ {|","mac":"|} ^ vector_check_mac ^ {|"}|}))
    r.body

let test_get_secret () =
  let body =
    Printf.sprintf
      {|{"encrypted":{"other":{"iv":"AA","ciphertext":"AA","mac":"AA"},"abcd":{"iv":"%s","ciphertext":"%s","mac":"%s"}}}|}
      vector_encrypted.iv vector_encrypted.ciphertext vector_encrypted.mac
  in
  let log, fetch = mock (json body) in
  let t = client_of fetch in
  check_string "secret" vector_secret
    (ok_value
       (Secrets.get_secret t ~key_id:"abcd" ~key:(vector_key ())
          ~name:Secret_storage.secret_cross_signing_master));
  let r = one_request log in
  check_string "url" (account_data_url "m.cross_signing.master") r.url

let test_get_secret_missing_key_id () =
  let _, fetch = mock (json {|{"encrypted":{}}|}) in
  check_bool "no copy for this key" true
    (is_error
       (Secrets.get_secret (client_of fetch) ~key_id:"abcd" ~key:(vector_key ())
          ~name:Secret_storage.secret_cross_signing_master));
  let name = Secret_storage.secret_cross_signing_master in
  let encrypted =
    Secret_storage.encrypt
      ~random:(fixed_random (String.make 65536 '\x72'))
      (vector_key ()) ~name "\xff"
  in
  let encrypted =
    Result.get_ok
      (Jsont_bytesrw.encode_string Secret_storage.Encrypted.jsont encrypted)
  in
  let _, fetch = mock (json ({|{"encrypted":{"abcd":|} ^ encrypted ^ "}}")) in
  check_bool "non-UTF-8 plaintext" true
    (is_error
       (Secrets.get_secret (client_of fetch) ~key_id:"abcd" ~key:(vector_key ())
          ~name))

let test_get_secret_opt_absent () =
  let log, fetch =
    mock
      (json_status ~status:404
         {|{"errcode":"M_NOT_FOUND","error":"Account data not found"}|})
  in
  check_str_opt "missing event is optional" None
    (ok_value
       (Secrets.get_secret_opt (client_of fetch) ~key_id:"abcd"
          ~key:(vector_key ()) ~name:Secret_storage.secret_cross_signing_master));
  check_int "one request" 1 (List.length (requests log))

let vector_description_json =
  {|{"algorithm":"m.secret_storage.v1.aes-hmac-sha2","passphrase":|}
  ^ {|{"algorithm":"m.pbkdf2","salt":"MmMsAlty","iterations":1000,"bits":256},|}
  ^ {|"iv":"|} ^ vector_check_iv ^ {|","mac":"|} ^ vector_check_mac ^ {|"}|}

let open_store_fetch ~default ~description =
  mock (fun req ->
      let url = Fetch.Middleware.Url.to_string req.url in
      if String.ends_with ~suffix:"m.secret_storage.default_key" url then
        Fetch_mock.respond default req
      else if String.ends_with ~suffix:"m.secret_storage.key.abcd" url then
        Fetch_mock.respond description req
      else Fetch_mock.respond "{}" req)

let test_open_secret_store_passphrase () =
  let log, fetch =
    open_store_fetch ~default:{|{"key":"abcd"}|}
      ~description:vector_description_json
  in
  let store =
    ok_value
      (Secrets.open_secret_store (client_of fetch) ~credential:vector_passphrase)
  in
  check_string "key id" "abcd" (Secrets.store_key_id store);
  match requests log with
  | [ default; description ] ->
      check_string "default endpoint"
        (account_data_url "m.secret_storage.default_key")
        default.url;
      check_string "description endpoint"
        (account_data_url "m.secret_storage.key.abcd")
        description.url
  | rs ->
      Alcotest.failf "expected default and description requests, got %d"
        (List.length rs)

let test_open_secret_store_recovery_fallback () =
  let _, fetch =
    open_store_fetch ~default:{|{"key":"abcd"}|}
      ~description:vector_description_json
  in
  let store =
    ok_value
      (Secrets.open_secret_store (client_of fetch)
         ~credential:vector_recovery_key)
  in
  check_string "recovery key fallback" "abcd" (Secrets.store_key_id store)

let test_open_secret_store_rejects_wrong_credential_and_algorithm () =
  let _, fetch =
    open_store_fetch ~default:{|{"key":"abcd"}|}
      ~description:vector_description_json
  in
  let error credential =
    match Secrets.open_secret_store (client_of fetch) ~credential with
    | Error e -> Error.to_string e
    | Ok _ -> Alcotest.fail "expected the wrong credential to be rejected"
  in
  let passphrase_error = error "not the passphrase" in
  let wrong_recovery =
    Secret_storage.generate_key
      ~random:(fixed_random (String.make 65536 '\x71'))
    |> Secret_storage.Recovery_key.encode
  in
  check_string "wrong recovery key preserves the passphrase error"
    passphrase_error (error wrong_recovery);
  let _, fetch =
    open_store_fetch ~default:{|{"key":"abcd"}|}
      ~description:{|{"algorithm":"m.something-else"}|}
  in
  check_bool "unsupported algorithm" true
    (is_error
       (Secrets.open_secret_store (client_of fetch)
          ~credential:vector_passphrase))

let test_open_secret_store_missing_default () =
  let _, fetch =
    mock
      (json_status ~status:404
         {|{"errcode":"M_NOT_FOUND","error":"Account data not found"}|})
  in
  check_bool "missing default is rejected" true
    (is_error
       (Secrets.open_secret_store (client_of fetch)
          ~credential:vector_passphrase))

let test_create_secret_store_order_and_reopen () =
  let random = fixed_random (String.make 65536 '\x11') in
  let log, fetch =
    mock (fun req ->
        if String.equal (Http.Method.to_string req.meth) "GET" then
          json_status ~status:404
            {|{"errcode":"M_NOT_FOUND","error":"not present"}|} req
        else json "{}" req)
  in
  let created =
    ok_value
      (Secrets.create_secret_store (client_of fetch) ~random
         ~secrets:[ ("m.test.secret", "hello") ]
         ())
  in
  let calls = requests log in
  check_int "description, read, secret, default order" 4 (List.length calls);
  check_string "description is first" "PUT" (List.nth calls 0).meth;
  check_bool "description path has generated id" true
    (String.starts_with ~prefix:"https://hs.example/_matrix/client/v3/user/"
       (List.nth calls 0).url
    && contains (List.nth calls 0).url ("m.secret_storage.key." ^ created.key_id)
    );
  check_string "secret existence is second" "GET" (List.nth calls 1).meth;
  check_string "secret write is third" "PUT" (List.nth calls 2).meth;
  check_string "default is last" "PUT" (List.nth calls 3).meth;
  check_string "default body"
    (Printf.sprintf {|{"key":"%s"}|} created.key_id)
    (Option.get (List.nth calls 3).body);
  check_bool "key id is Rust-shaped" true
    (String.length created.key_id = 32
    && String.for_all
         (fun c ->
           (c >= 'A' && c <= 'Z')
           || (c >= 'a' && c <= 'z')
           || (c >= '0' && c <= '9'))
         created.key_id);

  let description_body = Option.get (List.nth calls 0).body in
  let reopen_log, reopen_fetch =
    mock (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        if String.ends_with ~suffix:"m.secret_storage.default_key" url then
          json (Printf.sprintf {|{"key":"%s"}|} created.key_id) req
        else if
          String.ends_with
            ~suffix:("m.secret_storage.key." ^ created.key_id)
            url
        then json description_body req
        else json_status ~status:404 {|{"errcode":"M_NOT_FOUND"}|} req)
  in
  let reopened =
    ok_value
      (Secrets.open_secret_store (client_of reopen_fetch)
         ~credential:created.recovery_key)
  in
  check_string "recovery credential reopens" created.key_id
    (Secrets.store_key_id reopened);
  check_int "reopen reads default and description" 2
    (List.length (requests reopen_log));

  let pass_log, pass_fetch =
    mock (fun req ->
        if String.equal (Http.Method.to_string req.meth) "GET" then
          json_status ~status:404 {|{"errcode":"M_NOT_FOUND"}|} req
        else json "{}" req)
  in
  let pass_created =
    ok_value
      (Secrets.create_secret_store (client_of pass_fetch)
         ~random:(fixed_random (String.make 65536 '\x22'))
         ~passphrase:"correct horse battery staple" ())
  in
  let pass_calls = requests pass_log in
  check_int "passphrase creation has two writes" 2 (List.length pass_calls);
  let pass_description = Option.get (List.nth pass_calls 0).body in
  check_bool "passphrase metadata is published" true
    (contains pass_description {|"algorithm":"m.pbkdf2"|});
  let pass_reopen_fetch =
    snd
      (mock (fun req ->
           let url = Fetch.Middleware.Url.to_string req.url in
           if String.ends_with ~suffix:"m.secret_storage.default_key" url then
             json (Printf.sprintf {|{"key":"%s"}|} pass_created.key_id) req
           else json pass_description req))
  in
  let pass_reopened =
    ok_value
      (Secrets.open_secret_store
         (client_of pass_reopen_fetch)
         ~credential:"correct horse battery staple")
  in
  check_string "passphrase reopens" pass_created.key_id
    (Secrets.store_key_id pass_reopened)

let test_create_secret_store_failure_short_circuit () =
  let first_log, first_fetch =
    mock (fun req -> json_status ~status:500 {|{"errcode":"M_UNKNOWN"}|} req)
  in
  check_bool "description failure is returned" true
    (is_error
       (Secrets.create_secret_store (client_of first_fetch)
          ~random:(fixed_random (String.make 65536 '\x33'))
          ()));
  check_int "description failure makes no later writes" 1
    (List.length (requests first_log));
  let put_count = ref 0 in
  let second_log, second_fetch =
    mock (fun req ->
        if String.equal (Http.Method.to_string req.meth) "PUT" then
          if !put_count = 0 then begin
            incr put_count;
            json "{}" req
          end
          else begin
            incr put_count;
            json_status ~status:500 {|{"errcode":"M_UNKNOWN"}|} req
          end
        else json_status ~status:404 {|{"errcode":"M_NOT_FOUND"}|} req)
  in
  check_bool "default failure is returned" true
    (is_error
       (Secrets.create_secret_store (client_of second_fetch)
          ~random:(fixed_random (String.make 65536 '\x44'))
          ()));
  check_int "default failure stops after successful description" 2
    (List.length (requests second_log))

let recovery_store_harness () =
  let description = ref None in
  let default = ref None in
  let events = ref [] in
  let log, fetch =
    mock (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        let body () =
          match req.body with
          | Fetch.String body -> body
          | _ -> Alcotest.fail "expected a string account-data body"
        in
        let method_ = Http.Method.to_string req.meth in
        if String.equal method_ "PUT" then begin
          let body = body () in
          if String.ends_with ~suffix:"m.secret_storage.default_key" url then
            default := Some body
          else if contains url "m.secret_storage.key." then begin
            description := Some body
          end
          else
            List.iter
              (fun name ->
                if String.ends_with ~suffix:name url then
                  events := (name, body) :: List.remove_assoc name !events)
              [
                Secret_storage.secret_cross_signing_master;
                Secret_storage.secret_cross_signing_user_signing;
                Secret_storage.secret_cross_signing_self_signing;
                Secret_storage.secret_megolm_backup_v1;
              ];
          json "{}" req
        end
        else if String.ends_with ~suffix:"m.secret_storage.default_key" url then
          Option.fold
            ~none:(json_status ~status:404 "{}")
            ~some:json !default req
        else if contains url "m.secret_storage.key." then
          match !description with
          | Some body -> json body req
          | None -> json_status ~status:404 "{}" req
        else
          match
            List.find_opt
              (fun (name, _) -> String.ends_with ~suffix:name url)
              !events
          with
          | Some (_, body) -> json body req
          | None -> json_status ~status:404 "{}" req)
  in
  (log, fetch, events)

let seed_for_test = function
  | None -> None
  | Some secret ->
      Some
        (Matrix_proto.Base64.encode
           (Crypto_key.Ed25519.Private.to_bytes secret))

let test_create_recovery_store_export () =
  let identity =
    Cross_signing.create_private_identity ~user_id:test_session.user_id
  in
  Cross_signing.generate_private_keys
    ~random:(fixed_random (String.make 65536 '\x74'))
    identity;
  let backup =
    Backup.Decryption_key.generate
      ~random:(fixed_random (String.make 64 '\x33'))
  in
  let log, fetch, events = recovery_store_harness () in
  let created =
    ok_value
      (Secrets.create_recovery_store (client_of fetch)
         ~random:(fixed_random (String.make 65536 '\x75'))
         ~private_identity:identity ~backup_key:backup ())
  in
  let names =
    List.filter_map
      (fun r ->
        if String.equal r.meth "PUT" then
          List.find_opt
            (fun name -> String.ends_with ~suffix:name r.url)
            [
              Secret_storage.secret_cross_signing_master;
              Secret_storage.secret_cross_signing_user_signing;
              Secret_storage.secret_cross_signing_self_signing;
              Secret_storage.secret_megolm_backup_v1;
            ]
        else None)
      (requests log)
  in
  check_bool "Rust export order" true
    (names
    = [
        Secret_storage.secret_cross_signing_master;
        Secret_storage.secret_cross_signing_user_signing;
        Secret_storage.secret_cross_signing_self_signing;
        Secret_storage.secret_megolm_backup_v1;
      ]);
  check_int "all exported event bodies retained" 4 (List.length !events);
  let reopened =
    ok_value
      (Secrets.open_secret_store (client_of fetch)
         ~credential:created.recovery_key)
  in
  let check_seed label event_type expected =
    let actual =
      ok_value (Secrets.get_store_secret reopened ~name:event_type)
    in
    (match actual with
    | None ->
        Alcotest.failf "missing %s (key id %s, events %s)" label
          (Secrets.store_key_id reopened)
          (String.concat "," (List.map fst !events))
    | Some _ -> ());
    check_str_opt label (Some expected) actual
  in
  check_seed "master seed" Secret_storage.secret_cross_signing_master
    (Option.get (seed_for_test (Cross_signing.master_secret identity)));
  check_seed "user-signing seed"
    Secret_storage.secret_cross_signing_user_signing
    (Option.get (seed_for_test (Cross_signing.user_signing_secret identity)));
  check_seed "self-signing seed"
    Secret_storage.secret_cross_signing_self_signing
    (Option.get (seed_for_test (Cross_signing.self_signing_secret identity)));
  check_seed "backup seed" Secret_storage.secret_megolm_backup_v1
    (Backup.Decryption_key.to_base64 backup);
  ignore events

let test_create_recovery_store_partial_and_mismatch () =
  let identity =
    Cross_signing.create_private_identity ~user_id:test_session.user_id
  in
  Cross_signing.generate_private_keys
    ~random:(fixed_random (String.make 65536 '\x76'))
    identity;
  Cross_signing.set_user_signing_secret identity None;
  let log, fetch, _ = recovery_store_harness () in
  let created =
    ok_value
      (Secrets.create_recovery_store (client_of fetch)
         ~random:(fixed_random (String.make 65536 '\x77'))
         ~private_identity:identity ())
  in
  ignore created;
  let names =
    List.filter_map
      (fun r ->
        if String.equal r.meth "PUT" then
          List.find_opt
            (fun name -> String.ends_with ~suffix:name r.url)
            [
              Secret_storage.secret_cross_signing_master;
              Secret_storage.secret_cross_signing_user_signing;
              Secret_storage.secret_cross_signing_self_signing;
              Secret_storage.secret_megolm_backup_v1;
            ]
        else None)
      (requests log)
  in
  check_bool "missing seed is omitted" true
    (names
    = [
        Secret_storage.secret_cross_signing_master;
        Secret_storage.secret_cross_signing_self_signing;
      ]);
  let other =
    Cross_signing.create_private_identity ~user_id:(uid "@bob:example.org")
  in
  let mismatch_log, mismatch_fetch, _ = recovery_store_harness () in
  check_bool "identity mismatch is rejected before writes" true
    (is_error
       (Secrets.create_recovery_store (client_of mismatch_fetch)
          ~random:(fixed_random (String.make 65536 '\x78'))
          ~private_identity:other ()));
  check_int "mismatch makes no account-data requests" 0
    (List.length (requests mismatch_log));
  let pass_log, pass_fetch, _ = recovery_store_harness () in
  let pass_created =
    ok_value
      (Secrets.create_recovery_store (client_of pass_fetch)
         ~random:(fixed_random (String.make 65536 '\x79'))
         ~passphrase:"recovery passphrase" ~private_identity:identity ())
  in
  ignore pass_log;
  let reopened =
    ok_value
      (Secrets.open_secret_store (client_of pass_fetch)
         ~credential:"recovery passphrase")
  in
  check_string "passphrase recovery key id" pass_created.key_id
    (Secrets.store_key_id reopened)

let json_string j =
  Result.get_ok (Jsont_bytesrw.encode_string Matrix_proto.Json.Codec.json j)

let cross_signing_key_json key =
  Result.get_ok (Jsont_bytesrw.encode_string Keys.cross_signing_key_jsont key)

let device_keys_json key =
  Result.get_ok (Jsont_bytesrw.encode_string Keys.device_keys_jsont key)

let secret_event_json ?(key_id = "store-key") ~key ~random ~name secret =
  let encrypted = Secret_storage.encrypt ~random key ~name secret in
  let encrypted =
    Result.get_ok
      (Jsont_bytesrw.encode_string Secret_storage.Encrypted.jsont encrypted)
  in
  {|{"encrypted":{"|} ^ key_id ^ {|":|} ^ encrypted ^ "}}"

let cross_signing_query_json ?device ~user_id (upload : Cross_signing.upload) =
  let user_id = Id.User_id.to_string user_id in
  let device_keys =
    match device with
    | None -> "{}"
    | Some (device : Keys.device_keys) ->
        {|{"|} ^ user_id ^ {|":{"|}
        ^ Id.Device_id.to_string device.device_id
        ^ {|":|} ^ device_keys_json device ^ "}}"
  in
  {|{"device_keys":|} ^ device_keys ^ {|,"failures":{},"master_keys":{"|}
  ^ user_id ^ "\":"
  ^ cross_signing_key_json upload.master_key
  ^ {|},"self_signing_keys":{"|} ^ user_id ^ "\":"
  ^ cross_signing_key_json upload.self_signing_key
  ^ {|},"user_signing_keys":{"|} ^ user_id ^ "\":"
  ^ cross_signing_key_json upload.user_signing_key
  ^ "}}"

let seed_of_private = function
  | None -> None
  | Some secret ->
      Some
        (Matrix_proto.Base64.encode
           (Matrix_client.Crypto_key.Ed25519.Private.to_bytes secret))

let test_import_cross_signing () =
  let user_id = uid "@alice:example.org" in
  let encryption =
    Encryption.create
      ~random:(fixed_random (String.make 65536 '\x11'))
      ~user_id ~device_id:(did "IMPORT") ()
  in
  let identity = Cross_signing.create_private_identity ~user_id in
  Cross_signing.generate_private_keys
    ~random:(fixed_random (String.make 65536 '\x22'))
    identity;
  let upload = Option.get (Cross_signing.build_upload identity) in
  let device = Encryption.device_keys_for_upload encryption in
  let signed_device =
    Cross_signing.sign_device_keys
      ~signer:(Option.get (Cross_signing.self_signing_secret identity))
      ~signer_user_id:user_id device
  in
  let storage_key =
    Secret_storage.generate_key
      ~random:(fixed_random (String.make 65536 '\x33'))
  in
  let description =
    Secret_storage.Key_description.v
      ~random:(fixed_random (String.make 65536 '\x44'))
      storage_key
  in
  let description_json =
    Result.get_ok
      (Jsont_bytesrw.encode_string Secret_storage.Key_description.jsont
         description)
  in
  let names =
    [
      ( Secret_storage.secret_cross_signing_master,
        seed_of_private (Cross_signing.master_secret identity) );
      ( Secret_storage.secret_cross_signing_self_signing,
        seed_of_private (Cross_signing.self_signing_secret identity) );
      ( Secret_storage.secret_cross_signing_user_signing,
        seed_of_private (Cross_signing.user_signing_secret identity) );
    ]
  in
  let query_count = ref 0 in
  let log, fetch =
    mock (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        if String.ends_with ~suffix:"m.secret_storage.default_key" url then
          Fetch_mock.respond {|{"key":"store-key"}|} req
        else if String.ends_with ~suffix:"m.secret_storage.key.store-key" url
        then Fetch_mock.respond description_json req
        else if String.ends_with ~suffix:"/keys/query" url then (
          incr query_count;
          let device = if !query_count = 1 then device else signed_device in
          Fetch_mock.respond
            (cross_signing_query_json ~device ~user_id upload)
            req)
        else if String.ends_with ~suffix:"/keys/signatures/upload" url then
          Fetch_mock.respond {|{"failures":{}}|} req
        else
          match
            List.find_opt
              (fun (name, _) -> String.ends_with ~suffix:name url)
              names
          with
          | Some (name, Some seed) ->
              Fetch_mock.respond
                (secret_event_json ~key:storage_key
                   ~random:(fixed_random (String.make 65536 '\x55'))
                   ~name seed)
                req
          | _ -> Fetch_mock.respond "{}" req)
  in
  let store =
    ok_value
      (Secrets.open_secret_store (client_of fetch)
         ~credential:(Secret_storage.Recovery_key.encode storage_key))
  in
  let imported = ok_value (Secrets.import_cross_signing store ~encryption) in
  let public role key =
    Cross_signing.key_ed25519 (Cross_signing.key ~role key)
  in
  check_bool "master private public matches" true
    (match
       ( Cross_signing.master_public imported,
         public Cross_signing.Master upload.master_key )
     with
    | Some actual, Some expected ->
        Matrix_client.Crypto_key.Ed25519.Public.equal actual expected
    | _ -> false);
  check_bool "all three private keys imported" true
    (Option.is_some (Cross_signing.self_signing_secret imported)
    && Option.is_some (Cross_signing.user_signing_secret imported));
  check_bool "the recovered self-signing key verifies this device" true
    (match
       Encryption.find_device encryption user_id ~device_id:(did "IMPORT")
     with
    | Some device -> device.trust = Encryption.Verified
    | None -> false);
  let other_encryption =
    Encryption.create
      ~random:(fixed_random (String.make 65536 '\x56'))
      ~user_id:(uid "@bob:example.org") ~device_id:(did "OTHER") ()
  in
  check_bool "a store cannot cross account boundaries" true
    (is_error (Secrets.import_cross_signing store ~encryption:other_encryption));
  match requests log with
  | [
   default;
   description;
   master;
   self_signing;
   user_signing;
   query;
   signature_upload;
   refresh_query;
  ] ->
      check_string "default endpoint"
        (account_data_url "m.secret_storage.default_key")
        default.url;
      check_string "description endpoint"
        (account_data_url "m.secret_storage.key.store-key")
        description.url;
      check_string "master endpoint"
        (account_data_url "m.cross_signing.master")
        master.url;
      check_string "self-signing endpoint"
        (account_data_url "m.cross_signing.self_signing")
        self_signing.url;
      check_string "user-signing endpoint"
        (account_data_url "m.cross_signing.user_signing")
        user_signing.url;
      check_string "keys query endpoint"
        "https://hs.example/_matrix/client/v3/keys/query" query.url;
      check_string "signature upload endpoint"
        "https://hs.example/_matrix/client/v3/keys/signatures/upload"
        signature_upload.url;
      check_string "refresh query endpoint"
        "https://hs.example/_matrix/client/v3/keys/query" refresh_query.url;
      let expected_upload =
        {|{"|}
        ^ Id.User_id.to_string user_id
        ^ {|":{"IMPORT":|}
        ^ device_keys_json signed_device
        ^ "}}"
      in
      let body = Option.value signature_upload.body ~default:"" in
      check_bool "self-signature upload body" true
        (match
           ( Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json
               expected_upload,
             Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json body )
         with
        | Ok expected, Ok actual -> Jsont.Json.equal expected actual
        | _ -> false)
  | rs ->
      Alcotest.failf "expected eight import requests, got %d" (List.length rs)

let test_import_cross_signing_partial_and_stale () =
  let user_id = uid "@alice:example.org" in
  let encryption =
    Encryption.create
      ~random:(fixed_random (String.make 65536 '\x61'))
      ~user_id ~device_id:(did "PARTIAL") ()
  in
  let identity = Cross_signing.create_private_identity ~user_id in
  Cross_signing.generate_private_keys
    ~random:(fixed_random (String.make 65536 '\x62'))
    identity;
  let upload = Option.get (Cross_signing.build_upload identity) in
  let storage_key =
    Secret_storage.generate_key
      ~random:(fixed_random (String.make 65536 '\x63'))
  in
  let description =
    Secret_storage.Key_description.v
      ~random:(fixed_random (String.make 65536 '\x64'))
      storage_key
  in
  let description_json =
    Result.get_ok
      (Jsont_bytesrw.encode_string Secret_storage.Key_description.jsont
         description)
  in
  let master_name = Secret_storage.secret_cross_signing_master in
  let master_seed =
    Option.get (seed_of_private (Cross_signing.master_secret identity))
  in
  let master_event =
    secret_event_json ~key:storage_key
      ~random:(fixed_random (String.make 65536 '\x65'))
      ~name:master_name master_seed
  in
  let stale_seed =
    let stale = Cross_signing.create_private_identity ~user_id in
    Cross_signing.generate_private_keys
      ~random:(fixed_random (String.make 65536 '\x66'))
      stale;
    Option.get (seed_of_private (Cross_signing.master_secret stale))
  in
  let run_import ~master_body =
    let log, fetch =
      mock (fun req ->
          let url = Fetch.Middleware.Url.to_string req.url in
          if String.ends_with ~suffix:"m.secret_storage.default_key" url then
            Fetch_mock.respond {|{"key":"store-key"}|} req
          else if String.ends_with ~suffix:"m.secret_storage.key.store-key" url
          then Fetch_mock.respond description_json req
          else if String.ends_with ~suffix:"m.cross_signing.master" url then
            master_body req
          else if
            String.ends_with ~suffix:"m.cross_signing.self_signing" url
            || String.ends_with ~suffix:"m.cross_signing.user_signing" url
          then Fetch_mock.respond ~status:404 {|{"errcode":"M_NOT_FOUND"}|} req
          else if String.ends_with ~suffix:"/keys/query" url then
            Fetch_mock.respond (cross_signing_query_json ~user_id upload) req
          else Fetch_mock.respond "{}" req)
    in
    let store =
      ok_value
        (Secrets.open_secret_store (client_of fetch)
           ~credential:(Secret_storage.Recovery_key.encode storage_key))
    in
    let result = Secrets.import_cross_signing store ~encryption in
    (result, requests log)
  in
  let imported, partial_requests =
    run_import ~master_body:(fun req -> Fetch_mock.respond master_event req)
  in
  let imported = ok_value imported in
  check_bool "partial import keeps matching key" true
    (Option.is_some (Cross_signing.master_secret imported));
  check_bool "partial import omits missing keys" true
    (Option.is_none (Cross_signing.self_signing_secret imported));
  check_int "partial request count" 6 (List.length partial_requests);
  let stale_event =
    secret_event_json ~key:storage_key
      ~random:(fixed_random (String.make 65536 '\x67'))
      ~name:master_name stale_seed
  in
  let stale, _ =
    run_import ~master_body:(fun req -> Fetch_mock.respond stale_event req)
  in
  check_bool "stale private seed is rejected atomically" true (is_error stale)

let test_store_secret_preserves_other_keys () =
  (* A copy encrypted under another key may use an algorithm this client
     does not know; storing ours must leave it alone. *)
  let existing =
    {|{"encrypted":{"other":{"ciphertext":"opaque","some_field":3}}}|}
  in
  let log, fetch =
    mock (fun req ->
        match Http.Method.to_string req.meth with
        | "GET" -> Fetch_mock.respond existing req
        | _ -> Fetch_mock.respond "{}" req)
  in
  let t = client_of fetch in
  let random = fixed_random (hex "101112131415161718191a1b1c1d1e1f") in
  check_bool "invalid UTF-8 is rejected before I/O" true
    (is_error
       (Secrets.store_secret t ~random ~key_id:"abcd" ~key:(vector_key ())
          ~name:Secret_storage.secret_cross_signing_master "\xff"));
  check_int "invalid UTF-8 made no request" 0 (List.length (requests log));
  ok_value
    (Secrets.store_secret t ~random ~key_id:"abcd" ~key:(vector_key ())
       ~name:Secret_storage.secret_cross_signing_master vector_secret);
  match requests log with
  | [ get; put ] ->
      check_string "get" "GET" get.meth;
      check_string "put" "PUT" put.meth;
      check_string "url" (account_data_url "m.cross_signing.master") put.url;
      check_str_opt "body"
        (Some
           (Printf.sprintf
              {|{"encrypted":{"abcd":{"iv":"%s","ciphertext":"%s","mac":"%s"},"other":{"ciphertext":"opaque","some_field":3}}}|}
              vector_encrypted.iv vector_encrypted.ciphertext
              vector_encrypted.mac))
        put.body
  | rs -> Alcotest.failf "expected two requests, got %d" (List.length rs)

let test_put_store_secret_round_trip () =
  let name = Secret_storage.secret_cross_signing_master in
  let existing =
    {|{"encrypted":{"other":{"ciphertext":"opaque","some_field":3}}}|}
  in
  let current = ref existing in
  let log, fetch =
    mock (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        if String.ends_with ~suffix:"m.secret_storage.default_key" url then
          json {|{"key":"abcd"}|} req
        else if String.ends_with ~suffix:"m.secret_storage.key.abcd" url then
          json vector_description_json req
        else if String.equal (Http.Method.to_string req.meth) "GET" then
          json !current req
        else begin
          current := Option.get (body_of_request req);
          json "{}" req
        end)
  in
  let store =
    ok_value
      (Secrets.open_secret_store (client_of fetch) ~credential:vector_passphrase)
  in
  ok_value
    (Secrets.put_store_secret store
       ~random:(fixed_random (hex "101112131415161718191a1b1c1d1e1f"))
       ~name vector_secret);
  check_str_opt "stored secret decrypts" (Some vector_secret)
    (ok_value (Secrets.get_store_secret store ~name));
  match requests log with
  | [ default; description; get; put; read_back ] ->
      check_string "default lookup" "GET" default.meth;
      check_string "description lookup" "GET" description.meth;
      check_string "store write reads first" "GET" get.meth;
      check_string "store write follows with PUT" "PUT" put.meth;
      check_string "round trip reads back" "GET" read_back.meth;
      check_string "store GET URL" (account_data_url name) get.url;
      check_string "store PUT URL" (account_data_url name) put.url;
      check_string "read-back URL" (account_data_url name) read_back.url;
      check_bool "other key copy is preserved" true
        (contains (Option.get put.body)
           {|"other":{"ciphertext":"opaque","some_field":3}|})
  | rs ->
      Alcotest.failf
        "expected open GETs, GET/PUT, and read-back; got %d requests"
        (List.length rs)

let test_put_store_secret_failure_short_circuit () =
  let name = Secret_storage.secret_cross_signing_master in
  let log, fetch =
    mock (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        if String.ends_with ~suffix:"m.secret_storage.default_key" url then
          json {|{"key":"abcd"}|} req
        else if String.ends_with ~suffix:"m.secret_storage.key.abcd" url then
          json vector_description_json req
        else if String.equal (Http.Method.to_string req.meth) "GET" then
          json_status ~status:500 {|{"errcode":"M_UNKNOWN"}|} req
        else Alcotest.fail "PUT must not follow a failed account-data GET")
  in
  let store =
    ok_value
      (Secrets.open_secret_store (client_of fetch) ~credential:vector_passphrase)
  in
  check_bool "account-data read failure is returned" true
    (is_error
       (Secrets.put_store_secret store
          ~random:(fixed_random (hex "101112131415161718191a1b1c1d1e1f"))
          ~name vector_secret));
  check_int "failed GET short-circuits before PUT" 3
    (List.length (requests log));

  let invalid_log, invalid_fetch =
    mock (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        if String.ends_with ~suffix:"m.secret_storage.default_key" url then
          json {|{"key":"abcd"}|} req
        else if String.ends_with ~suffix:"m.secret_storage.key.abcd" url then
          json vector_description_json req
        else
          Alcotest.fail "invalid UTF-8 must be rejected before account-data I/O")
  in
  let invalid_store =
    ok_value
      (Secrets.open_secret_store (client_of invalid_fetch)
         ~credential:vector_passphrase)
  in
  check_bool "invalid UTF-8 is returned" true
    (is_error
       (Secrets.put_store_secret invalid_store
          ~random:(fixed_random (hex "101112131415161718191a1b1c1d1e1f"))
          ~name "\xff"));
  check_int "invalid UTF-8 made no write/read" 2
    (List.length (requests invalid_log))

(* {1 7. Send-to-device} *)

let jstring s = Jsont.Json.string s

let content pairs =
  Jsont.Json.object'
    (List.map (fun (k, v) -> Jsont.Json.mem (Jsont.Json.name k) v) pairs)

let test_send_to_device () =
  let log, fetch = mock (json "{}") in
  let t = client_of fetch in
  ok_value
    (To_device.send t ~event_type:"m.room_key_request" ~txn_id:"txn-1"
       [
         ( uid "@bob:example.org",
           [
             ( To_device.Device (did "BOBDEVICE"),
               content [ ("action", jstring "request") ] );
             (To_device.All, content [ ("action", jstring "cancel") ]);
           ] );
       ]);
  let r = one_request log in
  check_string "method" "PUT" r.meth;
  check_string "url"
    "https://hs.example/_matrix/client/v3/sendToDevice/m.room_key_request/txn-1"
    r.url;
  check_str_opt "body"
    (Some
       {|{"messages":{"@bob:example.org":{"*":{"action":"cancel"},"BOBDEVICE":{"action":"request"}}}}|})
    r.body

let test_send_to_device_new_txn () =
  let log, fetch = mock (json "{}") in
  let t = client_of fetch in
  ok_value
    (To_device.send_with_new_txn t ~event_type:"m.room_key"
       [
         ( uid "@bob:example.org",
           [ (To_device.Device (did "BOBDEVICE"), content []) ] );
       ]);
  let r = one_request log in
  let prefix =
    "https://hs.example/_matrix/client/v3/sendToDevice/m.room_key/"
  in
  if not (String.starts_with ~prefix r.url) then
    Alcotest.failf "unexpected url %s" r.url;
  if String.length r.url <= String.length prefix then
    Alcotest.fail "no transaction id segment in the URL"

let test_send_to_device_drops_empty_users () =
  let log, fetch = mock (json "{}") in
  let t = client_of fetch in
  ok_value
    (To_device.send t ~event_type:"m.room_key" ~txn_id:"t"
       [
         (uid "@bob:example.org", []);
         (uid "@carol:example.org", [ (To_device.Device (did "C"), content []) ]);
       ]);
  check_str_opt "body" (Some {|{"messages":{"@carol:example.org":{"C":{}}}}|})
    (one_request log).body

(* {1 8. Cross-signing} *)

let key_id s = Result.get_ok (Matrix_client.Crypto_key.Key_id.of_string s)

let signature s =
  Result.get_ok (Matrix_client.Crypto_key.Signature.of_bytes (String.make 64 s))

let master_key : Keys.cross_signing_key =
  {
    user_id = uid "@alice:example.org";
    usage = [ Keys.Master ];
    keys = [ (key_id "ed25519:MASTERKEY", "MASTERKEY") ];
    signatures = [];
  }

let self_signing_key : Keys.cross_signing_key =
  {
    user_id = uid "@alice:example.org";
    usage = [ Keys.Self_signing ];
    keys = [ (key_id "ed25519:SELFKEY", "SELFKEY") ];
    signatures =
      [
        ( uid "@alice:example.org",
          [ (key_id "ed25519:MASTERKEY", signature 's') ] );
      ];
  }

let test_upload_signing_keys () =
  let log, fetch = mock (json "{}") in
  let t = client_of fetch in
  ok_value (Keys.upload_signing_keys t ~master_key ~self_signing_key ());
  let r = one_request log in
  check_string "method" "POST" r.meth;
  check_string "url"
    "https://hs.example/_matrix/client/v3/keys/device_signing/upload" r.url;
  check_str_opt "body"
    (Some
       ({|{"master_key":{"user_id":"@alice:example.org","usage":["master"],|}
      ^ {|"keys":{"ed25519:MASTERKEY":"MASTERKEY"},"signatures":{}},|}
      ^ {|"self_signing_key":{"user_id":"@alice:example.org","usage":["self_signing"],|}
      ^ {|"keys":{"ed25519:SELFKEY":"SELFKEY"},|}
      ^ {|"signatures":{"@alice:example.org":{"ed25519:MASTERKEY":"|}
       ^ Matrix_client.Crypto_key.Signature.to_base64 (signature 's')
       ^ {|"}}}}|}))
    r.body

let test_upload_signing_keys_with_auth () =
  let log, fetch = mock (json "{}") in
  let t = client_of fetch in
  let auth =
    Matrix_client.Uiaa.password_auth ~user:"@alice:example.org"
      ~password:"hunter2" ~session:"sess" ()
  in
  ok_value (Keys.upload_signing_keys t ~master_key ~auth ());
  let body = Option.get (one_request log).body in
  (* The auth object is spliced in alongside the keys. *)
  let has sub =
    let n = String.length sub in
    let rec go i =
      i + n <= String.length body && (String.sub body i n = sub || go (i + 1))
    in
    go 0
  in
  check_bool "carries auth" true (has {|"auth":{|});
  check_bool "carries session" true (has {|"session":"sess"|});
  check_bool "carries the key" true (has {|"master_key"|})

let test_upload_signing_keys_uiaa () =
  (* First attempt is challenged, the callback answers, the retry wins. *)
  let attempts = ref 0 in
  let _, fetch =
    mock (fun req ->
        incr attempts;
        if !attempts = 1 then
          Fetch_mock.respond ~status:401
            {|{"session":"sess","flows":[{"stages":["m.login.password"]}],"completed":[],"params":{}}|}
            req
        else Fetch_mock.respond "{}" req)
  in
  let t = client_of fetch in
  let seen = ref None in
  let result =
    Keys.upload_signing_keys_uiaa t ~master_key
      ~auth_callback:(fun challenge ->
        seen := challenge.session;
        Some
          (Matrix_client.Uiaa.password_auth ~user:"@alice:example.org"
             ~password:"hunter2" ?session:challenge.session ()))
      ()
  in
  (match result with
  | Matrix_client.Uiaa.Uiaa_success () -> ()
  | Matrix_client.Uiaa.Uiaa_auth_required _ ->
      Alcotest.fail "still challenged after answering"
  | Matrix_client.Uiaa.Uiaa_error e ->
      Alcotest.failf "upload failed: %s" (Error.to_string e));
  check_str_opt "challenge session" (Some "sess") !seen;
  check_int "attempts" 2 !attempts

let test_upload_signatures () =
  let log, fetch =
    mock
      (json
         {|{"failures":{"@alice:example.org":{"HIJKLMN":{"errcode":"M_INVALID_SIGNATURE","error":"Invalid signature"}}}}|})
  in
  let t = client_of fetch in
  let signed =
    content
      [
        ("user_id", jstring "@alice:example.org");
        ("device_id", jstring "HIJKLMN");
      ]
  in
  let r =
    ok_value
      (Keys.upload_signatures t
         [
           (uid "@alice:example.org", [ ("HIJKLMN", signed) ]);
           (uid "@bob:example.org", []);
         ])
  in
  let req = one_request log in
  check_string "method" "POST" req.meth;
  check_string "url"
    "https://hs.example/_matrix/client/v3/keys/signatures/upload" req.url;
  (* The user with nothing to publish is dropped rather than sent empty. *)
  check_str_opt "body"
    (Some
       {|{"@alice:example.org":{"HIJKLMN":{"user_id":"@alice:example.org","device_id":"HIJKLMN"}}}|})
    req.body;
  match r.failures with
  | [ (u, [ ("HIJKLMN", _) ]) ]
    when Matrix_proto.Id.User_id.to_string u = "@alice:example.org" ->
      ()
  | _ -> Alcotest.fail "expected one reported failure"

let test_query_keys_cross_signing () =
  let log, fetch =
    mock
      (json
         ({|{"device_keys":{},"failures":{"remote.example":{"status":503}},|}
        ^ {|"master_keys":{"@bob:example.org":{"user_id":"@bob:example.org",|}
        ^ {|"usage":["master"],"keys":{"ed25519:BOBMASTER":"BOBMASTER"}}},|}
        ^ {|"self_signing_keys":{"@bob:example.org":{"user_id":"@bob:example.org",|}
        ^ {|"usage":["self_signing"],"keys":{"ed25519:BOBSELF":"BOBSELF"}}},|}
        ^ {|"user_signing_keys":{}}|}))
  in
  let t = client_of fetch in
  let r =
    ok_value (Keys.query_keys t ~users:[ (uid "@bob:example.org", []) ] ())
  in
  check_string "url" "https://hs.example/_matrix/client/v3/keys/query"
    (one_request log).url;
  check_int "failures" 1 (List.length r.failures);
  check_int "user signing keys" 0 (List.length r.user_signing_keys);
  (match r.master_keys with
  | [ (u, k) ] when Matrix_proto.Id.User_id.to_string u = "@bob:example.org" ->
      check_string "usage" "master" (Keys.key_usage_to_string (List.hd k.usage));
      check_string "key" "BOBMASTER"
        (List.assoc (key_id "ed25519:BOBMASTER") k.keys)
  | _ -> Alcotest.fail "expected one master key");
  check_int "self signing keys" 1 (List.length r.self_signing_keys)

(* {1 9. Server-side key backup} *)

module Ck = Matrix_client.Crypto_key

let backup_key () =
  Backup.Decryption_key.generate ~random:(fixed_random (String.make 64 '\x33'))

let backup_public () = Backup.Decryption_key.public (backup_key ())
let backup_public_b64 () = Ck.Curve25519.Public.to_base64 (backup_public ())

let session_data : Backup.encrypted_session_data =
  { ephemeral = "eph"; ciphertext = "ct"; mac = "mac" }

let backup_data : Room_keys.key_backup_data =
  {
    first_message_index = 1;
    forwarded_count = 0;
    is_verified = true;
    session_data;
  }

let backup_data_json =
  {|{"first_message_index":1,"forwarded_count":0,"is_verified":true,|}
  ^ {|"session_data":{"ephemeral":"eph","ciphertext":"ct","mac":"mac"}}|}

let test_backup_version_state () =
  let state server local = Backup.version_state ~server ~local in
  check_bool "absent is safe to create" true
    (match state None None with Backup.Absent -> true | _ -> false);
  check_bool "same version is current" true
    (match state (Some "7") (Some "7") with
    | Backup.Current "7" -> true
    | _ -> false);
  check_bool "server-only is protected" true
    (match state (Some "7") None with
    | Backup.Server_only "7" -> true
    | _ -> false);
  check_bool "local-only is protected" true
    (match state None (Some "6") with
    | Backup.Local_only "6" -> true
    | _ -> false);
  check_bool "different versions are protected" true
    (match state (Some "7") (Some "6") with
    | Backup.Diverged { server = "7"; local = "6" } -> true
    | _ -> false);
  let auth_data =
    Backup.auth_data_to_json { public_key = backup_public (); signatures = [] }
  in
  let current ?(algorithm = Backup.backup_algorithm) ?local_key auth_data =
    Backup.current_version_state ~algorithm ~auth_data ~local_key
  in
  check_bool "matching algorithm and public key are compatible" true
    (current ~local_key:(backup_public ()) auth_data = Backup.Compatible);
  check_bool "missing local public key fails closed" true
    (current auth_data = Backup.Missing_local_key);
  check_bool "unsupported algorithm fails closed" true
    (match current ~algorithm:"com.example.backup" auth_data with
    | Backup.Unsupported_algorithm "com.example.backup" -> true
    | _ -> false);
  check_bool "malformed auth data fails closed" true
    (match current ~local_key:(backup_public ()) (Jsont.Json.object' []) with
    | Backup.Malformed_auth_data _ -> true
    | _ -> false);
  let other_key =
    Backup.Decryption_key.generate
      ~random:(fixed_random (String.make 64 '\x44'))
    |> Backup.Decryption_key.public
  in
  check_bool "different public key fails closed" true
    (current ~local_key:other_key auth_data = Backup.Different_public_key)

(* The backup secret is an SSSS value containing the Curve25519 secret in
   base64.  Keep this fixture separate from the cross-signing fixtures: the
   import path must validate the server version before changing the machine,
   and must never fetch any room keys. *)
let backup_import_version_json ?(algorithm = Backup.backup_algorithm)
    ?(public_key = backup_public_b64 ()) () =
  Printf.sprintf
    {|{"algorithm":"%s","auth_data":{"public_key":"%s"},"count":0,"etag":"","version":"7"}|}
    algorithm public_key

let backup_import_store_fetch ~secret_body ~version_body () =
  mock (fun req ->
      let url = Fetch.Middleware.Url.to_string req.url in
      if String.ends_with ~suffix:"m.secret_storage.default_key" url then
        json {|{"key":"abcd"}|} req
      else if String.ends_with ~suffix:"m.secret_storage.key.abcd" url then
        json vector_description_json req
      else if
        String.ends_with ~suffix:Secret_storage.secret_megolm_backup_v1 url
      then secret_body req
      else if String.ends_with ~suffix:"/room_keys/version" url then
        version_body req
      else Alcotest.failf "unexpected backup-import request: %s" url)

let open_backup_store ~secret_body ~version_body () =
  let log, fetch = backup_import_store_fetch ~secret_body ~version_body () in
  let store =
    ok_value
      (Secrets.open_secret_store (client_of fetch) ~credential:vector_passphrase)
  in
  (store, log, fetch)

let backup_secret_body_with_key ~key value req =
  json
    (secret_event_json ~key_id:"abcd" ~key
       ~random:(fixed_random (String.make 65536 '\x66'))
       ~name:Secret_storage.secret_megolm_backup_v1 value)
    req

let backup_secret_body value req =
  backup_secret_body_with_key ~key:(vector_key ()) value req

let test_import_backup_order_and_missing () =
  let missing_log, missing_fetch =
    backup_import_store_fetch
      ~secret_body:(json_status ~status:404 {|{"errcode":"M_NOT_FOUND"}|})
      ~version_body:(json (backup_import_version_json ()))
      ()
  in
  let missing_store =
    ok_value
      (Secrets.open_secret_store (client_of missing_fetch)
         ~credential:vector_passphrase)
  in
  let missing_machine =
    Encryption.create
      ~random:(fixed_random (String.make 65536 '\x67'))
      ~user_id:test_session.user_id ~device_id:(did "BACKUP") ()
  in
  let missing_driver = Encryption_driver.v missing_machine in
  check_bool "missing backup secret is classified as not configured" true
    (match
       Secrets.import_backup_typed missing_store ~encryption:missing_driver
     with
    | Ok Secrets.Not_configured -> true
    | _ -> false);
  check_bool "missing backup does not enable a version" true
    (Option.is_none
       (Encryption.backup_version (Encryption_driver.machine missing_driver)));
  check_int "missing secret does not fetch server version" 3
    (List.length (requests missing_log));

  let good_log, good_fetch =
    backup_import_store_fetch
      ~secret_body:
        (backup_secret_body (Backup.Decryption_key.to_base64 (backup_key ())))
      ~version_body:(json (backup_import_version_json ()))
      ()
  in
  let good_store =
    ok_value
      (Secrets.open_secret_store (client_of good_fetch)
         ~credential:vector_passphrase)
  in
  let good_machine =
    Encryption.create
      ~random:(fixed_random (String.make 65536 '\x68'))
      ~user_id:test_session.user_id ~device_id:(did "BACKUP") ()
  in
  let good_driver = Encryption_driver.v good_machine in
  check_bool "compatible backup is classified as imported" true
    (match Secrets.import_backup_typed good_store ~encryption:good_driver with
    | Ok Secrets.Imported -> true
    | _ -> false);
  (match Encryption.backup_version (Encryption_driver.machine good_driver) with
  | None ->
      Alcotest.failf "backup import did not enable version; requests: %s"
        (String.concat ", " (List.map (fun r -> r.url) (requests good_log)))
  | Some _ -> ());
  check_string "current server version is enabled" "7"
    (Option.get
       (Encryption.backup_version (Encryption_driver.machine good_driver)));
  check_bool "backup decryption is enabled" true
    (Encryption.backup_decryption_enabled
       (Encryption_driver.machine good_driver));
  match requests good_log with
  | [ default; description; secret; version ] ->
      check_string "default first" "GET" default.meth;
      check_string "description second" "GET" description.meth;
      check_string "backup secret third" "GET" secret.meth;
      check_string "version fourth" "GET" version.meth
  | rs ->
      Alcotest.failf "unexpected backup import order (%d requests)"
        (List.length rs)

let test_import_backup_rejects_before_mutation () =
  let malformed_log, malformed_fetch =
    backup_import_store_fetch ~secret_body:(backup_secret_body "!")
      ~version_body:(json (backup_import_version_json ()))
      ()
  in
  let malformed_store =
    ok_value
      (Secrets.open_secret_store
         (client_of malformed_fetch)
         ~credential:vector_passphrase)
  in
  let malformed_machine =
    Encryption.create
      ~random:(fixed_random (String.make 65536 '\x69'))
      ~user_id:test_session.user_id ~device_id:(did "BACKUP") ()
  in
  let malformed_driver = Encryption_driver.v malformed_machine in
  check_bool "invalid backup Base64 is an ordinary error" true
    (match
       Secrets.import_backup_typed malformed_store ~encryption:malformed_driver
     with
    | Error (Secrets.Other (Error.Json_error _)) -> true
    | _ -> false);
  check_bool "legacy backup import still rejects invalid Base64" true
    (is_error
       (Secrets.import_backup malformed_store ~encryption:malformed_driver));
  check_bool "malformed key leaves machine untouched" true
    (Option.is_none
       (Encryption.backup_version (Encryption_driver.machine malformed_driver)));
  let malformed_requests = requests malformed_log in
  check_int "each import reads only the encrypted secret" 4
    (List.length malformed_requests);
  check_bool "malformed key stops before version request" true
    (List.for_all
       (fun request ->
         not (String.ends_with ~suffix:"/room_keys/version" request.url))
       malformed_requests);

  let decryption_log, decryption_fetch =
    backup_import_store_fetch
      ~secret_body:
        (backup_secret_body_with_key
           ~key:
             (Secret_storage.generate_key
                ~random:(fixed_random (String.make 65536 '\x6a')))
           (Backup.Decryption_key.to_base64 (backup_key ())))
      ~version_body:(json (backup_import_version_json ()))
      ()
  in
  let decryption_store =
    ok_value
      (Secrets.open_secret_store
         (client_of decryption_fetch)
         ~credential:vector_passphrase)
  in
  let decryption_machine =
    Encryption.create
      ~random:(fixed_random (String.make 65536 '\x6b'))
      ~user_id:test_session.user_id ~device_id:(did "BACKUP") ()
  in
  let decryption_driver = Encryption_driver.v decryption_machine in
  check_bool "undecryptable SSSS backup is repairable" true
    (match
       Secrets.import_backup_typed decryption_store
         ~encryption:decryption_driver
     with
    | Error Secrets.Missing_or_invalid_backup_secret -> true
    | _ -> false);
  check_int "undecryptable backup stops before version request" 3
    (List.length (requests decryption_log));

  let other =
    Backup.Decryption_key.generate
      ~random:(fixed_random (String.make 64 '\x70'))
    |> Backup.Decryption_key.public |> Ck.Curve25519.Public.to_base64
  in
  let mismatch_log, mismatch_fetch =
    backup_import_store_fetch
      ~secret_body:
        (backup_secret_body (Backup.Decryption_key.to_base64 (backup_key ())))
      ~version_body:(json (backup_import_version_json ~public_key:other ()))
      ()
  in
  let mismatch_store =
    ok_value
      (Secrets.open_secret_store (client_of mismatch_fetch)
         ~credential:vector_passphrase)
  in
  let mismatch_machine =
    Encryption.create
      ~random:(fixed_random (String.make 65536 '\x71'))
      ~user_id:test_session.user_id ~device_id:(did "BACKUP") ()
  in
  let mismatch_driver = Encryption_driver.v mismatch_machine in
  check_bool "public-key mismatch is typed" true
    (match
       Secrets.import_backup_typed mismatch_store ~encryption:mismatch_driver
     with
    | Error Secrets.Inconsistent_backup_key -> true
    | _ -> false);
  check_bool "mismatch leaves machine untouched" true
    (Option.is_none
       (Encryption.backup_version (Encryption_driver.machine mismatch_driver)));
  check_int "mismatch fetches exactly one version" 4
    (List.length (requests mismatch_log))

let test_import_backup_persistence_failure () =
  (* A stale crypto-store handle makes the final driver save fail.  The
     account-data and version validation still complete, proving that this
     path reports persistence failure rather than claiming the import was
     durable. *)
  Eio_main.run @@ fun env ->
  let base = Filename.temp_file "matrix-backup-import-" "" in
  Sys.remove base;
  Unix.mkdir base 0o700;
  Unix.putenv "XDG_DATA_HOME" (Filename.concat base "data");
  Unix.putenv "XDG_CONFIG_HOME" (Filename.concat base "config");
  Unix.putenv "XDG_CACHE_HOME" (Filename.concat base "cache");
  Unix.putenv "XDG_STATE_HOME" (Filename.concat base "state");
  let runtime = Filename.concat base "runtime" in
  Unix.mkdir runtime 0o700;
  Unix.putenv "XDG_RUNTIME_DIR" runtime;
  let xdg = Xdge.create (Eio.Stdenv.fs env) "matrix-backup-import" in
  let current_store = Crypto_store.create ~xdg ~profile:"default" in
  let stale_store = Crypto_store.create ~xdg ~profile:"default" in
  let current_machine =
    Encryption.create
      ~random:(fixed_random (String.make 65536 '\x72'))
      ~user_id:test_session.user_id ~device_id:(did "CURRENT") ()
  in
  let current_driver =
    Encryption_driver.v ~store:current_store current_machine
  in
  ok_value (Encryption_driver.save current_driver);
  let stale_machine =
    Encryption.create
      ~random:(fixed_random (String.make 65536 '\x73'))
      ~user_id:test_session.user_id ~device_id:(did "BACKUP") ()
  in
  let stale_driver = Encryption_driver.v ~store:stale_store stale_machine in
  let _, fetch =
    backup_import_store_fetch
      ~secret_body:
        (backup_secret_body (Backup.Decryption_key.to_base64 (backup_key ())))
      ~version_body:(json (backup_import_version_json ()))
      ()
  in
  let store =
    ok_value
      (Secrets.open_secret_store (client_of fetch) ~credential:vector_passphrase)
  in
  match Secrets.import_backup store ~encryption:stale_driver with
  | Error (Error.Policy_denied _) -> ()
  | Ok () -> Alcotest.fail "backup import reported success after stale save"
  | Error error ->
      Alcotest.failf "wrong persistence error: %s" (Error.to_string error)

let test_create_version () =
  let log, fetch = mock (json {|{"version":"1"}|}) in
  let t = client_of fetch in
  let auth_data =
    Backup.auth_data_to_json { public_key = backup_public (); signatures = [] }
  in
  check_string "version" "1"
    (ok_value
       (Room_keys.create_version t ~algorithm:Backup.backup_algorithm ~auth_data));
  let r = one_request log in
  check_string "method" "POST" r.meth;
  check_string "url" "https://hs.example/_matrix/client/v3/room_keys/version"
    r.url;
  check_str_opt "body"
    (Some
       (Printf.sprintf
          {|{"algorithm":"m.megolm_backup.v1.curve25519-aes-sha2","auth_data":{"public_key":"%s","signatures":{}}}|}
          (backup_public_b64 ())))
    r.body

let test_get_current_version () =
  let log, fetch =
    mock
      (json
         (Printf.sprintf
            {|{"algorithm":"m.megolm_backup.v1.curve25519-aes-sha2","auth_data":{"public_key":"%s"},"count":42,"etag":"tag","version":"7"}|}
            (backup_public_b64 ())))
  in
  let t = client_of fetch in
  let v = ok_value (Room_keys.get_current_version t) in
  check_string "version" "7" v.version;
  check_int "count" 42 v.count;
  check_string "etag" "tag" v.etag;
  let auth =
    ok_str (Jsont.Json.decode Backup.megolm_v1_auth_data_jsont v.auth_data)
  in
  check_string "public key" (backup_public_b64 ())
    (Ck.Curve25519.Public.to_base64 auth.public_key);
  let r = one_request log in
  check_string "method" "GET" r.meth;
  check_string "url" "https://hs.example/_matrix/client/v3/room_keys/version"
    r.url

let test_update_and_delete_version () =
  let log, fetch = mock (json "{}") in
  let t = client_of fetch in
  let auth_data = Jsont.Json.object' [] in
  ok_value
    (Room_keys.update_version t ~version:"7" ~algorithm:Backup.backup_algorithm
       ~auth_data);
  ok_value (Room_keys.delete_version t ~version:"7");
  match requests log with
  | [ put; del ] ->
      check_string "put method" "PUT" put.meth;
      check_string "put url"
        "https://hs.example/_matrix/client/v3/room_keys/version/7" put.url;
      check_string "delete method" "DELETE" del.meth;
      check_string "delete url"
        "https://hs.example/_matrix/client/v3/room_keys/version/7" del.url
  | rs -> Alcotest.failf "expected two requests, got %d" (List.length rs)

let test_put_keys () =
  let log, fetch = mock (json {|{"etag":"tag","count":3}|}) in
  let t = client_of fetch in
  let r =
    ok_value
      (Room_keys.put_keys t ~version:"7"
         [ ("!room:example.org", [ ("sess1", backup_data) ]) ])
  in
  check_string "etag" "tag" r.etag;
  check_int "count" 3 r.count;
  let req = one_request log in
  check_string "method" "PUT" req.meth;
  check_string "url"
    "https://hs.example/_matrix/client/v3/room_keys/keys?version=7" req.url;
  check_str_opt "body"
    (Some
       (Printf.sprintf
          {|{"rooms":{"!room:example.org":{"sessions":{"sess1":%s}}}}|}
          backup_data_json))
    req.body

let test_get_keys () =
  let body =
    Printf.sprintf {|{"rooms":{"!room:example.org":{"sessions":{"sess1":%s}}}}|}
      backup_data_json
  in
  let log, fetch = mock (json body) in
  let t = client_of fetch in
  let rooms = ok_value (Room_keys.get_keys t ~version:"7") in
  (match rooms with
  | [ ("!room:example.org", [ ("sess1", d) ]) ] ->
      check_int "index" 1 d.first_message_index;
      check_bool "verified" true d.is_verified;
      check_string "ephemeral" "eph" d.session_data.ephemeral
  | _ -> Alcotest.fail "unexpected backup shape");
  check_string "url"
    "https://hs.example/_matrix/client/v3/room_keys/keys?version=7"
    (one_request log).url

let test_room_and_session_key_paths () =
  let log, fetch = mock (json {|{"etag":"tag","count":1}|}) in
  let t = client_of fetch in
  let room_id = rid "!room:example.org" in
  ignore
    (Room_keys.put_room_keys t ~version:"7" ~room_id [ ("s", backup_data) ]);
  ignore (Room_keys.get_room_keys t ~version:"7" ~room_id);
  ignore (Room_keys.delete_room_keys t ~version:"7" ~room_id);
  ignore
    (Room_keys.put_session_key t ~version:"7" ~room_id ~session_id:(sid "s")
       backup_data);
  ignore
    (Room_keys.get_session_key t ~version:"7" ~room_id ~session_id:(sid "s"));
  ignore
    (Room_keys.delete_session_key t ~version:"7" ~room_id ~session_id:(sid "s"));
  ignore (Room_keys.delete_keys t ~version:"7");
  let base = "https://hs.example/_matrix/client/v3/room_keys/keys" in
  let room = base ^ "/!room:example.org" in
  let session = room ^ "/s" in
  let expected =
    [
      ("PUT", room ^ "?version=7");
      ("GET", room ^ "?version=7");
      ("DELETE", room ^ "?version=7");
      ("PUT", session ^ "?version=7");
      ("GET", session ^ "?version=7");
      ("DELETE", session ^ "?version=7");
      ("DELETE", base ^ "?version=7");
    ]
  in
  List.iter2
    (fun (meth, url) r ->
      check_string "method" meth r.meth;
      check_string "url" url r.url)
    expected (requests log)

let test_put_room_keys_body () =
  let log, fetch = mock (json {|{"etag":"tag","count":1}|}) in
  let t = client_of fetch in
  ignore
    (Room_keys.put_room_keys t ~version:"7" ~room_id:(rid "!room:example.org")
       [ ("sess1", backup_data) ]);
  check_str_opt "body"
    (Some (Printf.sprintf {|{"sessions":{"sess1":%s}}|} backup_data_json))
    (one_request log).body

(* {1 10. Backup session data} *)

let backup_megolm_keys byte =
  let outbound =
    Olm.Megolm.Outbound.create
      ~random:(fixed_random (String.make 4096 byte))
      ~room_id:(rid "!room:example.org") ()
  in
  ( Olm.Megolm.Outbound.exported_session_key outbound,
    Olm.Megolm.Outbound.session_key outbound )

let test_backup_session_round_trip () =
  (* 32 bytes seed the backup key, another 32 the ephemeral key. *)
  let random = fixed_random (String.make 128 '\x11') in
  let decryption_key = Backup.Decryption_key.generate ~random in
  let encryption_key = Backup.Decryption_key.public decryption_key in
  let session_key, _ = backup_megolm_keys '\x12' in
  let data =
    ok_crypto
      (Backup.encrypt_room_key ~random encryption_key ~session_key
         ~sender_key:"RF3s+E7RkTQTGF2d8Deol0FkQvgII2aJDf3/Jp5mxVU")
  in
  let plaintext = ok_crypto (Backup.decrypt_room_key decryption_key data) in
  let key =
    ok_crypto
      (Backup.parse_recovered_key ~room_id:(rid "!room:example.org")
         ~session_id:(sid "sess") plaintext)
  in
  check_string "session key" session_key key.session_key;
  check_string "sender key" "RF3s+E7RkTQTGF2d8Deol0FkQvgII2aJDf3/Jp5mxVU"
    key.sender_key;
  check_string "algorithm" "m.megolm.v1.aes-sha2" key.algorithm;
  check_string "room id" "!room:example.org"
    (Matrix_proto.Id.Room_id.to_string key.room_id);
  check_bool "not forwarded" false key.forwarded;
  (* The mac is the truncated HMAC of the empty string, so it is 8 bytes. *)
  check_int "mac length" 8 (String.length (unb64 data.mac))

let test_backup_session_wrong_key () =
  let random = fixed_random (String.make 128 '\x11') in
  let encryption_key =
    Backup.Decryption_key.public (Backup.Decryption_key.generate ~random)
  in
  let session_key, signed_session_key = backup_megolm_keys '\x13' in
  let data =
    ok_crypto
      (Backup.encrypt_room_key ~random encryption_key ~session_key
         ~sender_key:"RF3s+E7RkTQTGF2d8Deol0FkQvgII2aJDf3/Jp5mxVU")
  in
  let other =
    Backup.Decryption_key.generate
      ~random:(fixed_random (String.make 64 '\x22'))
  in
  check_bool "wrong backup key" true
    (is_error (Backup.decrypt_room_key other data));
  check_bool "malformed session key is rejected before randomness" true
    (is_error
       (Backup.encrypt_room_key ~random:(fixed_random "") encryption_key
          ~session_key:"not a Megolm key"
          ~sender_key:"RF3s+E7RkTQTGF2d8Deol0FkQvgII2aJDf3/Jp5mxVU"));
  check_bool "signed room key is not accepted as an exported key" true
    (is_error
       (Backup.encrypt_room_key ~random:(fixed_random "") encryption_key
          ~session_key:signed_session_key
          ~sender_key:"RF3s+E7RkTQTGF2d8Deol0FkQvgII2aJDf3/Jp5mxVU"));
  let overlong_session_key =
    Result.get_ok (Matrix_proto.Base64.decode session_key) ^ "x"
    |> Matrix_proto.Base64.encode
  in
  check_bool "overlong exported key is rejected before randomness" true
    (is_error
       (Backup.encrypt_room_key ~random:(fixed_random "") encryption_key
          ~session_key:overlong_session_key
          ~sender_key:"RF3s+E7RkTQTGF2d8Deol0FkQvgII2aJDf3/Jp5mxVU"));
  check_bool "malformed sender key is rejected before randomness" true
    (is_error
       (Backup.encrypt_room_key ~random:(fixed_random "") encryption_key
          ~session_key ~sender_key:"not a Curve25519 key"))

let test_backup_recovery_key () =
  let key = backup_key () in
  let printed = Backup.Recovery_key.encode key in
  let back = ok_crypto (Backup.Recovery_key.decode printed) in
  check_string "private"
    (Backup.Decryption_key.to_base64 key)
    (Backup.Decryption_key.to_base64 back);
  check_string "public"
    (Ck.Curve25519.Public.to_base64 (Backup.Decryption_key.public key))
    (Ck.Curve25519.Public.to_base64 (Backup.Decryption_key.public back));
  (* The public half is recomputed, not carried, so a corrupted key is
     caught by the parity byte first. *)
  let b = Bytes.of_string printed in
  Bytes.set b 0 (Bytes.get b 1);
  check_bool "corrupted" true
    (is_error (Backup.Recovery_key.decode (Bytes.to_string b)))

let test_backup_auth_data_signature () =
  let random = fixed_random (String.make 64 '\x44') in
  let signing_priv, signing_pub =
    Matrix_client.Crypto_key.Ed25519.generate ~random ()
  in
  let key =
    Backup.Decryption_key.public (Backup.Decryption_key.generate ~random)
  in
  let user_id = uid "@alice:example.org" in
  let device_key_id = key_id "ed25519:TESTDEVICE" in
  let auth : Backup.megolm_v1_auth_data =
    { public_key = key; signatures = [] }
  in
  check_int "unsigned" 0 (List.length auth.signatures);
  let signed =
    Backup.sign_auth_data ~signing_key:signing_priv ~user_id
      ~key_id:device_key_id auth
  in
  check_string "state" "valid"
    (match
       Backup.verify_auth_data_signature ~verify_key:signing_pub signed ~user_id
         ~key_id:device_key_id
     with
    | Backup.Valid_but_not_trusted -> "valid"
    | Backup.Missing -> "missing"
    | Backup.Invalid -> "invalid"
    | Backup.Valid_and_trusted -> "trusted");
  (* A signature over a different public key must not verify. *)
  let tampered =
    {
      signed with
      Backup.public_key =
        Backup.Decryption_key.public
          (Backup.Decryption_key.generate
             ~random:(fixed_random (String.make 64 '\x55')));
    }
  in
  check_string "tampered" "invalid"
    (match
       Backup.verify_auth_data_signature ~verify_key:signing_pub tampered
         ~user_id ~key_id:device_key_id
     with
    | Backup.Invalid -> "invalid"
    | Backup.Missing -> "missing"
    | _ -> "valid");
  (* Signing again under the same key id replaces rather than duplicates. *)
  let twice =
    Backup.sign_auth_data ~signing_key:signing_priv ~user_id
      ~key_id:device_key_id signed
  in
  check_int "one user" 1 (List.length twice.signatures);
  check_int "one signature" 1
    (List.length (List.assoc user_id twice.signatures))

(* {1 11. Dehydrated devices} *)

let dehydrated_url =
  "https://hs.example/_matrix/client/unstable/org.matrix.msc3814.v1/dehydrated_device"

let test_dehydrated_get () =
  let log, fetch =
    mock (json {|{"device_id":"DEHYDRATED","device_data":{"algorithm":"x"}}|})
  in
  let t = client_of fetch in
  let d = ok_value (Dehydrated_device.get t) in
  check_string "device id" "DEHYDRATED" (Id.Device_id.to_string d.device_id);
  let r = one_request log in
  check_string "method" "GET" r.meth;
  check_string "url" dehydrated_url r.url

let pickle_store_harness initial =
  let current = ref initial in
  let log, fetch =
    mock (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        if String.ends_with ~suffix:"m.secret_storage.default_key" url then
          json {|{"key":"abcd"}|} req
        else if String.ends_with ~suffix:"m.secret_storage.key.abcd" url then
          json vector_description_json req
        else if String.equal (Http.Method.to_string req.meth) "GET" then
          match !current with
          | Some body -> json body req
          | None -> json_status ~status:404 {|{"errcode":"M_NOT_FOUND"}|} req
        else
          match body_of_request req with
          | Some body ->
              current := Some body;
              json "{}" req
          | None -> Alcotest.fail "pickle-key PUT had no body")
  in
  (log, fetch, current)

let open_pickle_store fetch =
  ok_value
    (Secrets.open_secret_store (client_of fetch) ~credential:vector_passphrase)

let test_pickle_key_codec () =
  let ok_pickle = function
    | Ok key -> key
    | Error (`Msg message) -> Alcotest.failf "invalid pickle key: %s" message
  in
  let key =
    Dehydrated_device.Pickle_key.generate
      ~random:(fixed_random (String.make 64 '\x7a'))
  in
  let unpadded = Dehydrated_device.Pickle_key.to_base64 key in
  let padded = Base64.encode_string ~pad:true (String.make 32 '\x7a') in
  check_int "pickle key bytes" 32
    (String.length
       (Option.get (Result.to_option (Matrix_proto.Base64.decode unpadded))));
  check_int "unpadded length" 43 (String.length unpadded);
  check_string "padded round trip" unpadded
    (Dehydrated_device.Pickle_key.to_base64
       (ok_pickle (Dehydrated_device.Pickle_key.of_base64 padded)));
  check_string "unpadded round trip" unpadded
    (Dehydrated_device.Pickle_key.to_base64
       (ok_pickle (Dehydrated_device.Pickle_key.of_base64 unpadded)));
  List.iter
    (fun value ->
      check_bool "invalid pickle key rejected" true
        (match Dehydrated_device.Pickle_key.of_base64 value with
        | Error _ -> true
        | Ok _ -> false))
    [ ""; "AAAA"; "not base64!" ]

let test_pickle_key_presence_reset_and_malformed () =
  let log, fetch, _ = pickle_store_harness None in
  let store = open_pickle_store fetch in
  check_string "pickle secret name" "org.matrix.msc3814"
    Dehydrated_device.pickle_key_secret_name;
  check_bool "absent key is not stored" false
    (ok_value (Dehydrated_device.is_key_stored store));
  check_bool "absent key loads as None" true
    (Option.is_none (ok_value (Dehydrated_device.load_key store)));
  let key =
    ok_value
      (Dehydrated_device.reset_key store
         ~random:(fixed_random (String.make 64 '\x7b')))
  in
  check_int "reset key is 32 bytes" 32
    (String.length
       (Option.get
          (Result.to_option
             (Matrix_proto.Base64.decode
                (Dehydrated_device.Pickle_key.to_base64 key)))));
  (match requests log with
  | [ default; description; initial_presence; initial_load; get; put ] ->
      check_string "initial presence GET" "GET" initial_presence.meth;
      check_string "initial load GET" "GET" initial_load.meth;
      check_string "reset starts with GET" "GET" get.meth;
      check_string "reset follows with PUT" "PUT" put.meth;
      check_string "reset GET URL"
        (account_data_url "org.matrix.msc3814")
        get.url;
      check_string "reset PUT URL"
        (account_data_url "org.matrix.msc3814")
        put.url;
      check_string "open default URL"
        (account_data_url "m.secret_storage.default_key")
        default.url;
      check_string "open description URL"
        (account_data_url "m.secret_storage.key.abcd")
        description.url
  | rs ->
      Alcotest.failf "expected open, reset, presence, load requests; got %d"
        (List.length rs));
  check_bool "reset key is present" true
    (ok_value (Dehydrated_device.is_key_stored store));
  check_string "reset key round trip"
    (Dehydrated_device.Pickle_key.to_base64 key)
    (Dehydrated_device.Pickle_key.to_base64
       (Option.get (ok_value (Dehydrated_device.load_key store))));
  check_int "presence and load each make one GET" 8 (List.length (requests log));

  let malformed =
    secret_event_json ~key_id:"abcd" ~key:(vector_key ())
      ~random:(fixed_random (String.make 65536 '\x7c'))
      ~name:Dehydrated_device.pickle_key_secret_name "not-a-key"
  in
  let _, malformed_fetch, _ = pickle_store_harness (Some malformed) in
  let malformed_store = open_pickle_store malformed_fetch in
  check_bool "malformed stored value is still present" true
    (ok_value (Dehydrated_device.is_key_stored malformed_store));
  check_bool "malformed stored value fails strict load" true
    (is_error (Dehydrated_device.load_key malformed_store))

let test_driver_pickle_cache_order_and_create () =
  let key =
    Dehydrated_device.Pickle_key.generate
      ~random:(fixed_random (String.make 64 '\x7d'))
  in
  let log, fetch, _ = pickle_store_harness None in
  let store = open_pickle_store fetch in
  let driver =
    Encryption_driver.v
      (Encryption.create
         ~random:(fixed_random (String.make 65536 '\x7e'))
         ~user_id:test_session.user_id ~device_id:(did "CACHE") ())
  in
  Encryption.set_dehydrated_pickle_key
    (Encryption_driver.machine driver)
    (Dehydrated_device.Pickle_key.to_base64 key);
  let before = List.length (requests log) in
  check_string "cache-first key"
    (Dehydrated_device.Pickle_key.to_base64 key)
    (Dehydrated_device.Pickle_key.to_base64
       (Option.get (ok_value (Dehydrated_device.cached_key driver))));
  check_string "cache-first load"
    (Dehydrated_device.Pickle_key.to_base64 key)
    (Dehydrated_device.Pickle_key.to_base64
       (Option.get
          (ok_value (Dehydrated_device.load_key_with_driver driver store))));
  check_int "cache-first makes no SSSS request" before
    (List.length (requests log));

  let absent_log, absent_fetch, _ = pickle_store_harness None in
  let absent_store = open_pickle_store absent_fetch in
  let absent_driver =
    Encryption_driver.v
      (Encryption.create
         ~random:(fixed_random (String.make 65536 '\x7f'))
         ~user_id:test_session.user_id ~device_id:(did "ABSENT") ())
  in
  check_bool "false create flag preserves absence" true
    (Option.is_none
       (ok_value
          (Dehydrated_device.load_key_with_driver ~create_if_missing:false
             absent_driver absent_store)));
  check_int "false create flag makes one SSSS GET" 3
    (List.length (requests absent_log));

  let fetched_log, fetched_fetch, _ =
    pickle_store_harness
      (Some
         (secret_event_json ~key_id:"abcd" ~key:(vector_key ())
            ~random:(fixed_random (String.make 65536 '\x7f'))
            ~name:Dehydrated_device.pickle_key_secret_name
            (Dehydrated_device.Pickle_key.to_base64 key)))
  in
  let fetched_store = open_pickle_store fetched_fetch in
  let fetched_driver =
    Encryption_driver.v
      (Encryption.create
         ~random:(fixed_random (String.make 65536 '\x80'))
         ~user_id:test_session.user_id ~device_id:(did "FETCH") ())
  in
  let loaded =
    ok_value
      (Dehydrated_device.load_key_with_driver fetched_driver fetched_store)
  in
  check_bool "fetched key returned" true (Option.is_some loaded);
  check_string "fetched key cached"
    (Dehydrated_device.Pickle_key.to_base64 key)
    (Option.get
       (Encryption.dehydrated_pickle_key
          (Encryption_driver.machine fetched_driver)));
  check_int "fetched load has one SSSS GET" 3
    (List.length (requests fetched_log));
  let create_log, create_fetch, _ = pickle_store_harness None in
  let create_store = open_pickle_store create_fetch in
  let create_driver =
    Encryption_driver.v
      (Encryption.create
         ~random:(fixed_random (String.make 65536 '\x81'))
         ~user_id:test_session.user_id ~device_id:(did "CREATE") ())
  in
  let created =
    ok_value
      (Dehydrated_device.load_key_with_driver ~create_if_missing:true
         ~random:(fixed_random (String.make 64 '\x82'))
         create_driver create_store)
  in
  check_bool "create-if-missing returns a key" true (Option.is_some created);
  check_int "create-if-missing is GET then PUT" 5
    (List.length (requests create_log))

let test_driver_pickle_reset_and_put_remember () =
  let log, fetch, _ = pickle_store_harness None in
  let store = open_pickle_store fetch in
  let driver =
    Encryption_driver.v
      (Encryption.create
         ~random:(fixed_random (String.make 65536 '\x83'))
         ~user_id:test_session.user_id ~device_id:(did "RESET") ())
  in
  let key =
    ok_value
      (Dehydrated_device.reset_key_with_driver driver store
         ~random:(fixed_random (String.make 64 '\x84')))
  in
  check_string "reset caches locally"
    (Dehydrated_device.Pickle_key.to_base64 key)
    (Option.get
       (Encryption.dehydrated_pickle_key (Encryption_driver.machine driver)));
  (match List.rev (requests log) with
  | put :: get :: _ ->
      check_string "reset SSSS GET" "GET" get.meth;
      check_string "reset SSSS PUT" "PUT" put.meth
  | _ -> Alcotest.fail "reset did not make GET then PUT");

  let put_log, put_fetch = mock (json {|{"device_id":"DEHYDRATED"}|}) in
  let put_driver =
    Encryption_driver.v
      (Encryption.create
         ~random:(fixed_random (String.make 65536 '\x85'))
         ~user_id:test_session.user_id ~device_id:(did "PUT") ())
  in
  let uploaded =
    ok_value
      (Dehydrated_device.put_and_remember put_driver (client_of put_fetch)
         ~device_id:(did "DEHYDRATED") ~device_data:(content []) ())
  in
  check_string "remembered uploaded id" "DEHYDRATED"
    (Id.Device_id.to_string
       (Option.get
          (Encryption.last_uploaded_device_id
             (Encryption_driver.machine put_driver))));
  check_string "put result" "DEHYDRATED" (Id.Device_id.to_string uploaded);
  check_string "remember PUT method" "PUT" (one_request put_log).meth;

  let fail_log, fail_fetch =
    mock (json_status ~status:500 {|{"errcode":"M_UNKNOWN","error":"down"}|})
  in
  let fail_driver =
    Encryption_driver.v
      (Encryption.create
         ~random:(fixed_random (String.make 65536 '\x86'))
         ~user_id:test_session.user_id ~device_id:(did "PUTFAIL") ())
  in
  check_bool "failed upload is returned" true
    (is_error
       (Dehydrated_device.put_and_remember fail_driver (client_of fail_fetch)
          ~device_id:(did "DEHYDRATED") ~device_data:(content []) ()));
  check_bool "failed upload is not remembered" true
    (Option.is_none
       (Encryption.last_uploaded_device_id
          (Encryption_driver.machine fail_driver)));
  check_string "failed upload attempted once" "PUT" (one_request fail_log).meth

let test_dehydrated_cached_fields_restart () =
  Eio_main.run @@ fun env ->
  let base = Filename.temp_file "matrix-dehydrated-cache-" "" in
  Sys.remove base;
  Unix.mkdir base 0o700;
  Unix.putenv "XDG_DATA_HOME" (Filename.concat base "data");
  Unix.putenv "XDG_CONFIG_HOME" (Filename.concat base "config");
  Unix.putenv "XDG_CACHE_HOME" (Filename.concat base "cache");
  Unix.putenv "XDG_STATE_HOME" (Filename.concat base "state");
  let runtime = Filename.concat base "runtime" in
  Unix.mkdir runtime 0o700;
  Unix.putenv "XDG_RUNTIME_DIR" runtime;
  let xdg = Xdge.create (Eio.Stdenv.fs env) "matrix-dehydrated-cache" in
  let store = Crypto_store.create ~xdg ~profile:"default" in
  let machine =
    Encryption.create
      ~random:(fixed_random (String.make 65536 '\x87'))
      ~user_id:test_session.user_id ~device_id:(did "RESTART") ()
  in
  let driver = Encryption_driver.v ~store machine in
  let key =
    Dehydrated_device.Pickle_key.generate
      ~random:(fixed_random (String.make 64 '\x88'))
  in
  Encryption.set_dehydrated_pickle_key machine
    (Dehydrated_device.Pickle_key.to_base64 key);
  Encryption.set_last_uploaded_device_id machine (did "DEHYDRATED");
  ok_value (Encryption_driver.save driver);
  let restored =
    ok_value
      (Encryption_driver.create
         ~random:(fixed_random (String.make 65536 '\x89'))
         ~user_id:test_session.user_id ~device_id:(did "RESTART") ~store ())
  in
  check_string "cached key survives restart"
    (Dehydrated_device.Pickle_key.to_base64 key)
    (Option.get
       (Encryption.dehydrated_pickle_key (Encryption_driver.machine restored)));
  check_string "uploaded device id survives restart" "DEHYDRATED"
    (Id.Device_id.to_string
       (Option.get
          (Encryption.last_uploaded_device_id
             (Encryption_driver.machine restored))));
  let state_path =
    Eio.Path.(Xdge.data_dir xdg / "profiles" / "default" / "crypto_state.json")
  in
  let legacy =
    match
      Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json
        (Eio.Path.load state_path)
    with
    | Error error -> Alcotest.failf "decode dehydrated state: %s" error
    | Ok (Jsont.Object (members, meta)) ->
        Jsont.Object
          ( List.filter
              (fun ((name, _), _) ->
                not
                  (String.equal name "dehydrated_pickle_key"
                  || String.equal name "last_uploaded_device_id"))
              members,
            meta )
    | Ok _ -> Alcotest.fail "stored dehydrated state is not an object"
  in
  let legacy =
    match
      Jsont_bytesrw.encode_string ~format:Jsont.Indent
        Matrix_proto.Json.Codec.json legacy
    with
    | Ok encoded -> encoded
    | Error error -> Alcotest.failf "encode legacy dehydrated state: %s" error
  in
  Eio.Path.save ~create:(`Or_truncate 0o600) state_path legacy;
  let migrated =
    ok_value
      (Encryption_driver.create
         ~random:(fixed_random (String.make 65536 '\x8a'))
         ~user_id:test_session.user_id ~device_id:(did "RESTART")
         ~store:(Crypto_store.create ~xdg ~profile:"default")
         ())
  in
  check_bool "legacy cache defaults to absent" true
    (Option.is_none
       (Encryption.dehydrated_pickle_key (Encryption_driver.machine migrated)));
  check_bool "legacy uploaded id defaults to absent" true
    (Option.is_none
       (Encryption.last_uploaded_device_id (Encryption_driver.machine migrated)))

let test_dehydrated_get_if_present () =
  let log, fetch =
    mock (json {|{"device_id":"DEHYDRATED","device_data":{}}|})
  in
  let device = ok_value (Dehydrated_device.get_if_present (client_of fetch)) in
  check_bool "successful GET is present" true (Option.is_some device);
  check_int "successful GET is single request" 1 (List.length (requests log));
  List.iter
    (fun errcode ->
      let log, fetch =
        mock
          (json_status ~status:404
             (Printf.sprintf {|{"errcode":"%s","error":"absent"}|} errcode))
      in
      check_bool
        (errcode ^ " means absent")
        true
        (Option.is_none
           (ok_value (Dehydrated_device.get_if_present (client_of fetch))));
      check_int (errcode ^ " is single request") 1 (List.length (requests log)))
    [ "M_NOT_FOUND"; "M_UNRECOGNIZED" ];
  let _, fetch =
    mock (json_status ~status:500 {|{"errcode":"M_UNKNOWN","error":"down"}|})
  in
  check_bool "other GET failures propagate" true
    (is_error (Dehydrated_device.get_if_present (client_of fetch)))

let test_dehydrated_put () =
  let log, fetch = mock (json {|{"device_id":"DEHYDRATED"}|}) in
  let t = client_of fetch in
  let device_data =
    content [ ("algorithm", jstring "org.matrix.msc3814.v2") ]
  in
  check_string "device id" "DEHYDRATED"
    (Id.Device_id.to_string
       (ok_value
          (Dehydrated_device.put t ~device_id:(did "DEHYDRATED")
             ~initial_device_display_name:"dehydrated" ~device_data
             ~one_time_keys:[ ("signed_curve25519:AAA", content []) ]
             ())));
  let r = one_request log in
  check_string "method" "PUT" r.meth;
  check_string "url" dehydrated_url r.url;
  check_str_opt "body"
    (Some
       ({|{"device_id":"DEHYDRATED","initial_device_display_name":"dehydrated",|}
      ^ {|"device_data":{"algorithm":"org.matrix.msc3814.v2"},|}
      ^ {|"one_time_keys":{"signed_curve25519:AAA":{}}}|}))
    r.body

(* Neither [one_time_keys] nor [fallback_keys] must appear in the body when
   the caller leaves both at their default, which the .mli documents as
   "the server already holds them, leave them alone." An empty object for
   either would instead tell the server it now holds none. *)
let test_dehydrated_put_omits_absent_keys () =
  let log, fetch = mock (json {|{"device_id":"DEHYDRATED"}|}) in
  let t = client_of fetch in
  let device_data =
    content [ ("algorithm", jstring "org.matrix.msc3814.v2") ]
  in
  ignore
    (ok_value
       (Dehydrated_device.put t ~device_id:(did "DEHYDRATED") ~device_data ()));
  let r = one_request log in
  let body = Option.value r.body ~default:"" in
  check_bool "one_time_keys is omitted" false (contains body "one_time_keys");
  check_bool "fallback_keys is omitted" false (contains body "fallback_keys")

let test_dehydrated_create_and_upload () =
  let log, fetch = mock (json {|{"device_id":"SERVER_DEHYDRATED"}|}) in
  let client = client_of fetch in
  let user_id = test_session.user_id in
  let random = fixed_random (String.make 65536 '\x44') in
  let driver =
    ok_value
      (Encryption_driver.create ~random ~user_id
         ~device_id:test_session.device_id ())
  in
  let identity = Cross_signing.create_private_identity ~user_id in
  Cross_signing.generate_private_keys ~random identity;
  let pickle_key = Dehydrated_device.Pickle_key.generate ~random in
  let uploaded =
    ok_value
      (Dehydrated_device.create_and_upload driver client
         ~private_identity:identity ~pickle_key ~random ())
  in
  check_string "server device id" "SERVER_DEHYDRATED"
    (Id.Device_id.to_string uploaded);
  let r = one_request log in
  let body = Option.get r.body in
  let member name = function
    | Jsont.Object (members, _) -> (
        match
          List.find_opt
            (fun ((member_name, _), _) -> String.equal member_name name)
            members
        with
        | Some (_, value) -> value
        | None -> Alcotest.failf "request has no %S member" name)
    | _ -> Alcotest.fail "request body is not an object"
  in
  let decoded =
    match Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json body with
    | Ok json -> json
    | Error error -> Alcotest.failf "invalid request JSON: %s" error
  in
  let decode codec json =
    match Jsont.Json.decode codec json with
    | Ok value -> value
    | Error error -> Alcotest.failf "invalid request member: %s" error
  in
  let device_keys =
    decode Keys.device_keys_jsont (member "device_keys" decoded)
  in
  let device_id = device_keys.device_id in
  let curve_key_id =
    Crypto_key.Key_id.of_device ~algorithm:"curve25519" device_id
  in
  let curve_key = List.assoc curve_key_id device_keys.keys in
  check_string "device id is Curve25519 identity key"
    (Id.Device_id.to_string device_id)
    curve_key;
  check_string "algorithms" "m.olm.v1.curve25519-aes-sha2,m.megolm.v1.aes-sha2"
    (String.concat "," device_keys.algorithms);
  check_bool "dehydrated marker" true (device_keys.dehydrated = Some true);
  let canonical_device_keys (keys : Keys.device_keys) =
    let jmem key value = Jsont.Json.mem (Jsont.Json.name key) value in
    let fields =
      [
        jmem "algorithms"
          (Jsont.Json.list (List.map Jsont.Json.string keys.algorithms));
        jmem "device_id"
          (Jsont.Json.string (Id.Device_id.to_string keys.device_id));
        jmem "keys"
          (Jsont.Json.object'
             (List.map
                (fun (key, value) ->
                  jmem
                    (Crypto_key.Key_id.to_string key)
                    (Jsont.Json.string value))
                keys.keys));
        jmem "user_id" (Jsont.Json.string (Id.User_id.to_string keys.user_id));
      ]
    in
    Matrix_proto.Signed_json.canonical_json
      (Jsont.Json.object' (jmem "dehydrated" (Jsont.Json.bool true) :: fields))
  in
  let own_key_id = Crypto_key.Key_id.of_device ~algorithm:"ed25519" device_id in
  let own_signature =
    List.assoc device_keys.user_id device_keys.signatures
    |> List.assoc own_key_id
  in
  let own_public =
    Crypto_key.Ed25519.Public.of_base64 (List.assoc own_key_id device_keys.keys)
    |> ok_crypto
  in
  check_bool "account device signature verifies" true
    (Crypto_key.Ed25519.Public.verify own_public ~signature:own_signature
       ~data:(canonical_device_keys device_keys));
  let self_signing = Option.get (Cross_signing.self_signing_secret identity) in
  let self_public = Crypto_key.Ed25519.Private.public self_signing in
  let self_key : Keys.cross_signing_key =
    {
      user_id;
      usage = [ Keys.Self_signing ];
      keys =
        [
          ( Crypto_key.Key_id.v ~algorithm:"ed25519"
              ~id:(Crypto_key.Ed25519.Public.to_base64 self_public),
            Crypto_key.Ed25519.Public.to_base64 self_public );
        ];
      signatures = [];
    }
  in
  check_bool "self-signing device signature verifies" true
    (Cross_signing.verify_device_signature
       ~self_signing_key:(Cross_signing.key ~role:Keys.Self_signing self_key)
       ~device:(Cross_signing.create_device device_keys));
  let one_time_keys =
    decode
      (Keys.key_id_map Keys.one_time_key_jsont)
      (member "one_time_keys" decoded)
  in
  let fallback_keys =
    decode
      (Keys.key_id_map Keys.one_time_key_jsont)
      (member "fallback_keys" decoded)
  in
  check_int "50 one-time keys" 50 (List.length one_time_keys);
  check_int "one fallback key" 1 (List.length fallback_keys);
  let verify_key expected_fallback (_, (key : Keys.one_time_key)) =
    check_bool "fallback marker" true (expected_fallback = key.fallback);
    let signature =
      List.assoc device_keys.user_id (Option.get key.signatures)
      |> List.assoc own_key_id
    in
    check_bool "one-time signature verifies" true
      (Crypto_key.Ed25519.Public.verify own_public ~signature
         ~data:(Keys.one_time_key_signing_json ?fallback:key.fallback key.key))
  in
  List.iter (verify_key None) one_time_keys;
  List.iter (verify_key (Some true)) fallback_keys;
  let device_data =
    decode Matrix_proto.Json.Codec.json (member "device_data" decoded)
  in
  check_string "legacy algorithm" "org.matrix.msc3814.v1.olm"
    (decode Matrix_proto.Json.Codec.string (member "algorithm" device_data));
  check_string "default display name" "Dehydrated device"
    (decode Matrix_proto.Json.Codec.string
       (member "initial_device_display_name" decoded));
  let device_pickle =
    decode Matrix_proto.Json.Codec.string (member "device_pickle" device_data)
  in
  let raw_pickle_key =
    Matrix_proto.Base64.decode
      (Dehydrated_device.Pickle_key.to_base64 pickle_key)
    |> ok_crypto
  in
  let restored =
    ok_crypto
      (Matrix_client.Olm_dehydrated_pickle.unpickle ~device_id
         ~pickle_key:raw_pickle_key device_pickle)
  in
  let restored_ed, restored_curve =
    Matrix_client.Olm_account.identity_keys restored.account
  in
  check_string "pickled Ed25519 identity"
    (Crypto_key.Ed25519.Public.to_base64 own_public)
    (Crypto_key.Ed25519.Public.to_base64 restored_ed);
  check_string "pickled Curve25519 identity" curve_key
    (Crypto_key.Curve25519.Public.to_base64 restored_curve);
  check_string "primary device unchanged"
    (Id.Device_id.to_string test_session.device_id)
    (Id.Device_id.to_string
       (Encryption.device_id (Encryption_driver.machine driver)))

let test_dehydrated_create_explicit_display_name () =
  let log, fetch = mock (json {|{"device_id":"SERVER_DEHYDRATED"}|}) in
  let client = client_of fetch in
  let user_id = test_session.user_id in
  let random = fixed_random (String.make 65536 '\x56') in
  let driver =
    ok_value
      (Encryption_driver.create ~random ~user_id
         ~device_id:test_session.device_id ())
  in
  let identity = Cross_signing.create_private_identity ~user_id in
  Cross_signing.generate_private_keys ~random identity;
  let pickle_key = Dehydrated_device.Pickle_key.generate ~random in
  ignore
    (ok_value
       (Dehydrated_device.create_and_upload driver client
          ~private_identity:identity ~pickle_key
          ~initial_device_display_name:"my offline device" ~random ()));
  let request = one_request log in
  let body = Option.get request.body in
  let json =
    match Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json body with
    | Ok json -> json
    | Error error -> Alcotest.failf "invalid request JSON: %s" error
  in
  let display =
    match json with
    | Jsont.Object (members, _) ->
        Option.get
          (List.find_opt
             (fun ((name, _), _) ->
               String.equal name "initial_device_display_name")
             members)
        |> snd
    | _ -> Alcotest.fail "request body is not an object"
  in
  check_string "explicit display name" "my offline device"
    (match Jsont.Json.decode Matrix_proto.Json.Codec.string display with
    | Ok value -> value
    | Error error -> Alcotest.failf "invalid display name: %s" error)

let test_dehydrated_create_preflight () =
  let log, fetch = mock (json {|{"device_id":"SHOULD_NOT_EXIST"}|}) in
  let client = client_of fetch in
  let user_id = test_session.user_id in
  let random = fixed_random (String.make 65536 '\x55') in
  let driver =
    ok_value
      (Encryption_driver.create ~random ~user_id
         ~device_id:test_session.device_id ())
  in
  let key = Dehydrated_device.Pickle_key.generate ~random in
  let other =
    Cross_signing.create_private_identity ~user_id:(uid "@bob:example.org")
  in
  check_bool "mismatched identity is rejected" true
    (is_error
       (Dehydrated_device.create_and_upload driver client
          ~private_identity:other ~pickle_key:key ~random ()));
  let missing = Cross_signing.create_private_identity ~user_id in
  check_bool "missing self-signing key is rejected" true
    (is_error
       (Dehydrated_device.create_and_upload driver client
          ~private_identity:missing ~pickle_key:key ~random ()));
  check_int "preflight makes no request" 0 (List.length (requests log))

let test_dehydrated_delete () =
  let log, fetch = mock (json {|{"device_id":"DEHYDRATED"}|}) in
  let t = client_of fetch in
  ok_value (Dehydrated_device.delete t);
  let r = one_request log in
  check_string "method" "DELETE" r.meth;
  check_string "url" dehydrated_url r.url

let test_dehydrated_delete_if_present () =
  let log, fetch = mock (json {|{"device_id":"DEHYDRATED"}|}) in
  ok_value (Dehydrated_device.delete_if_present (client_of fetch));
  let r = one_request log in
  check_string "successful DELETE method" "DELETE" r.meth;
  check_string "successful DELETE path" dehydrated_url r.url;
  List.iter
    (fun errcode ->
      let log, fetch =
        mock
          (json_status ~status:404
             (Printf.sprintf {|{"errcode":"%s","error":"absent"}|} errcode))
      in
      ok_value (Dehydrated_device.delete_if_present (client_of fetch));
      check_int
        (errcode ^ " DELETE is single request")
        1
        (List.length (requests log)))
    [ "M_NOT_FOUND"; "M_UNRECOGNIZED" ];
  let _, fetch =
    mock (json_status ~status:500 {|{"errcode":"M_UNKNOWN","error":"down"}|})
  in
  check_bool "other DELETE failures propagate" true
    (is_error (Dehydrated_device.delete_if_present (client_of fetch)))

let test_dehydrated_events () =
  let log, fetch =
    mock (json {|{"next_batch":"tok2","events":[{"type":"m.room.encrypted"}]}|})
  in
  let t = client_of fetch in
  let e =
    ok_value
      (Dehydrated_device.get_events t ~device_id:(did "DEHYDRATED") ~from:"tok1"
         ())
  in
  check_str_opt "next batch" (Some "tok2") e.next_batch;
  check_int "events" 1 (List.length e.events);
  let r = one_request log in
  check_string "method" "POST" r.meth;
  check_string "url" (dehydrated_url ^ "/DEHYDRATED/events") r.url;
  check_str_opt "body" (Some {|{"next_batch":"tok1"}|}) r.body

(* Build a real legacy V1 account and its server representation.  Keeping
   this here lets the drain tests exercise pickle authentication and Olm
   decryption without depending on a homeserver implementation. *)
type legacy_fixture = {
  device_id : Id.Device_id.t;
  device_data : Jsont.json;
  account : Olm.Account.t;
  pickle_key : Dehydrated_device.Pickle_key.t;
}

let rehydrate_room = rid "!rehydrated:example.org"

let legacy_fixture ~seed ~pickle_key () =
  let random = fixed_random (String.make 65536 seed.[0]) in
  let account = Olm.Account.create ~random () in
  Olm.Account.generate_one_time_keys ~random account 1;
  let device_id =
    Result.get_ok
      (Id.Device_id.of_string
         (Crypto_key.Curve25519.Public.to_base64
            (Olm.Account.curve25519_key account)))
  in
  let raw_key =
    ok_crypto
      (Matrix_proto.Base64.decode
         (Dehydrated_device.Pickle_key.to_base64 pickle_key))
  in
  let pickle =
    ok_crypto
      (Matrix_client.Olm_dehydrated_pickle.pickle ~device_id ~pickle_key:raw_key
         account)
  in
  let device_data =
    content
      [
        ("algorithm", jstring "org.matrix.msc3814.v1.olm");
        ("device_pickle", jstring pickle);
      ]
  in
  { device_id; device_data; account; pickle_key }

let legacy_response fixture =
  json_string
    (content
       [
         ("device_id", jstring (Id.Device_id.to_string fixture.device_id));
         ("device_data", fixture.device_data);
       ])

let encrypted_room_key_fixture fixture =
  let sender_user = uid "@bob:example.org" in
  let sender_machine =
    Olm.Machine.create ~random:(fixed_random (String.make 65536 '\xa1')) ()
  in
  let sender_account = Olm.Machine.account sender_machine in
  let sender_ed, sender_curve = Olm.Account.identity_keys sender_account in
  let target_ed, target_curve = Olm.Account.identity_keys fixture.account in
  let target_otk =
    match Olm.Account.one_time_keys fixture.account with
    | (_, key) :: _ -> key
    | [] -> Alcotest.fail "legacy fixture has no one-time key"
  in
  let outbound =
    Olm.Megolm.Outbound.create
      ~random:(fixed_random (String.make 65536 '\xa2'))
      ~room_id:rehydrate_room ()
  in
  let room_key_content =
    content
      [
        ("algorithm", jstring "m.megolm.v1.aes-sha2");
        ("room_id", jstring (Id.Room_id.to_string rehydrate_room));
        ( "session_id",
          jstring
            (Id.Session_id.to_string (Olm.Megolm.Outbound.session_id outbound))
        );
        ("session_key", jstring (Olm.Megolm.Outbound.session_key outbound));
      ]
  in
  let plaintext : Ev.Olm_plaintext.t =
    {
      event_type = "m.room_key";
      content = room_key_content;
      sender = sender_user;
      sender_ed25519 = Crypto_key.Ed25519.Public.to_base64 sender_ed;
      recipient = test_session.user_id;
      recipient_ed25519 = Crypto_key.Ed25519.Public.to_base64 target_ed;
      sender_device_keys = None;
    }
  in
  let plaintext =
    Result.get_ok (Jsont_bytesrw.encode_string Ev.Olm_plaintext.jsont plaintext)
  in
  let message =
    match
      Olm.Machine.encrypt_to_device
        ~random:(fixed_random (String.make 65536 '\xa3'))
        sender_machine ~their_identity_key:target_curve
        ~their_one_time_key:target_otk ~plaintext
    with
    | Ok message -> message
    | Error error ->
        Alcotest.failf "cannot encrypt room key: %a" Olm.pp_error error
  in
  let olm_content : Ev.Encrypted.Olm.t =
    {
      sender_key = Crypto_key.Curve25519.Public.to_base64 sender_curve;
      ciphertext =
        [
          {
            Ev.Olm_ciphertext.recipient_key =
              Crypto_key.Curve25519.Public.to_base64 target_curve;
            message_type = message.message_type;
            body = message.ciphertext;
          };
        ];
    }
  in
  let to_device =
    content
      [
        ("type", jstring "m.room.encrypted");
        ("sender", jstring (Id.User_id.to_string sender_user));
        ( "content",
          Result.get_ok (Jsont.Json.encode Ev.Encrypted.Olm.jsont olm_content)
        );
      ]
  in
  let room_plaintext =
    content
      [
        ("room_id", jstring (Id.Room_id.to_string rehydrate_room));
        ("type", jstring "m.room.message");
        ("content", content [ ("body", jstring "rehydrated") ]);
      ]
  in
  let encrypted =
    Olm.Megolm.Outbound.encrypt outbound (json_string room_plaintext)
  in
  let room_event : Ev.Raw_event.t =
    {
      event_id = Some (Result.get_ok (Id.Event_id.of_string "$rehydrated"));
      sender = sender_user;
      origin_server_ts = Matrix_proto.Event.Timestamp.of_ms 0L;
      type_ = Matrix_proto.Event.Event_type.of_string "m.room.encrypted";
      state_key = None;
      redacts = None;
      content =
        content
          [
            ("algorithm", jstring "m.megolm.v1.aes-sha2");
            ( "sender_key",
              jstring (Crypto_key.Curve25519.Public.to_base64 sender_curve) );
            ( "session_id",
              jstring
                (Id.Session_id.to_string
                   (Olm.Megolm.Outbound.session_id outbound)) );
            ("ciphertext", jstring encrypted.ciphertext);
          ];
      unsigned = None;
      room_id = Some rehydrate_room;
    }
  in
  (to_device, room_event)

let events_response ?next events =
  let fields = [ ("events", Jsont.Json.list events) ] in
  let fields =
    match next with
    | None -> fields
    | Some token -> ("next_batch", jstring token) :: fields
  in
  json_string (content fields)

let rehydrate_driver ~seed () =
  ok_value
    (Encryption_driver.create
       ~random:(fixed_random (String.make 65536 seed.[0]))
       ~user_id:test_session.user_id ~device_id:test_session.device_id ())

let run_rehydrate fixture ~events_pages ?(delete_body = `Ok) () =
  let page = ref 0 in
  let log, fetch =
    mock (fun req ->
        let meth = Http.Method.to_string req.meth in
        let url = Fetch.Middleware.Url.to_string req.url in
        if meth = "GET" && String.equal url dehydrated_url then
          json (legacy_response fixture) req
        else if meth = "POST" then begin
          let response =
            match List.nth_opt events_pages !page with
            | Some body ->
                incr page;
                body
            | None -> Alcotest.fail "unexpected extra events page"
          in
          json response req
        end
        else if meth = "DELETE" then
          match delete_body with
          | `Ok -> json {|{"device_id":"deleted"}|} req
          | `Error ->
              json_status ~status:500
                {|{"errcode":"M_UNKNOWN","error":"delete failed"}|} req
        else Alcotest.failf "unexpected request %s %s" meth url)
  in
  (log, client_of fetch)

let assert_rehydrate_error_no_delete ?request_key label fixture ~events_pages
    expected_error =
  let log, client = run_rehydrate fixture ~events_pages () in
  let driver = rehydrate_driver ~seed:(label ^ "-driver") () in
  let request_key = Option.value request_key ~default:fixture.pickle_key in
  let result =
    Dehydrated_device.rehydrate driver client ~pickle_key:request_key
      ~random:(fixed_random (String.make 65536 label.[0]))
      ()
  in
  check_bool (label ^ " returns error") true
    (match result with
    | Error (Error.Json_error message) -> contains message expected_error
    | _ -> false);
  check_bool
    (label ^ " makes no DELETE")
    false
    (List.exists (fun r -> r.meth = "DELETE") (requests log))

let test_dehydrated_rehydrate_absent () =
  let log, fetch =
    mock (json_status ~status:404 {|{"errcode":"M_NOT_FOUND"}|})
  in
  let driver = rehydrate_driver ~seed:"absent-driver" () in
  check_bool "absent returns None" true
    (ok_value
       (Dehydrated_device.rehydrate driver (client_of fetch)
          ~pickle_key:
            (Dehydrated_device.Pickle_key.generate
               ~random:(fixed_random (String.make 64 '\x90')))
          ~random:(fixed_random (String.make 64 '\x91'))
          ())
    = None);
  check_bool "absent makes no POST or DELETE" true
    (List.for_all (fun r -> r.meth = "GET") (requests log))

let test_dehydrated_rehydrate_empty_and_pagination () =
  let key =
    Dehydrated_device.Pickle_key.generate
      ~random:(fixed_random (String.make 64 '\x92'))
  in
  let fixture = legacy_fixture ~seed:"empty" ~pickle_key:key () in
  let harmless = content [ ("type", jstring "m.dummy") ] in
  let pages =
    [ events_response ~next:"cursor" [ harmless ]; events_response [] ]
  in
  let log, client = run_rehydrate fixture ~events_pages:pages () in
  let driver = rehydrate_driver ~seed:"empty-driver" () in
  let primary_id = Encryption.device_id (Encryption_driver.machine driver) in
  let result =
    ok_value
      (Dehydrated_device.rehydrate driver client ~pickle_key:key
         ~random:(fixed_random (String.make 65536 '\x93'))
         ())
  in
  let result = Option.get result in
  check_int "empty drain imports no room keys" 0 result.room_keys_imported;
  check_int "paginated drain events" 1 result.to_device_events;
  check_bool "empty drain has no delete error" true
    (Option.is_none result.delete_error);
  check_string "primary identity unchanged"
    (Id.Device_id.to_string test_session.device_id)
    (Id.Device_id.to_string primary_id);
  (match requests log with
  | [ get; first; second; delete ] ->
      check_string "empty drain GET" "GET" get.meth;
      check_str_opt "first pagination body" (Some "{}") first.body;
      check_str_opt "second pagination body" (Some {|{"next_batch":"cursor"}|})
        second.body;
      check_string "terminal DELETE" "DELETE" delete.meth
  | rs ->
      Alcotest.failf "expected GET, two POSTs, DELETE; got %d" (List.length rs));

  let empty_key =
    Dehydrated_device.Pickle_key.generate
      ~random:(fixed_random (String.make 64 '\x9b'))
  in
  let empty_fixture =
    legacy_fixture ~seed:"truly-empty" ~pickle_key:empty_key ()
  in
  let empty_log, empty_client =
    run_rehydrate empty_fixture ~events_pages:[ events_response [] ] ()
  in
  let empty_driver = rehydrate_driver ~seed:"truly-empty-driver" () in
  let empty_result =
    ok_value
      (Dehydrated_device.rehydrate empty_driver empty_client
         ~pickle_key:empty_key
         ~random:(fixed_random (String.make 65536 '\x9c'))
         ())
  in
  let empty_result = Option.get empty_result in
  check_int "empty drain events" 0 empty_result.to_device_events;
  check_bool "empty drain deletes" true
    (List.exists (fun r -> r.meth = "DELETE") (requests empty_log))

let test_dehydrated_rehydrate_terminal_pages_and_cursor_errors () =
  let key =
    Dehydrated_device.Pickle_key.generate
      ~random:(fixed_random (String.make 64 '\x94'))
  in
  let fixture = legacy_fixture ~seed:"terminal" ~pickle_key:key () in
  let harmless = content [ ("type", jstring "m.dummy") ] in
  let log, client =
    run_rehydrate fixture ~events_pages:[ events_response [ harmless ] ] ()
  in
  let driver = rehydrate_driver ~seed:"terminal-driver" () in
  let result =
    ok_value
      (Dehydrated_device.rehydrate driver client ~pickle_key:key
         ~random:(fixed_random (String.make 65536 '\x95'))
         ())
  in
  let result = Option.get result in
  check_int "nonempty terminal count" 1 result.to_device_events;
  check_int "nonempty terminal imports no dummy" 0 result.room_keys_imported;
  check_bool "nonempty terminal deletes" true
    (List.exists (fun r -> r.meth = "DELETE") (requests log));

  let repeat_key =
    Dehydrated_device.Pickle_key.generate
      ~random:(fixed_random (String.make 64 '\x96'))
  in
  let repeat_fixture =
    legacy_fixture ~seed:"repeat" ~pickle_key:repeat_key ()
  in
  assert_rehydrate_error_no_delete "repeated cursor" repeat_fixture
    ~events_pages:
      [
        events_response ~next:"same" [ harmless ];
        events_response ~next:"same" [ harmless ];
      ]
    "repeated its cursor"

let test_dehydrated_rehydrate_invalid_data_and_key () =
  let key =
    Dehydrated_device.Pickle_key.generate
      ~random:(fixed_random (String.make 64 '\x97'))
  in
  let fixture = legacy_fixture ~seed:"invalid" ~pickle_key:key () in
  let invalid_data algorithm =
    let original_pickle =
      match fixture.device_data with
      | Jsont.Object (members, _) -> (
          match
            List.find_opt
              (fun ((name, _), _) -> String.equal name "device_pickle")
              members
          with
          | Some (_, value) -> value
          | None -> Alcotest.fail "fixture has no device_pickle")
      | _ -> Alcotest.fail "fixture device_data is not an object"
    in
    content
      [ ("algorithm", jstring algorithm); ("device_pickle", original_pickle) ]
  in
  List.iter
    (fun (label, data, needle) ->
      let bad = { fixture with device_data = data } in
      assert_rehydrate_error_no_delete label bad ~events_pages:[] needle)
    [
      ("unknown algorithm", invalid_data "org.matrix.msc3814.v2", "unsupported");
      ("malformed data", Jsont.Json.string "not-an-object", "object");
      ( "extra data",
        content
          [
            ("algorithm", jstring "org.matrix.msc3814.v1.olm");
            ("device_pickle", jstring "bad");
            ("extra", jstring "reject");
          ],
        "extra" );
    ];
  let wrong_key =
    Dehydrated_device.Pickle_key.generate
      ~random:(fixed_random (String.make 64 '\x98'))
  in
  assert_rehydrate_error_no_delete ~request_key:wrong_key "wrong key" fixture
    ~events_pages:[] "MAC";
  let original_pickle =
    match fixture.device_data with
    | Jsont.Object (members, _) -> (
        match
          List.find_opt
            (fun ((name, _), _) -> String.equal name "device_pickle")
            members
        with
        | Some (_, value) ->
            Result.get_ok
              (Jsont.Json.decode Matrix_proto.Json.Codec.string value)
        | None -> Alcotest.fail "fixture has no device_pickle")
    | _ -> Alcotest.fail "fixture device_data is not an object"
  in
  let tampered =
    {
      fixture with
      device_data =
        content
          [
            ("algorithm", jstring "org.matrix.msc3814.v1.olm");
            ( "device_pickle",
              jstring
                (String.init (String.length original_pickle) (fun i ->
                     if i = 0 then
                       if original_pickle.[0] = 'A' then 'B' else 'A'
                     else original_pickle.[i])) );
          ];
    }
  in
  assert_rehydrate_error_no_delete "tampered pickle" tampered ~events_pages:[]
    "MAC"

let test_dehydrated_rehydrate_event_cap () =
  let key =
    Dehydrated_device.Pickle_key.generate
      ~random:(fixed_random (String.make 64 '\x9d'))
  in
  let fixture = legacy_fixture ~seed:"cap" ~pickle_key:key () in
  let harmless = content [ ("type", jstring "m.dummy") ] in
  (* This deliberately exercises the production 100,000-event bound. *)
  let events = List.init 100_000 (fun _ -> harmless) in
  assert_rehydrate_error_no_delete "event cap" fixture
    ~events_pages:[ events_response events ]
    "event limit exceeded"

let test_dehydrated_rehydrate_real_room_key () =
  let key =
    Dehydrated_device.Pickle_key.generate
      ~random:(fixed_random (String.make 64 '\x9e'))
  in
  let fixture = legacy_fixture ~seed:"real-room-key" ~pickle_key:key () in
  let to_device, room_event = encrypted_room_key_fixture fixture in
  let log, client =
    run_rehydrate fixture ~events_pages:[ events_response [ to_device ] ] ()
  in
  let driver = rehydrate_driver ~seed:"real-room-key-driver" () in
  let primary = Encryption_driver.machine driver in
  let primary_id = Encryption.device_id primary in
  let result =
    ok_value
      (Dehydrated_device.rehydrate driver client ~pickle_key:key
         ~random:(fixed_random (String.make 65536 '\x9f'))
         ())
  in
  let result = Option.get result in
  check_int "real room key imported" 1 result.room_keys_imported;
  check_int "real Olm event counted" 1 result.to_device_events;
  check_bool "real room-key page deleted" true
    (List.exists (fun r -> r.meth = "DELETE") (requests log));
  check_string "real import preserves primary identity"
    (Id.Device_id.to_string test_session.device_id)
    (Id.Device_id.to_string primary_id);
  match Encryption.decrypt_room_event primary rehydrate_room room_event with
  | Error error ->
      Alcotest.failf "primary could not decrypt imported room event: %a"
        Encryption.pp_decrypt_error error
  | Ok decrypted ->
      let body =
        match decrypted.decrypted_content with
        | Jsont.Object (members, _) -> (
            match
              List.find_opt
                (fun ((name, _), _) -> String.equal name "body")
                members
            with
            | Some (_, value) ->
                Result.get_ok
                  (Jsont.Json.decode Matrix_proto.Json.Codec.string value)
            | None -> Alcotest.fail "decrypted event has no body")
        | _ -> Alcotest.fail "decrypted event content is not an object"
      in
      check_string "decrypted imported room event" "rehydrated" body

let test_dehydrated_rehydrate_delete_error_and_id_mismatch () =
  let key =
    Dehydrated_device.Pickle_key.generate
      ~random:(fixed_random (String.make 64 '\x99'))
  in
  let fixture = legacy_fixture ~seed:"delete" ~pickle_key:key () in
  let log, client =
    run_rehydrate fixture
      ~events_pages:[ events_response [] ]
      ~delete_body:`Error ()
  in
  let driver = rehydrate_driver ~seed:"delete-driver" () in
  Encryption.set_last_uploaded_device_id
    (Encryption_driver.machine driver)
    (did "DIFFERENT");
  let result =
    ok_value
      (Dehydrated_device.rehydrate driver client ~pickle_key:key
         ~random:(fixed_random (String.make 65536 '\x9a'))
         ())
  in
  let result = Option.get result in
  check_bool "delete error is returned after successful drain" true
    (Option.is_some result.delete_error);
  check_bool "delete attempted" true
    (List.exists (fun r -> r.meth = "DELETE") (requests log))

let test_dehydrated_lifecycle_callbacks () =
  let create_log, create_fetch =
    mock (json {|{"device_id":"SERVER_DEHYDRATED"}|})
  in
  let client = client_of create_fetch in
  let random = fixed_random (String.make 65536 '\x57') in
  let driver =
    ok_value
      (Encryption_driver.create ~random ~user_id:test_session.user_id
         ~device_id:test_session.device_id ())
  in
  let identity =
    Cross_signing.create_private_identity ~user_id:test_session.user_id
  in
  Cross_signing.generate_private_keys ~random identity;
  let key = Dehydrated_device.Pickle_key.generate ~random in
  let create_events = ref [] in
  ignore
    (ok_value
       (Dehydrated_device.create_and_upload_with_callbacks
          ~on_event:(fun event -> create_events := event :: !create_events)
          driver client ~private_identity:identity ~pickle_key:key ~random ()));
  (match List.rev !create_events with
  | [ Dehydrated_device.Created _; Dehydrated_device.Uploaded _ ] -> ()
  | _ -> Alcotest.fail "create callbacks did not bracket the upload");
  check_int "create callback request count" 1
    (List.length (requests create_log));

  let fixture = legacy_fixture ~seed:"lifecycle" ~pickle_key:key () in
  let rehydrate_log, rehydrate_client =
    run_rehydrate fixture ~events_pages:[ events_response [] ] ()
  in
  let rehydrate_driver = rehydrate_driver ~seed:"lifecycle-driver" () in
  let rehydrate_events = ref [] in
  ignore
    (ok_value
       (Dehydrated_device.rehydrate_with_callbacks
          ~on_event:(function
            | Dehydrated_device.Rehydration_started _ ->
                rehydrate_events := "started" :: !rehydrate_events
            | Dehydrated_device.Rehydration_progress _ ->
                rehydrate_events := "progress" :: !rehydrate_events
            | Dehydrated_device.Rehydration_completed _ ->
                rehydrate_events := "completed" :: !rehydrate_events)
          ~on_deleted:(fun () ->
            rehydrate_events := "deleted" :: !rehydrate_events)
          rehydrate_driver rehydrate_client ~pickle_key:key
          ~random:(fixed_random (String.make 65536 '\x58'))
          ()));
  (match List.rev !rehydrate_events with
  | [ "started"; "completed"; "deleted" ] -> ()
  | _ ->
      Alcotest.fail "rehydration callbacks did not reflect drain/delete order");
  check_bool "rehydrate callback deletion request" true
    (List.exists
       (fun request -> request.meth = "DELETE")
       (requests rehydrate_log))

(* {1 Portable room-key files} *)

let exported_key () : Room_key_export.room_key =
  {
    algorithm = "m.megolm.v1.aes-sha2";
    room_id = rid "!export:example.org";
    sender_key = "curve25519-key";
    session_id = sid "megolm-session";
    session_key = "exported-session-key";
    sender_claimed_keys = [ ("ed25519", "signing-key") ];
    forwarding_curve25519_key_chain = [ "forwarder" ];
    shared_history = true;
  }

let check_exported_key (expected : Room_key_export.room_key)
    (actual : Room_key_export.room_key) =
  check_string "algorithm" expected.algorithm actual.algorithm;
  check_string "room id"
    (Id.Room_id.to_string expected.room_id)
    (Id.Room_id.to_string actual.room_id);
  check_string "sender key" expected.sender_key actual.sender_key;
  check_string "session id"
    (Id.Session_id.to_string expected.session_id)
    (Id.Session_id.to_string actual.session_id);
  check_string "session key" expected.session_key actual.session_key;
  Alcotest.(check (list (pair string string)))
    "claimed keys" expected.sender_claimed_keys actual.sender_claimed_keys;
  Alcotest.(check (list string))
    "forwarding chain" expected.forwarding_curve25519_key_chain
    actual.forwarding_curve25519_key_chain;
  check_bool "shared history" expected.shared_history actual.shared_history

let test_room_key_export_round_trip () =
  let expected = exported_key () in
  let random_bytes = String.init 32 (fun i -> Char.chr (0x80 + i)) in
  let armour =
    ok_export
      (Room_key_export.encrypt
         ~random:(fixed_random random_bytes)
         ~passphrase:"correct horse" ~rounds:10 [ expected ])
  in
  let actual =
    match
      ok_export (Room_key_export.decrypt ~passphrase:"correct horse" armour)
    with
    | [ key ] -> key
    | keys ->
        Alcotest.failf "expected one exported key, got %d" (List.length keys)
  in
  check_exported_key expected actual;
  let payload =
    armour |> String.split_on_char '\n' |> fun lines ->
    List.nth lines 1 |> Matrix_proto.Base64.decode |> Result.get_ok
  in
  check_int "version" 1 (Char.code payload.[0]);
  check_int "IV bit 63 was cleared" 0 (Char.code payload.[1 + 16 + 8] land 0x80)

let test_room_key_export_rejects_tampering () =
  let armour =
    ok_export
      (Room_key_export.encrypt
         ~random:(fixed_random (String.make 32 '\x42'))
         ~passphrase:"right" ~rounds:10
         [ exported_key () ])
  in
  (match Room_key_export.decrypt ~passphrase:"wrong" armour with
  | Error Room_key_export.Invalid_mac -> ()
  | Error error ->
      Alcotest.failf "wrong password error: %a" Room_key_export.pp_error error
  | Ok _ -> Alcotest.fail "wrong password decrypted a key export");
  let lines = String.split_on_char '\n' armour in
  let payload =
    Matrix_proto.Base64.decode (List.nth lines 1)
    |> Result.get_ok |> Bytes.of_string
  in
  let offset = 1 + 16 + 16 + 4 in
  Bytes.set payload offset
    (Char.chr (Char.code (Bytes.get payload offset) lxor 1));
  let tampered =
    String.concat "\n"
      [
        List.hd lines;
        Matrix_proto.Base64.encode (Bytes.to_string payload);
        List.nth lines 2;
      ]
  in
  match Room_key_export.decrypt ~passphrase:"right" tampered with
  | Error Room_key_export.Invalid_mac -> ()
  | Error error ->
      Alcotest.failf "tamper error: %a" Room_key_export.pp_error error
  | Ok _ -> Alcotest.fail "tampered key export was accepted"

let rust_key_export =
  "-----BEGIN MEGOLM SESSION DATA-----\n"
  ^ "Af7mGhlzQ+eGvHu93u0YXd3D/+vYMs3E7gQqOhuCtkvGAAAAASH7pEdWvFyAP1JUisAcpEo\n"
  ^ "Xke2Q7Kr9hVl/SCc6jXBNeJCZcrUbUV4D/tRQIl3E9L4fOk928YI1J+3z96qiH0uE7hpsCI\n"
  ^ "CkHKwjPU+0XTzFdIk1X8H7sZ+MD/2Sg/q3y8rtUjz7uEj4GUTnb+9SCOTVmJsRfqgUpM1CU\n"
  ^ "bDLytHf1JkohY4tWEgpsCc67xdzgodjr12qYrfg/zNm3LGpxlrffJknw4rk5QFTj4kMbqbD\n"
  ^ "ZZgDTni+HxRTDGge2J620lMOiznvXX+H09Rwruqx5aJvvaaKd86jWRpiO2oSFqHn4u5ONl9\n"
  ^ "41uzm62Sj0eIm6ZbA9NQs87jQw4LxsejhZVL+NdjIg80zVSBTWhTdo0DTnbFSNP4ReOiz0U\n"
  ^ "XosOF8A5T8Vdx2nvA0GXltfcHKVKQYh/LJAkNQ7P9UYL4ae/5TtQZkhB1KxCLTRWqADCl53\n"
  ^ "uBMGpG53EMgY6G6K2DEIOkcv7sdXQF5WpemiSWZqJRWj+cjfs9BpCTbkp/rszWFl2TniWpR\n"
  ^ "RqIbT2jORlN4rTvdtF0F4z1pqP4qWyR3sLNTkXm9CFRzWADNG0RDZKxbCoo6RPvtaCTfaHo\n"
  ^ "SwfvzBS6CjfAG+FOugpV48o7+XetaUUPZ6/tZSPhCdeV8eP9q5r0QwWeXFogzoNzWt4HYx9\n"
  ^ "MdXxzD+f0mtg5gzehrrEEARwI2bCvPpHxlt/Na9oW/GBpkjwR1LSKgg4CtpRyWngPjdEKpZ\n"
  ^ "GYW19pdjg0qdXNk/eqZsQTsNWVo6A\n" ^ "-----END MEGOLM SESSION DATA-----"

let test_room_key_export_rust_fixture () =
  let keys =
    ok_export (Room_key_export.decrypt ~passphrase:"1234" rust_key_export)
  in
  check_bool "Rust fixture is not empty" true (keys <> []);
  let key = List.hd keys in
  check_string "Rust algorithm" "m.megolm.v1.aes-sha2" key.algorithm;
  check_bool "Rust session key" true (String.length key.session_key > 100)

let test_room_key_export_decrypt_rounds_policy () =
  let armour =
    ok_export
      (Room_key_export.encrypt
         ~random:(fixed_random (String.make 32 '\x31'))
         ~passphrase:"x" ~rounds:10
         [ exported_key () ])
  in
  (match Room_key_export.decrypt ~max_rounds:9L ~passphrase:"x" armour with
  | Error (Room_key_export.Invalid_rounds 10L) -> ()
  | Error error ->
      Alcotest.failf "wrong low-round policy error: %a" Room_key_export.pp_error
        error
  | Ok _ -> Alcotest.fail "decrypt ignored its max_rounds policy");
  match Room_key_export.decrypt ~max_rounds:10L ~passphrase:"x" armour with
  | Ok [ _ ] -> ()
  | Ok keys ->
      Alcotest.failf "accepted export returned %d keys" (List.length keys)
  | Error error ->
      Alcotest.failf "trusted low-round policy rejected its fixture: %a"
        Room_key_export.pp_error error

let test_room_key_export_validation () =
  (match Room_key_export.decrypt ~passphrase:"x" "not an export" with
  | Error Room_key_export.Invalid_headers -> ()
  | _ -> Alcotest.fail "bad headers were accepted");
  let malformed =
    "-----BEGIN MEGOLM SESSION DATA-----\nAA\n-----END MEGOLM SESSION DATA-----"
  in
  (match Room_key_export.decrypt ~passphrase:"x" malformed with
  | Error Room_key_export.Truncated -> ()
  | _ -> Alcotest.fail "a truncated export was accepted");
  (match
     Room_key_export.encrypt
       ~random:(fixed_random (String.make 32 '\x00'))
       ~passphrase:"x" ~rounds:0 []
   with
  | Error (Room_key_export.Invalid_rounds 0L) -> ()
  | _ -> Alcotest.fail "a zero PBKDF2 iteration count was accepted");
  let hostile_payload =
    String.make 1 (Char.chr 1)
    ^ String.make 16 '\000' ^ String.make 16 '\000' ^ String.make 4 '\xff'
    ^ String.make 32 '\000'
  in
  let hostile_armour =
    String.concat "\n"
      [
        "-----BEGIN MEGOLM SESSION DATA-----";
        Matrix_proto.Base64.encode hostile_payload;
        "-----END MEGOLM SESSION DATA-----";
      ]
  in
  (match Room_key_export.decrypt ~passphrase:"x" hostile_armour with
  | Error (Room_key_export.Invalid_rounds 0xffff_ffffL) -> ()
  | Error error ->
      Alcotest.failf "hostile PBKDF2 count returned %a" Room_key_export.pp_error
        error
  | Ok _ -> Alcotest.fail "hostile PBKDF2 count was accepted");
  if Sys.int_size > 32 then
    let too_many = Int64.to_int 0x1_0000_0000L in
    match
      Room_key_export.encrypt
        ~random:(fixed_random (String.make 32 '\x00'))
        ~passphrase:"x" ~rounds:too_many []
    with
    | Error (Room_key_export.Invalid_rounds 0x1_0000_0000L) -> ()
    | _ -> Alcotest.fail "an oversized PBKDF2 iteration count was accepted"

(* {1 Suite} *)

let () =
  Alcotest.run "matrix e2ee api"
    [
      ( "recovery key",
        [
          Alcotest.test_case "encode" `Quick test_encode_key;
          Alcotest.test_case "decode" `Quick test_decode_key;
          Alcotest.test_case "parity catches a transposition" `Quick
            test_decode_key_parity;
          Alcotest.test_case "header is checked" `Quick test_decode_key_header;
        ] );
      ( "secret storage keys",
        [
          Alcotest.test_case "PBKDF2 from a passphrase" `Quick
            test_key_from_passphrase;
          Alcotest.test_case "unknown KDF rejected" `Quick
            test_key_from_passphrase_rejects_unknown_kdf;
          Alcotest.test_case "recovery key round trip" `Quick
            test_recovery_key_round_trip;
          Alcotest.test_case "description check value" `Quick
            test_key_description_check_value;
          Alcotest.test_case "IV bit 63 is cleared" `Quick
            test_iv_bit_63_is_cleared;
          Alcotest.test_case "check_key" `Quick test_check_key;
        ] );
      ( "secret storage crypto",
        [
          Alcotest.test_case "encrypt matches the vector" `Quick
            test_encrypt_matches_vector;
          Alcotest.test_case "decrypt the vector" `Quick test_decrypt_vector;
          Alcotest.test_case "wrong key or name fails" `Quick
            test_decrypt_wrong_key_and_name;
          Alcotest.test_case "round trip" `Quick test_encrypt_decrypt_round_trip;
        ] );
      ( "secret storage account data",
        [
          Alcotest.test_case "default key id" `Quick
            (run test_get_default_key_id);
          Alcotest.test_case "no default key" `Quick
            (run test_get_default_key_id_absent);
          Alcotest.test_case "put key description" `Quick
            (run test_put_key_description);
          Alcotest.test_case "get secret" `Quick (run test_get_secret);
          Alcotest.test_case "get secret without a copy" `Quick
            (run test_get_secret_missing_key_id);
          Alcotest.test_case "get optional secret absent" `Quick
            (run test_get_secret_opt_absent);
          Alcotest.test_case "open secret store with passphrase" `Quick
            (run test_open_secret_store_passphrase);
          Alcotest.test_case "open secret store recovery fallback" `Quick
            (run test_open_secret_store_recovery_fallback);
          Alcotest.test_case "open secret store rejects bad credentials" `Quick
            (run test_open_secret_store_rejects_wrong_credential_and_algorithm);
          Alcotest.test_case "open secret store missing default" `Quick
            (run test_open_secret_store_missing_default);
          Alcotest.test_case "import backup order and missing" `Quick
            (run test_import_backup_order_and_missing);
          Alcotest.test_case "import backup validates before mutation" `Quick
            (run test_import_backup_rejects_before_mutation);
          Alcotest.test_case "import backup persistence failure" `Quick
            test_import_backup_persistence_failure;
          Alcotest.test_case "create secret store order and reopen" `Quick
            (run test_create_secret_store_order_and_reopen);
          Alcotest.test_case "create secret store failure short circuit" `Quick
            (run test_create_secret_store_failure_short_circuit);
          Alcotest.test_case "create recovery store export" `Quick
            (run test_create_recovery_store_export);
          Alcotest.test_case "create recovery store partial and mismatch" `Quick
            (run test_create_recovery_store_partial_and_mismatch);
          Alcotest.test_case "import all cross-signing secrets" `Quick
            (run test_import_cross_signing);
          Alcotest.test_case "partial and stale cross-signing import" `Quick
            (run test_import_cross_signing_partial_and_stale);
          Alcotest.test_case "store secret keeps other copies" `Quick
            (run test_store_secret_preserves_other_keys);
          Alcotest.test_case "store handle writes and reopens secret" `Quick
            (run test_put_store_secret_round_trip);
          Alcotest.test_case "store handle write short circuit" `Quick
            (run test_put_store_secret_failure_short_circuit);
        ] );
      ( "send to device",
        [
          Alcotest.test_case "send" `Quick (run test_send_to_device);
          Alcotest.test_case "fresh transaction id" `Quick
            (run test_send_to_device_new_txn);
          Alcotest.test_case "empty users are dropped" `Quick
            (run test_send_to_device_drops_empty_users);
        ] );
      ( "cross-signing",
        [
          Alcotest.test_case "upload signing keys" `Quick
            (run test_upload_signing_keys);
          Alcotest.test_case "upload with auth" `Quick
            (run test_upload_signing_keys_with_auth);
          Alcotest.test_case "UIAA challenge and retry" `Quick
            (run test_upload_signing_keys_uiaa);
          Alcotest.test_case "upload signatures" `Quick
            (run test_upload_signatures);
          Alcotest.test_case "query returns cross-signing keys" `Quick
            (run test_query_keys_cross_signing);
        ] );
      ( "room keys",
        [
          Alcotest.test_case "backup version alignment" `Quick
            test_backup_version_state;
          Alcotest.test_case "create version" `Quick (run test_create_version);
          Alcotest.test_case "current version" `Quick
            (run test_get_current_version);
          Alcotest.test_case "update and delete version" `Quick
            (run test_update_and_delete_version);
          Alcotest.test_case "put keys" `Quick (run test_put_keys);
          Alcotest.test_case "get keys" `Quick (run test_get_keys);
          Alcotest.test_case "per-room and per-session paths" `Quick
            (run test_room_and_session_key_paths);
          Alcotest.test_case "put room keys body" `Quick
            (run test_put_room_keys_body);
        ] );
      ( "backup session data",
        [
          Alcotest.test_case "encrypt and decrypt" `Quick
            (run test_backup_session_round_trip);
          Alcotest.test_case "wrong key fails" `Quick
            (run test_backup_session_wrong_key);
          Alcotest.test_case "recovery key" `Quick
            (run test_backup_recovery_key);
          Alcotest.test_case "auth data signature" `Quick
            (run test_backup_auth_data_signature);
        ] );
      ( "portable room-key export",
        [
          Alcotest.test_case "round trip and IV clamp" `Quick
            test_room_key_export_round_trip;
          Alcotest.test_case "wrong password and tampering" `Quick
            test_room_key_export_rejects_tampering;
          Alcotest.test_case "pinned Rust fixture" `Quick
            test_room_key_export_rust_fixture;
          Alcotest.test_case "decrypt rounds policy" `Quick
            test_room_key_export_decrypt_rounds_policy;
          Alcotest.test_case "validation" `Quick test_room_key_export_validation;
        ] );
      ( "dehydrated devices",
        [
          Alcotest.test_case "pickle key codec" `Quick test_pickle_key_codec;
          Alcotest.test_case "get" `Quick (run test_dehydrated_get);
          Alcotest.test_case "get if present" `Quick
            (run test_dehydrated_get_if_present);
          Alcotest.test_case "put" `Quick (run test_dehydrated_put);
          Alcotest.test_case "put omits absent one_time/fallback keys" `Quick
            (run test_dehydrated_put_omits_absent_keys);
          Alcotest.test_case "create and upload" `Quick
            (run test_dehydrated_create_and_upload);
          Alcotest.test_case "create explicit display name" `Quick
            (run test_dehydrated_create_explicit_display_name);
          Alcotest.test_case "lifecycle callbacks" `Quick
            (run test_dehydrated_lifecycle_callbacks);
          Alcotest.test_case "create preflight" `Quick
            (run test_dehydrated_create_preflight);
          Alcotest.test_case "delete" `Quick (run test_dehydrated_delete);
          Alcotest.test_case "delete if present" `Quick
            (run test_dehydrated_delete_if_present);
          Alcotest.test_case "pickle key presence and reset" `Quick
            (run test_pickle_key_presence_reset_and_malformed);
          Alcotest.test_case "driver pickle cache order and create" `Quick
            (run test_driver_pickle_cache_order_and_create);
          Alcotest.test_case "driver pickle reset and upload remember" `Quick
            (run test_driver_pickle_reset_and_put_remember);
          Alcotest.test_case "dehydrated cached fields restart" `Quick
            test_dehydrated_cached_fields_restart;
          Alcotest.test_case "events" `Quick (run test_dehydrated_events);
          Alcotest.test_case "rehydrate absent" `Quick
            (run test_dehydrated_rehydrate_absent);
          Alcotest.test_case "rehydrate pagination" `Quick
            (run test_dehydrated_rehydrate_empty_and_pagination);
          Alcotest.test_case "rehydrate terminal and cursor" `Quick
            (run test_dehydrated_rehydrate_terminal_pages_and_cursor_errors);
          Alcotest.test_case "rehydrate invalid data and key" `Quick
            (run test_dehydrated_rehydrate_invalid_data_and_key);
          Alcotest.test_case "rehydrate event cap" `Quick
            (run test_dehydrated_rehydrate_event_cap);
          Alcotest.test_case "rehydrate delete error and mismatch" `Quick
            (run test_dehydrated_rehydrate_delete_error_and_id_mismatch);
          Alcotest.test_case "rehydrate real room key" `Quick
            (run test_dehydrated_rehydrate_real_room_key);
        ] );
    ]

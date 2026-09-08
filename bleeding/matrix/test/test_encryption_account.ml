(** Regression tests for constructing encryption from an existing Olm account.
*)

module Ck = Matrix_client.Crypto_key
module Error = Matrix_client.Error
module Id = Matrix_proto.Id
module Olm = Matrix_client.Olm
module Random = Matrix_client.Random
module Encryption = Matrix_client.Encryption
module Driver = Matrix_client.Encryption_driver
module Crypto_store = Matrix_client.Crypto_store

let user_id = Id.User_id.of_string_exn "@account:example.org"
let device_id = Id.Device_id.of_string_exn "ACCOUNTDEV"

(* A source with more than enough bytes for account creation and the machine's
   initial requests.  Distinct fill bytes make accidental account generation
   easy to spot in these tests. *)
let random fill =
  Random.of_source (Eio.Flow.string_source (String.make 65_536 fill))

let account fill = Olm.Account.create ~random:(random fill) ()

let check_identity label expected actual =
  let expected_ed, expected_curve = Olm.Account.identity_keys expected in
  let actual_ed, actual_curve = Encryption.identity_keys actual in
  Alcotest.(check bool)
    (label ^ " Ed25519") true
    (Ck.Ed25519.Public.equal expected_ed actual_ed);
  Alcotest.(check bool)
    (label ^ " Curve25519") true
    (Ck.Curve25519.Public.equal expected_curve actual_curve);
  let expected_pickle = Olm.Account.to_pickle expected in
  let actual_pickle =
    Olm.Account.to_pickle (Encryption.snapshot actual).account
  in
  Alcotest.(check string)
    (label ^ " Ed25519 private")
    (Ck.Ed25519.Private.to_bytes expected_pickle.ed25519)
    (Ck.Ed25519.Private.to_bytes actual_pickle.ed25519);
  Alcotest.(check string)
    (label ^ " Curve25519 private")
    (Ck.Curve25519.Secret.to_bytes expected_pickle.curve25519)
    (Ck.Curve25519.Secret.to_bytes actual_pickle.curve25519)

let key_value id keys =
  match List.assoc_opt id keys with
  | Some value -> value
  | None -> Alcotest.failf "missing uploaded key %s" (Ck.Key_id.to_string id)

let test_pure_identity_and_upload () =
  let supplied = account '\x11' in
  let machine =
    Encryption.create_with_account ~random:(random '\x22') ~user_id ~device_id
      ~account:supplied ()
  in
  check_identity "pure machine" supplied machine;
  let upload = Encryption.device_keys_for_upload machine in
  Alcotest.(check string)
    "upload user"
    (Id.User_id.to_string user_id)
    (Id.User_id.to_string upload.user_id);
  Alcotest.(check string)
    "upload device"
    (Id.Device_id.to_string device_id)
    (Id.Device_id.to_string upload.device_id);
  let ed, curve = Olm.Account.identity_keys supplied in
  Alcotest.(check string)
    "uploaded Ed25519 key"
    (Ck.Ed25519.Public.to_base64 ed)
    (key_value (Ck.Key_id.of_device ~algorithm:"ed25519" device_id) upload.keys);
  Alcotest.(check string)
    "uploaded Curve25519 key"
    (Ck.Curve25519.Public.to_base64 curve)
    (key_value
       (Ck.Key_id.of_device ~algorithm:"curve25519" device_id)
       upload.keys)

let test_driver_without_store () =
  let supplied = account '\x33' in
  let driver =
    match
      Driver.create_with_account ~random:(random '\x44') ~user_id ~device_id
        ~account:supplied ()
    with
    | Ok driver -> driver
    | Error error ->
        Alcotest.failf "create without store: %s" (Error.to_string error)
  in
  check_identity "driver" supplied (Driver.machine driver);
  match Driver.save driver with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "save without store: %s" (Error.to_string error)

let with_store f =
  Eio_main.run @@ fun env ->
  let base = Filename.temp_file "matrix-account-" "" in
  Sys.remove base;
  Unix.mkdir base 0o700;
  Unix.putenv "XDG_DATA_HOME" (Filename.concat base "data");
  Unix.putenv "XDG_CONFIG_HOME" (Filename.concat base "config");
  Unix.putenv "XDG_CACHE_HOME" (Filename.concat base "cache");
  Unix.putenv "XDG_STATE_HOME" (Filename.concat base "state");
  let runtime = Filename.concat base "runtime" in
  Unix.mkdir runtime 0o700;
  Unix.putenv "XDG_RUNTIME_DIR" runtime;
  let xdg = Xdge.create (Eio.Stdenv.fs env) "matrix-account-test" in
  f xdg (Crypto_store.create ~xdg ~profile:"default")

let store_files xdg =
  let dir = Eio.Path.(Xdge.data_dir xdg / "profiles" / "default") in
  List.map
    (fun name ->
      let path = Eio.Path.(dir / name) in
      (name, if Eio.Path.is_file path then Some (Eio.Path.load path) else None))
    [
      "device.json";
      "one_time_keys.json";
      "olm_sessions.json";
      "megolm_inbound.json";
      "megolm_outbound.json";
      "crypto_state.json";
      ".crypto_generation";
    ]

let test_driver_store_round_trip () =
  with_store @@ fun _xdg store ->
  let supplied = account '\x55' in
  let driver =
    match
      Driver.create_with_account ~random:(random '\x66') ~user_id ~device_id
        ~account:supplied ~store ()
    with
    | Ok driver -> driver
    | Error error ->
        Alcotest.failf "create with empty store: %s" (Error.to_string error)
  in
  check_identity "stored driver before save" supplied (Driver.machine driver);
  (match Driver.save driver with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "save supplied account: %s" (Error.to_string error));
  let restored =
    match
      Driver.create ~random:(random '\x77') ~user_id ~device_id ~store ()
    with
    | Ok driver -> driver
    | Error error ->
        Alcotest.failf "reload supplied account: %s" (Error.to_string error)
  in
  check_identity "stored driver after reload" supplied (Driver.machine restored)

let test_nonempty_store_rejected_unchanged () =
  with_store @@ fun xdg store ->
  let original = account '\x88' in
  let driver =
    match
      Driver.create_with_account ~random:(random '\x99') ~user_id ~device_id
        ~account:original ~store ()
    with
    | Ok driver -> driver
    | Error error ->
        Alcotest.failf "create initial store: %s" (Error.to_string error)
  in
  (match Driver.save driver with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "save initial store: %s" (Error.to_string error));
  let before =
    match Crypto_store.load store with
    | Ok (Some snapshot) -> snapshot
    | Ok None -> Alcotest.fail "saved store is empty"
    | Error error ->
        Alcotest.failf "load initial store: %s" (Error.to_string error)
  in
  let files_before = store_files xdg in
  let replacement = account '\xaa' in
  (match
     Driver.create_with_account ~random:(random '\xbb') ~user_id ~device_id
       ~account:replacement ~store ()
   with
  | Error (Error.Json_error _) -> ()
  | Ok _ -> Alcotest.fail "non-empty store was accepted"
  | Error error ->
      Alcotest.failf "wrong non-empty-store error: %s" (Error.to_string error));
  let after =
    match Crypto_store.load store with
    | Ok (Some snapshot) -> snapshot
    | Ok None -> Alcotest.fail "rejected create erased the store"
    | Error error ->
        Alcotest.failf "load after rejection: %s" (Error.to_string error)
  in
  Alcotest.(check (list (pair string (option string))))
    "rejected create preserves every store file" files_before (store_files xdg);
  let before_ed, before_curve = Olm.Account.identity_keys before.account in
  let after_ed, after_curve = Olm.Account.identity_keys after.account in
  Alcotest.(check bool)
    "rejected create preserves Ed25519" true
    (Ck.Ed25519.Public.equal before_ed after_ed);
  Alcotest.(check bool)
    "rejected create preserves Curve25519" true
    (Ck.Curve25519.Public.equal before_curve after_curve)

let test_eio_rejection () =
  with_store @@ fun _xdg store ->
  let original = account '\xcc' in
  let initial =
    match
      Driver.create_with_account ~random:(random '\xdd') ~user_id ~device_id
        ~account:original ~store ()
    with
    | Ok driver -> driver
    | Error error ->
        Alcotest.failf "create Eio fixture: %s" (Error.to_string error)
  in
  (match Driver.save initial with
  | Ok () -> ()
  | Error error -> Alcotest.failf "save Eio fixture: %s" (Error.to_string error));
  let replacement = account '\xee' in
  try
    ignore
      (Matrix_eio.Encryption.create_with_account ~random:(random '\xff')
         ~user_id ~device_id ~account:replacement ~store ());
    Alcotest.fail "Eio wrapper accepted a non-empty store"
  with
  | Eio.Io (Matrix_eio.Error.E (Matrix_eio.Error.Json _), _) -> ()
  | Eio.Io _ -> Alcotest.fail "Eio wrapper raised the wrong error"

let () =
  Alcotest.run "encryption-account"
    [
      ( "account",
        [
          Alcotest.test_case "pure identity and upload" `Quick
            test_pure_identity_and_upload;
          Alcotest.test_case "driver without store" `Quick
            test_driver_without_store;
          Alcotest.test_case "driver store round trip" `Quick
            test_driver_store_round_trip;
          Alcotest.test_case "non-empty store rejected unchanged" `Quick
            test_nonempty_store_rejected_unchanged;
          Alcotest.test_case "Eio rejection" `Quick test_eio_rejection;
        ] );
    ]

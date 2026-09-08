module Qr = Matrix_client.Qr_login.Msc4108
module Secrets = Qr.Secrets
module Cs = Matrix_client.Cross_signing
module Ck = Matrix_client.Crypto_key
module Backup = Matrix_client.Backup
module Random = Matrix_client.Random
module Id = Matrix_proto.Id
module Base64 = Matrix_proto.Base64

let user_id = Id.User_id.of_string_exn "@alice:example.org"

let random byte =
  Random.of_source (Eio.Flow.string_source (String.make 4_096 byte))

let complete_identity byte =
  let identity = Cs.create_private_identity ~user_id in
  Cs.generate_private_keys ~random:(random byte) identity;
  identity

let get_secret name = function
  | Some secret -> secret
  | None -> Alcotest.failf "generated identity has no %s secret" name

let secrets identity =
  ( get_secret "master" (Cs.master_secret identity),
    get_secret "self-signing" (Cs.self_signing_secret identity),
    get_secret "user-signing" (Cs.user_signing_secret identity) )

let encode_secret secret = Base64.encode (Ck.Ed25519.Private.to_bytes secret)
let private_bytes secret = Ck.Ed25519.Private.to_bytes secret

let partial_identity identity ~master ~self_signing ~user_signing =
  let master_secret, self_signing_secret, user_signing_secret =
    secrets identity
  in
  let expected_master = Ck.Ed25519.Private.public master_secret in
  let expected_self_signing = Ck.Ed25519.Private.public self_signing_secret in
  let expected_user_signing = Ck.Ed25519.Private.public user_signing_secret in
  match
    Cs.private_identity_of_secrets ~user_id ~expected_master
      ~expected_self_signing ~expected_user_signing
      ~master:(Option.map encode_secret master)
      ~self_signing:(Option.map encode_secret self_signing)
      ~user_signing:(Option.map encode_secret user_signing)
  with
  | Ok identity -> identity
  | Error error ->
      Alcotest.failf "could not construct a partial identity: %a"
        Cs.pp_private_identity_import_error error

let check_missing expected identity =
  match Secrets.export ~private_identity:identity () with
  | Error (Secrets.Missing_cross_signing_secret actual) ->
      Alcotest.(check bool) "missing role" true (actual = expected)
  | Error error ->
      Alcotest.failf "wrong export error: %a" Secrets.pp_error error
  | Ok _ -> Alcotest.fail "incomplete identity was exported"

let test_missing_master () =
  let identity = complete_identity '\x11' in
  let _, self_signing, user_signing = secrets identity in
  check_missing Cs.Master
    (partial_identity identity ~master:None ~self_signing:(Some self_signing)
       ~user_signing:(Some user_signing))

let test_missing_self_signing () =
  let identity = complete_identity '\x12' in
  let master, _, user_signing = secrets identity in
  check_missing Cs.Self_signing
    (partial_identity identity ~master:(Some master) ~self_signing:None
       ~user_signing:(Some user_signing))

let test_missing_user_signing () =
  let identity = complete_identity '\x13' in
  let master, self_signing, _ = secrets identity in
  check_missing Cs.User_signing
    (partial_identity identity ~master:(Some master)
       ~self_signing:(Some self_signing) ~user_signing:None)

let export identity =
  match Secrets.export ~private_identity:identity () with
  | Ok bundle -> bundle
  | Error error ->
      Alcotest.failf "complete identity did not export: %a" Secrets.pp_error
        error

let import bundle =
  match Secrets.import ~user_id bundle with
  | Ok imported -> imported
  | Error error ->
      Alcotest.failf "valid bundle did not import: %a" Secrets.pp_error error

let check_identity expected actual =
  let expected_master, expected_self, expected_user = secrets expected in
  let actual_master, actual_self, actual_user = secrets actual in
  Alcotest.(check string)
    "master bytes"
    (private_bytes expected_master)
    (private_bytes actual_master);
  Alcotest.(check string)
    "self-signing bytes"
    (private_bytes expected_self)
    (private_bytes actual_self);
  Alcotest.(check string)
    "user-signing bytes"
    (private_bytes expected_user)
    (private_bytes actual_user)

let test_cross_signing_roundtrip () =
  let identity = complete_identity '\x21' in
  let master, self_signing, user_signing = secrets identity in
  let bundle = export identity in
  Alcotest.(check string)
    "wire master" (encode_secret master)
    bundle.Qr.Messages.cross_signing.master_key;
  Alcotest.(check string)
    "wire self-signing"
    (encode_secret self_signing)
    bundle.cross_signing.self_signing_key;
  Alcotest.(check string)
    "wire user-signing"
    (encode_secret user_signing)
    bundle.cross_signing.user_signing_key;
  Alcotest.(check bool) "backup absent on wire" true (bundle.backup = None);
  let imported = import bundle in
  Alcotest.(check bool) "backup remains absent" true (imported.backup = None);
  check_identity identity imported.private_identity

let test_backup_roundtrip () =
  let identity = complete_identity '\x31' in
  let decryption_key = Backup.Decryption_key.generate ~random:(random '\x32') in
  let backup = { Secrets.backup_version = "nine"; decryption_key } in
  let bundle =
    match Secrets.export ~private_identity:identity ~backup () with
    | Ok bundle -> bundle
    | Error error -> Alcotest.failf "backup export: %a" Secrets.pp_error error
  in
  (match bundle.backup with
  | None -> Alcotest.fail "backup missing from wire bundle"
  | Some wire ->
      Alcotest.(check string) "algorithm" Backup.backup_algorithm wire.algorithm;
      Alcotest.(check string) "version" "nine" wire.backup_version;
      Alcotest.(check string)
        "wire key"
        (Backup.Decryption_key.to_base64 decryption_key)
        wire.key);
  let imported = import bundle in
  match imported.backup with
  | None -> Alcotest.fail "backup missing after import"
  | Some actual ->
      Alcotest.(check string) "imported version" "nine" actual.backup_version;
      Alcotest.(check string)
        "imported key"
        (Backup.Decryption_key.to_base64 decryption_key)
        (Backup.Decryption_key.to_base64 actual.decryption_key)

let check_invalid_signing expected_role bundle =
  match Secrets.import ~user_id bundle with
  | Error (Secrets.Invalid_cross_signing_secret (Cs.Invalid_secret (role, _)))
    ->
      Alcotest.(check bool) "invalid role" true (role = expected_role)
  | Error error ->
      Alcotest.failf "wrong signing error: %a" Secrets.pp_error error
  | Ok _ -> Alcotest.fail "malformed signing seed was imported"

let test_invalid_cross_signing_secrets () =
  let bundle = export (complete_identity '\x41') in
  check_invalid_signing Cs.Master
    {
      bundle with
      cross_signing = { bundle.cross_signing with master_key = "not base64!" };
    };
  check_invalid_signing Cs.Self_signing
    {
      bundle with
      cross_signing = { bundle.cross_signing with self_signing_key = "AQ" };
    }

let test_unsupported_backup_algorithm () =
  let bundle = export (complete_identity '\x51') in
  let bundle =
    {
      bundle with
      backup =
        Some
          {
            Qr.Messages.algorithm = "m.megolm_backup.v2.future";
            backup_version = "1";
            key = Base64.encode (String.make 32 '\x52');
          };
    }
  in
  match Secrets.import ~user_id bundle with
  | Error (Secrets.Unsupported_backup_algorithm algorithm) ->
      Alcotest.(check string)
        "unsupported algorithm" "m.megolm_backup.v2.future" algorithm
  | Error error ->
      Alcotest.failf "wrong algorithm error: %a" Secrets.pp_error error
  | Ok _ -> Alcotest.fail "unsupported backup algorithm was imported"

let test_invalid_backup_key () =
  let bundle = export (complete_identity '\x61') in
  let bundle =
    {
      bundle with
      backup =
        Some
          {
            Qr.Messages.algorithm = Backup.backup_algorithm;
            backup_version = "7";
            key = "not base64!";
          };
    }
  in
  match Secrets.import ~user_id bundle with
  | Error (Secrets.Invalid_backup_key reason) ->
      Alcotest.(check bool) "reason present" true (String.length reason > 0)
  | Error error ->
      Alcotest.failf "wrong backup-key error: %a" Secrets.pp_error error
  | Ok _ -> Alcotest.fail "malformed backup key was imported"

let test_error_printer () =
  let errors =
    [
      Secrets.Missing_cross_signing_secret Cs.Master;
      Secrets.Invalid_cross_signing_secret
        (Cs.Invalid_secret (Cs.Self_signing, "bad seed"));
      Secrets.Unsupported_backup_algorithm "future";
      Secrets.Invalid_backup_key "bad key";
    ]
  in
  List.iter
    (fun error ->
      let rendered = Format.asprintf "%a" Secrets.pp_error error in
      Alcotest.(check bool)
        "printer is non-empty" true
        (String.length rendered > 0))
    errors

let () =
  Alcotest.run "qr secrets"
    [
      ( "export",
        [
          Alcotest.test_case "missing master" `Quick test_missing_master;
          Alcotest.test_case "missing self-signing" `Quick
            test_missing_self_signing;
          Alcotest.test_case "missing user-signing" `Quick
            test_missing_user_signing;
          Alcotest.test_case "cross-signing round trip" `Quick
            test_cross_signing_roundtrip;
          Alcotest.test_case "backup round trip" `Quick test_backup_roundtrip;
        ] );
      ( "import failures",
        [
          Alcotest.test_case "invalid cross-signing secrets" `Quick
            test_invalid_cross_signing_secrets;
          Alcotest.test_case "unsupported backup algorithm" `Quick
            test_unsupported_backup_algorithm;
          Alcotest.test_case "invalid backup key" `Quick test_invalid_backup_key;
          Alcotest.test_case "error printer" `Quick test_error_printer;
        ] );
    ]

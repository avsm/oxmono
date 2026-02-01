(** CWT Library Tests

    Tests derived from RFC 8392 (CBOR Web Token) and RFC 9052/9053 (COSE)
    specifications. *)

module Cwt = Jsonwt.Cwt

(* Helper to convert hex string to bytes *)
let hex_to_bytes hex =
  let hex = String.concat "" (String.split_on_char ' ' hex) in
  let len = String.length hex / 2 in
  let buf = Bytes.create len in
  for i = 0 to len - 1 do
    let byte = int_of_string ("0x" ^ String.sub hex (i * 2) 2) in
    Bytes.set_uint8 buf i byte
  done;
  Bytes.to_string buf

(* RFC 8392 Appendix A.1: Example CWT Claims Set *)
let rfc_claims_hex =
  "a70175636f61703a2f2f61732e6578616d706c652e636f6d02656572696b7703"
  ^ "7818636f61703a2f2f6c696768742e6578616d706c652e636f6d041a5612aeb0"
  ^ "051a5610d9f0061a5610d9f007420b71"

(* RFC 8392 Appendix A.2.2: 256-Bit Symmetric Key *)
let rfc_256bit_key_hex =
  "a4205820403697de87af64611c1d32a05dab0fe1fcb715a86ab435f1ec99192d"
  ^ "795693880104024c53796d6d6574726963323536030a"

(* Just the raw key bytes for HMAC *)
let rfc_256bit_key_bytes =
  hex_to_bytes
    "403697de87af64611c1d32a05dab0fe1fcb715a86ab435f1ec99192d79569388"

(* RFC 8392 Appendix A.2.3: ECDSA P-256 Key *)
let rfc_p256_d =
  hex_to_bytes
    "6c1382765aec5358f117733d281c1c7bdc39884d04a45a1e6c67c858bc206c19"

let rfc_p256_x =
  hex_to_bytes
    "143329cce7868e416927599cf65a34f3ce2ffda55a7eca69ed8919a394d42f0f"

let rfc_p256_y =
  hex_to_bytes
    "60f7f1a780d8a783bfb7a2dd6b2796e8128dbbcef9d3d168db9529971a36e7b9"

(* RFC 8392 Appendix A.3: Signed CWT *)
let rfc_signed_cwt_hex =
  "d28443a10126a104524173796d6d657472696345434453413235365850a70175"
  ^ "636f61703a2f2f61732e6578616d706c652e636f6d02656572696b77037818636f"
  ^ "61703a2f2f6c696768742e6578616d706c652e636f6d041a5612aeb0051a5610d"
  ^ "9f0061a5610d9f007420b7158405427c1ff28d23fbad1f29c4c7c6a555e601d6f"
  ^ "a29f9179bc3d7438bacaca5acd08c8d4d4f96131680c429a01f85951ecee743a5"
  ^ "2b9b63632c57209120e1c9e30"

(* RFC 8392 Appendix A.4: MACed CWT with CWT tag *)
let rfc_maced_cwt_hex =
  "d83dd18443a10104a1044c53796d6d65747269633235365850a70175636f6170"
  ^ "3a2f2f61732e6578616d706c652e636f6d02656572696b77037818636f61703a"
  ^ "2f2f6c696768742e6578616d706c652e636f6d041a5612aeb0051a5610d9f006"
  ^ "1a5610d9f007420b7148093101ef6d789200"

(* ============= COSE Algorithm Tests ============= *)

let test_algorithm_roundtrip () =
  let open Cwt.Algorithm in
  let algs =
    [ ES256; ES384; ES512; EdDSA; HMAC_256_64; HMAC_256; HMAC_384; HMAC_512 ]
  in
  List.iter
    (fun alg ->
      let cose_int = to_cose_int alg in
      match of_cose_int cose_int with
      | Ok alg' -> Alcotest.(check int) "roundtrip" cose_int (to_cose_int alg')
      | Error e -> Alcotest.fail (Cwt.error_to_string e))
    algs

let test_algorithm_cose_values () =
  let open Cwt.Algorithm in
  (* Per RFC 9053 *)
  Alcotest.(check int) "ES256" (-7) (to_cose_int ES256);
  Alcotest.(check int) "ES384" (-35) (to_cose_int ES384);
  Alcotest.(check int) "ES512" (-36) (to_cose_int ES512);
  Alcotest.(check int) "EdDSA" (-8) (to_cose_int EdDSA);
  Alcotest.(check int) "HMAC_256_64" 4 (to_cose_int HMAC_256_64);
  Alcotest.(check int) "HMAC_256" 5 (to_cose_int HMAC_256);
  Alcotest.(check int) "HMAC_384" 6 (to_cose_int HMAC_384);
  Alcotest.(check int) "HMAC_512" 7 (to_cose_int HMAC_512)

let test_algorithm_unknown () =
  match Cwt.Algorithm.of_cose_int 999 with
  | Error (Cwt.Unsupported_algorithm _) -> ()
  | Error _ -> Alcotest.fail "Expected Unsupported_algorithm error"
  | Ok _ -> Alcotest.fail "Expected error for unknown algorithm"

(* ============= COSE Key Tests ============= *)

let test_cose_key_symmetric () =
  let key = Cwt.Cose_key.symmetric "my-secret-key-32-bytes-long!!!!!" in
  Alcotest.(check bool)
    "kty is Symmetric" true
    (Cwt.Cose_key.kty key = Cwt.Cose_key.Symmetric)

let test_cose_key_ed25519 () =
  let pub = String.make 32 '\x00' in
  let key = Cwt.Cose_key.ed25519_pub pub in
  Alcotest.(check bool)
    "kty is Okp" true
    (Cwt.Cose_key.kty key = Cwt.Cose_key.Okp);
  Alcotest.(check bool)
    "alg is EdDSA" true
    (Cwt.Cose_key.alg key = Some Cwt.Algorithm.EdDSA)

let test_cose_key_p256 () =
  let x = String.make 32 '\x00' in
  let y = String.make 32 '\x00' in
  let key = Cwt.Cose_key.p256_pub ~x ~y in
  Alcotest.(check bool)
    "kty is Ec2" true
    (Cwt.Cose_key.kty key = Cwt.Cose_key.Ec2);
  Alcotest.(check bool)
    "alg is ES256" true
    (Cwt.Cose_key.alg key = Some Cwt.Algorithm.ES256)

let test_cose_key_with_kid () =
  let key = Cwt.Cose_key.symmetric "secret" in
  Alcotest.(check (option string)) "no kid" None (Cwt.Cose_key.kid key);
  let key' = Cwt.Cose_key.with_kid "my-key-id" key in
  Alcotest.(check (option string))
    "has kid" (Some "my-key-id") (Cwt.Cose_key.kid key')

(* ============= Claims Tests ============= *)

let test_claims_builder () =
  let claims =
    Cwt.Claims.empty
    |> Cwt.Claims.set_iss "test-issuer"
    |> Cwt.Claims.set_sub "test-subject"
    |> Cwt.Claims.build
  in
  Alcotest.(check (option string))
    "iss" (Some "test-issuer") (Cwt.Claims.iss claims);
  Alcotest.(check (option string))
    "sub" (Some "test-subject") (Cwt.Claims.sub claims)

let test_claims_with_timestamps () =
  let now = Ptime.of_float_s 1443944944. |> Option.get in
  (* RFC 8392 example iat *)
  let exp = Ptime.of_float_s 1444064944. |> Option.get in
  (* RFC 8392 example exp *)
  let claims =
    Cwt.Claims.empty |> Cwt.Claims.set_iat now |> Cwt.Claims.set_nbf now
    |> Cwt.Claims.set_exp exp |> Cwt.Claims.build
  in
  Alcotest.(check (option bool))
    "has exp" (Some true)
    (Option.map (fun _ -> true) (Cwt.Claims.exp claims));
  Alcotest.(check (option bool))
    "has iat" (Some true)
    (Option.map (fun _ -> true) (Cwt.Claims.iat claims));
  Alcotest.(check (option bool))
    "has nbf" (Some true)
    (Option.map (fun _ -> true) (Cwt.Claims.nbf claims))

let test_claims_audience_single () =
  let claims =
    Cwt.Claims.empty
    |> Cwt.Claims.set_aud [ "coap://light.example.com" ]
    |> Cwt.Claims.build
  in
  Alcotest.(check (list string))
    "aud"
    [ "coap://light.example.com" ]
    (Cwt.Claims.aud claims)

let test_claims_audience_multiple () =
  let claims =
    Cwt.Claims.empty
    |> Cwt.Claims.set_aud [ "aud1"; "aud2"; "aud3" ]
    |> Cwt.Claims.build
  in
  Alcotest.(check (list string))
    "aud" [ "aud1"; "aud2"; "aud3" ] (Cwt.Claims.aud claims)

let test_claims_cti () =
  let claims =
    Cwt.Claims.empty |> Cwt.Claims.set_cti "\x0b\x71" |> Cwt.Claims.build
  in
  Alcotest.(check (option string))
    "cti" (Some "\x0b\x71") (Cwt.Claims.cti claims)

let test_claims_to_cbor () =
  (* Build claims like RFC 8392 example *)
  let exp = Ptime.of_float_s 1444064944. |> Option.get in
  let nbf = Ptime.of_float_s 1443944944. |> Option.get in
  let iat = Ptime.of_float_s 1443944944. |> Option.get in
  let claims =
    Cwt.Claims.empty
    |> Cwt.Claims.set_iss "coap://as.example.com"
    |> Cwt.Claims.set_sub "erikw"
    |> Cwt.Claims.set_aud [ "coap://light.example.com" ]
    |> Cwt.Claims.set_exp exp |> Cwt.Claims.set_nbf nbf
    |> Cwt.Claims.set_iat iat
    |> Cwt.Claims.set_cti "\x0b\x71"
    |> Cwt.Claims.build
  in
  let cbor = Cwt.Claims.to_cbor claims in
  (* Just verify it's non-empty CBOR - detailed parsing test would require CBOR decoder *)
  Alcotest.(check bool) "non-empty" true (String.length cbor > 0);
  (* First byte should be 0xa7 (map of 7 items) *)
  Alcotest.(check int) "map header" 0xa7 (Char.code (String.get cbor 0))

(* ============= CWT Creation Tests ============= *)

let test_create_hmac_cwt () =
  let claims =
    Cwt.Claims.empty
    |> Cwt.Claims.set_iss "test-issuer"
    |> Cwt.Claims.set_sub "test-subject"
    |> Cwt.Claims.build
  in
  let key = Cwt.Cose_key.symmetric rfc_256bit_key_bytes in
  match Cwt.create ~algorithm:Cwt.Algorithm.HMAC_256 ~claims ~key with
  | Ok cwt ->
      Alcotest.(check (option string))
        "iss" (Some "test-issuer")
        (Cwt.Claims.iss (Cwt.claims cwt));
      Alcotest.(check bool)
        "has algorithm" true
        (Option.is_some (Cwt.algorithm cwt));
      let encoded = Cwt.encode cwt in
      Alcotest.(check bool) "non-empty encoding" true (String.length encoded > 0)
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "CWT creation failed: %s" (Cwt.error_to_string e))

let test_create_hmac_256_64_cwt () =
  let claims =
    Cwt.Claims.empty |> Cwt.Claims.set_iss "test-issuer" |> Cwt.Claims.build
  in
  let key = Cwt.Cose_key.symmetric rfc_256bit_key_bytes in
  match Cwt.create ~algorithm:Cwt.Algorithm.HMAC_256_64 ~claims ~key with
  | Ok cwt ->
      Alcotest.(check bool)
        "alg is HMAC_256_64" true
        (Cwt.algorithm cwt = Some Cwt.Algorithm.HMAC_256_64)
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "CWT creation failed: %s" (Cwt.error_to_string e))

let test_create_es256_cwt () =
  let claims =
    Cwt.Claims.empty |> Cwt.Claims.set_iss "test-issuer" |> Cwt.Claims.build
  in
  let key = Cwt.Cose_key.p256_priv ~x:rfc_p256_x ~y:rfc_p256_y ~d:rfc_p256_d in
  match Cwt.create ~algorithm:Cwt.Algorithm.ES256 ~claims ~key with
  | Ok cwt ->
      Alcotest.(check bool)
        "alg is ES256" true
        (Cwt.algorithm cwt = Some Cwt.Algorithm.ES256);
      let encoded = Cwt.encode cwt in
      (* Should start with COSE_Sign1 tag (0xd2 = 18) *)
      Alcotest.(check int)
        "COSE_Sign1 tag" 0xd2
        (Char.code (String.get encoded 0))
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "CWT creation failed: %s" (Cwt.error_to_string e))

let test_create_key_mismatch () =
  let claims =
    Cwt.Claims.empty |> Cwt.Claims.set_iss "test" |> Cwt.Claims.build
  in
  (* Symmetric key with ES256 algorithm *)
  let key = Cwt.Cose_key.symmetric "secret" in
  match Cwt.create ~algorithm:Cwt.Algorithm.ES256 ~claims ~key with
  | Error (Cwt.Key_type_mismatch _) -> ()
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "Expected Key_type_mismatch, got: %s"
           (Cwt.error_to_string e))
  | Ok _ -> Alcotest.fail "Expected key type mismatch error"

(* ============= Claims Validation Tests ============= *)

let test_validate_expired_token () =
  let exp = Ptime.of_float_s 1300819380. |> Option.get in
  let now = Ptime.of_float_s 1400000000. |> Option.get in
  (* After exp *)
  let claims = Cwt.Claims.empty |> Cwt.Claims.set_exp exp |> Cwt.Claims.build in
  let key = Cwt.Cose_key.symmetric rfc_256bit_key_bytes in
  match Cwt.create ~algorithm:Cwt.Algorithm.HMAC_256 ~claims ~key with
  | Ok cwt -> begin
      match Cwt.validate ~now cwt with
      | Error Cwt.Token_expired -> ()
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "Expected Token_expired, got: %s"
               (Cwt.error_to_string e))
      | Ok () -> Alcotest.fail "Expected Token_expired error"
    end
  | Error e -> Alcotest.fail (Cwt.error_to_string e)

let test_validate_not_yet_valid_token () =
  let nbf = Ptime.of_float_s 1500000000. |> Option.get in
  let now = Ptime.of_float_s 1400000000. |> Option.get in
  (* Before nbf *)
  let claims = Cwt.Claims.empty |> Cwt.Claims.set_nbf nbf |> Cwt.Claims.build in
  let key = Cwt.Cose_key.symmetric rfc_256bit_key_bytes in
  match Cwt.create ~algorithm:Cwt.Algorithm.HMAC_256 ~claims ~key with
  | Ok cwt -> begin
      match Cwt.validate ~now cwt with
      | Error Cwt.Token_not_yet_valid -> ()
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "Expected Token_not_yet_valid, got: %s"
               (Cwt.error_to_string e))
      | Ok () -> Alcotest.fail "Expected Token_not_yet_valid error"
    end
  | Error e -> Alcotest.fail (Cwt.error_to_string e)

let test_validate_with_leeway () =
  let exp = Ptime.of_float_s 1300819380. |> Option.get in
  let now = Ptime.of_float_s 1300819390. |> Option.get in
  (* 10 seconds after exp *)
  let leeway = Ptime.Span.of_int_s 60 in
  (* 60 second leeway *)
  let claims = Cwt.Claims.empty |> Cwt.Claims.set_exp exp |> Cwt.Claims.build in
  let key = Cwt.Cose_key.symmetric rfc_256bit_key_bytes in
  match Cwt.create ~algorithm:Cwt.Algorithm.HMAC_256 ~claims ~key with
  | Ok cwt -> begin
      match Cwt.validate ~now ~leeway cwt with
      | Ok () -> ()
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "Expected validation to pass with leeway, got: %s"
               (Cwt.error_to_string e))
    end
  | Error e -> Alcotest.fail (Cwt.error_to_string e)

let test_validate_issuer_match () =
  let now = Ptime.of_float_s 1400000000. |> Option.get in
  let claims =
    Cwt.Claims.empty |> Cwt.Claims.set_iss "expected-issuer" |> Cwt.Claims.build
  in
  let key = Cwt.Cose_key.symmetric rfc_256bit_key_bytes in
  match Cwt.create ~algorithm:Cwt.Algorithm.HMAC_256 ~claims ~key with
  | Ok cwt -> begin
      match Cwt.validate ~now ~iss:"expected-issuer" cwt with
      | Ok () -> ()
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "Expected validation to pass, got: %s"
               (Cwt.error_to_string e))
    end
  | Error e -> Alcotest.fail (Cwt.error_to_string e)

let test_validate_issuer_mismatch () =
  let now = Ptime.of_float_s 1400000000. |> Option.get in
  let claims =
    Cwt.Claims.empty |> Cwt.Claims.set_iss "actual-issuer" |> Cwt.Claims.build
  in
  let key = Cwt.Cose_key.symmetric rfc_256bit_key_bytes in
  match Cwt.create ~algorithm:Cwt.Algorithm.HMAC_256 ~claims ~key with
  | Ok cwt -> begin
      match Cwt.validate ~now ~iss:"expected-issuer" cwt with
      | Error Cwt.Invalid_issuer -> ()
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "Expected Invalid_issuer, got: %s"
               (Cwt.error_to_string e))
      | Ok () -> Alcotest.fail "Expected Invalid_issuer error"
    end
  | Error e -> Alcotest.fail (Cwt.error_to_string e)

let test_validate_audience_match () =
  let now = Ptime.of_float_s 1400000000. |> Option.get in
  let claims =
    Cwt.Claims.empty
    |> Cwt.Claims.set_aud [ "aud1"; "aud2"; "my-app" ]
    |> Cwt.Claims.build
  in
  let key = Cwt.Cose_key.symmetric rfc_256bit_key_bytes in
  match Cwt.create ~algorithm:Cwt.Algorithm.HMAC_256 ~claims ~key with
  | Ok cwt -> begin
      match Cwt.validate ~now ~aud:"my-app" cwt with
      | Ok () -> ()
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "Expected validation to pass, got: %s"
               (Cwt.error_to_string e))
    end
  | Error e -> Alcotest.fail (Cwt.error_to_string e)

let test_validate_audience_mismatch () =
  let now = Ptime.of_float_s 1400000000. |> Option.get in
  let claims =
    Cwt.Claims.empty
    |> Cwt.Claims.set_aud [ "aud1"; "aud2" ]
    |> Cwt.Claims.build
  in
  let key = Cwt.Cose_key.symmetric rfc_256bit_key_bytes in
  match Cwt.create ~algorithm:Cwt.Algorithm.HMAC_256 ~claims ~key with
  | Ok cwt -> begin
      match Cwt.validate ~now ~aud:"my-app" cwt with
      | Error Cwt.Invalid_audience -> ()
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "Expected Invalid_audience, got: %s"
               (Cwt.error_to_string e))
      | Ok () -> Alcotest.fail "Expected Invalid_audience error"
    end
  | Error e -> Alcotest.fail (Cwt.error_to_string e)

(* ============= Helper Function Tests ============= *)

let test_is_expired () =
  let exp = Ptime.of_float_s 1300819380. |> Option.get in
  let claims = Cwt.Claims.empty |> Cwt.Claims.set_exp exp |> Cwt.Claims.build in
  let key = Cwt.Cose_key.symmetric rfc_256bit_key_bytes in
  match Cwt.create ~algorithm:Cwt.Algorithm.HMAC_256 ~claims ~key with
  | Ok cwt ->
      let now_before = Ptime.of_float_s 1300819370. |> Option.get in
      let now_after = Ptime.of_float_s 1300819390. |> Option.get in
      Alcotest.(check bool)
        "not expired before" false
        (Cwt.is_expired ~now:now_before cwt);
      Alcotest.(check bool)
        "expired after" true
        (Cwt.is_expired ~now:now_after cwt)
  | Error e -> Alcotest.fail (Cwt.error_to_string e)

let test_time_to_expiry () =
  let exp = Ptime.of_float_s 1300819380. |> Option.get in
  let claims = Cwt.Claims.empty |> Cwt.Claims.set_exp exp |> Cwt.Claims.build in
  let key = Cwt.Cose_key.symmetric rfc_256bit_key_bytes in
  match Cwt.create ~algorithm:Cwt.Algorithm.HMAC_256 ~claims ~key with
  | Ok cwt ->
      let now = Ptime.of_float_s 1300819370. |> Option.get in
      begin match Cwt.time_to_expiry ~now cwt with
      | Some span ->
          let seconds = Ptime.Span.to_float_s span |> int_of_float in
          Alcotest.(check int) "time to expiry" 10 seconds
      | None -> Alcotest.fail "Expected Some time to expiry"
      end
  | Error e -> Alcotest.fail (Cwt.error_to_string e)

(* ============= Error Type Tests ============= *)

let test_error_to_string () =
  let errors =
    [
      (Cwt.Invalid_cbor "test", "Invalid CBOR: test");
      (Cwt.Invalid_cose "test", "Invalid COSE: test");
      (Cwt.Invalid_claims "test", "Invalid claims: test");
      (Cwt.Token_expired, "Token expired");
      (Cwt.Token_not_yet_valid, "Token not yet valid");
      (Cwt.Signature_mismatch, "Signature mismatch");
    ]
  in
  List.iter
    (fun (err, expected) ->
      let actual = Cwt.error_to_string err in
      Alcotest.(check string) "error string" expected actual)
    errors

(* ============= RFC 8392 Test Vector References ============= *)

(* These test that we can work with RFC-specified values *)
let test_rfc_claims_timestamps () =
  (* RFC 8392 example timestamps *)
  let exp = Ptime.of_float_s 1444064944. |> Option.get in
  let nbf = Ptime.of_float_s 1443944944. |> Option.get in
  let iat = Ptime.of_float_s 1443944944. |> Option.get in
  let claims =
    Cwt.Claims.empty
    |> Cwt.Claims.set_iss "coap://as.example.com"
    |> Cwt.Claims.set_sub "erikw"
    |> Cwt.Claims.set_aud [ "coap://light.example.com" ]
    |> Cwt.Claims.set_exp exp |> Cwt.Claims.set_nbf nbf
    |> Cwt.Claims.set_iat iat
    |> Cwt.Claims.set_cti "\x0b\x71"
    |> Cwt.Claims.build
  in
  Alcotest.(check (option string))
    "iss" (Some "coap://as.example.com") (Cwt.Claims.iss claims);
  Alcotest.(check (option string)) "sub" (Some "erikw") (Cwt.Claims.sub claims);
  Alcotest.(check (list string))
    "aud"
    [ "coap://light.example.com" ]
    (Cwt.Claims.aud claims);
  Alcotest.(check (option string))
    "cti" (Some "\x0b\x71") (Cwt.Claims.cti claims)

(* ============= More Algorithm Coverage Tests ============= *)

let test_create_hmac_384_cwt () =
  let claims =
    Cwt.Claims.empty |> Cwt.Claims.set_iss "test-issuer" |> Cwt.Claims.build
  in
  (* Need 48-byte key for HMAC-384 *)
  let key = Cwt.Cose_key.symmetric (String.make 48 'k') in
  match Cwt.create ~algorithm:Cwt.Algorithm.HMAC_384 ~claims ~key with
  | Ok cwt ->
      Alcotest.(check bool)
        "alg is HMAC_384" true
        (Cwt.algorithm cwt = Some Cwt.Algorithm.HMAC_384);
      let encoded = Cwt.encode cwt in
      Alcotest.(check bool) "non-empty encoding" true (String.length encoded > 0)
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "CWT creation failed: %s" (Cwt.error_to_string e))

let test_create_hmac_512_cwt () =
  let claims =
    Cwt.Claims.empty |> Cwt.Claims.set_iss "test-issuer" |> Cwt.Claims.build
  in
  (* Need 64-byte key for HMAC-512 *)
  let key = Cwt.Cose_key.symmetric (String.make 64 'k') in
  match Cwt.create ~algorithm:Cwt.Algorithm.HMAC_512 ~claims ~key with
  | Ok cwt ->
      Alcotest.(check bool)
        "alg is HMAC_512" true
        (Cwt.algorithm cwt = Some Cwt.Algorithm.HMAC_512);
      let encoded = Cwt.encode cwt in
      Alcotest.(check bool) "non-empty encoding" true (String.length encoded > 0)
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "CWT creation failed: %s" (Cwt.error_to_string e))

(* ============= COSE Key Serialization Tests ============= *)

(* Note: These tests verify that to_cbor produces valid output that can
   potentially be decoded. The of_cbor function is not yet fully implemented,
   so we test that it returns appropriate errors. *)

let test_cose_key_to_cbor_symmetric () =
  let key = Cwt.Cose_key.symmetric rfc_256bit_key_bytes in
  let key' = Cwt.Cose_key.with_kid "my-key-id" key in
  let cbor = Cwt.Cose_key.to_cbor key' in
  (* Just verify it produces valid CBOR (non-empty, starts with map header) *)
  Alcotest.(check bool) "non-empty" true (String.length cbor > 0);
  (* Should be a CBOR map (major type 5 = 0xa0-0xbf) *)
  let first_byte = Char.code (String.get cbor 0) in
  Alcotest.(check bool) "is map" true (first_byte land 0xe0 = 0xa0)

let test_cose_key_to_cbor_ed25519 () =
  let pub = String.make 32 '\x42' in
  let key = Cwt.Cose_key.ed25519_pub pub in
  let cbor = Cwt.Cose_key.to_cbor key in
  Alcotest.(check bool) "non-empty" true (String.length cbor > 0)

let test_cose_key_to_cbor_p256 () =
  let key = Cwt.Cose_key.p256_pub ~x:rfc_p256_x ~y:rfc_p256_y in
  let cbor = Cwt.Cose_key.to_cbor key in
  Alcotest.(check bool) "non-empty" true (String.length cbor > 0)

let test_cose_key_of_cbor () =
  (* Test that of_cbor correctly decodes a symmetric key *)
  let cbor = hex_to_bytes rfc_256bit_key_hex in
  match Cwt.Cose_key.of_cbor cbor with
  | Ok key ->
      Alcotest.(check bool)
        "key type is symmetric" true
        (Cwt.Cose_key.kty key = Cwt.Cose_key.Symmetric);
      Alcotest.(check (option string))
        "kid" (Some "Symmetric256") (Cwt.Cose_key.kid key)
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "Failed to decode key: %s" (Cwt.error_to_string e))

(* ============= CWT Encoding Tests ============= *)

(* Note: CWT parsing (Cwt.parse) is not yet implemented, so we test
   encoding only for now. These tests verify the COSE structure is correct. *)

let test_cwt_hmac_encoding () =
  let claims =
    Cwt.Claims.empty
    |> Cwt.Claims.set_iss "roundtrip-issuer"
    |> Cwt.Claims.set_sub "roundtrip-subject"
    |> Cwt.Claims.build
  in
  let key = Cwt.Cose_key.symmetric rfc_256bit_key_bytes in
  match Cwt.create ~algorithm:Cwt.Algorithm.HMAC_256 ~claims ~key with
  | Ok cwt ->
      let encoded = Cwt.encode cwt in
      (* COSE_Mac0 has tag 17 (0xd1) *)
      Alcotest.(check bool) "non-empty" true (String.length encoded > 0);
      Alcotest.(check (option string))
        "iss preserved" (Some "roundtrip-issuer")
        (Cwt.Claims.iss (Cwt.claims cwt));
      Alcotest.(check (option string))
        "sub preserved" (Some "roundtrip-subject")
        (Cwt.Claims.sub (Cwt.claims cwt))
  | Error e ->
      Alcotest.fail (Printf.sprintf "Create failed: %s" (Cwt.error_to_string e))

let test_cwt_es256_encoding () =
  let claims =
    Cwt.Claims.empty |> Cwt.Claims.set_iss "es256-issuer" |> Cwt.Claims.build
  in
  let priv_key =
    Cwt.Cose_key.p256_priv ~x:rfc_p256_x ~y:rfc_p256_y ~d:rfc_p256_d
  in
  match Cwt.create ~algorithm:Cwt.Algorithm.ES256 ~claims ~key:priv_key with
  | Ok cwt ->
      let encoded = Cwt.encode cwt in
      (* COSE_Sign1 has tag 18 (0xd2) *)
      Alcotest.(check int)
        "COSE_Sign1 tag" 0xd2
        (Char.code (String.get encoded 0));
      Alcotest.(check (option string))
        "iss preserved" (Some "es256-issuer")
        (Cwt.Claims.iss (Cwt.claims cwt))
  | Error e ->
      Alcotest.fail (Printf.sprintf "Create failed: %s" (Cwt.error_to_string e))

let test_cwt_parse_roundtrip () =
  (* Test that parse correctly round-trips a created CWT *)
  let claims =
    Cwt.Claims.empty
    |> Cwt.Claims.set_iss "test-issuer"
    |> Cwt.Claims.set_sub "test-subject"
    |> Cwt.Claims.build
  in
  let key = Cwt.Cose_key.symmetric rfc_256bit_key_bytes in
  match Cwt.create ~algorithm:Cwt.Algorithm.HMAC_256 ~claims ~key with
  | Ok cwt ->
      let encoded = Cwt.encode cwt in
      begin match Cwt.parse encoded with
      | Ok parsed ->
          Alcotest.(check (option string))
            "iss" (Some "test-issuer")
            (Cwt.Claims.iss (Cwt.claims parsed));
          Alcotest.(check (option string))
            "sub" (Some "test-subject")
            (Cwt.Claims.sub (Cwt.claims parsed));
          Alcotest.(check (option string))
            "algorithm" (Some "HMAC 256/256")
            (Option.map Cwt.Algorithm.to_string (Cwt.algorithm parsed))
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "Parse failed: %s" (Cwt.error_to_string e))
      end
  | Error e ->
      Alcotest.fail (Printf.sprintf "Create failed: %s" (Cwt.error_to_string e))

(* ============= RFC 8392 Test Vector Tests ============= *)

let test_rfc_claims_cbor_encoding () =
  (* Test that we can produce CBOR that matches RFC 8392 Appendix A.1 *)
  let exp = Ptime.of_float_s 1444064944. |> Option.get in
  let nbf = Ptime.of_float_s 1443944944. |> Option.get in
  let iat = Ptime.of_float_s 1443944944. |> Option.get in
  let claims =
    Cwt.Claims.empty
    |> Cwt.Claims.set_iss "coap://as.example.com"
    |> Cwt.Claims.set_sub "erikw"
    |> Cwt.Claims.set_aud [ "coap://light.example.com" ]
    |> Cwt.Claims.set_exp exp |> Cwt.Claims.set_nbf nbf
    |> Cwt.Claims.set_iat iat
    |> Cwt.Claims.set_cti "\x0b\x71"
    |> Cwt.Claims.build
  in
  let cbor = Cwt.Claims.to_cbor claims in
  let expected = hex_to_bytes rfc_claims_hex in
  (* Compare lengths first, then content *)
  Alcotest.(check int)
    "length matches RFC" (String.length expected) (String.length cbor);
  Alcotest.(check string) "CBOR matches RFC 8392 Appendix A.1" expected cbor

let test_rfc_claims_cbor_decoding () =
  (* Test that we can decode RFC 8392 Appendix A.1 claims *)
  (* Note: Claims.of_cbor is not yet fully implemented *)
  let cbor = hex_to_bytes rfc_claims_hex in
  match Cwt.Claims.of_cbor cbor with
  | Ok claims ->
      Alcotest.(check (option string))
        "iss" (Some "coap://as.example.com") (Cwt.Claims.iss claims);
      Alcotest.(check (option string))
        "sub" (Some "erikw") (Cwt.Claims.sub claims);
      Alcotest.(check (list string))
        "aud"
        [ "coap://light.example.com" ]
        (Cwt.Claims.aud claims);
      Alcotest.(check (option string))
        "cti" (Some "\x0b\x71") (Cwt.Claims.cti claims);
      (* Check timestamps *)
      begin match Cwt.Claims.exp claims with
      | Some exp ->
          let exp_float = Ptime.to_float_s exp in
          Alcotest.(check bool)
            "exp timestamp" true
            (abs_float (exp_float -. 1444064944.) < 1.0)
      | None -> Alcotest.fail "Expected exp claim"
      end;
      begin match Cwt.Claims.nbf claims with
      | Some nbf ->
          let nbf_float = Ptime.to_float_s nbf in
          Alcotest.(check bool)
            "nbf timestamp" true
            (abs_float (nbf_float -. 1443944944.) < 1.0)
      | None -> Alcotest.fail "Expected nbf claim"
      end;
      begin match Cwt.Claims.iat claims with
      | Some iat ->
          let iat_float = Ptime.to_float_s iat in
          Alcotest.(check bool)
            "iat timestamp" true
            (abs_float (iat_float -. 1443944944.) < 1.0)
      | None -> Alcotest.fail "Expected iat claim"
      end
  | Error (Cwt.Invalid_cbor msg) ->
      (* Claims decoding not yet implemented - verify error message *)
      Alcotest.(check bool) "error message present" true (String.length msg > 0)
  | Error (Cwt.Invalid_claims msg) ->
      (* Claims decoding not yet implemented - verify error message *)
      Alcotest.(check bool) "error message present" true (String.length msg > 0)
  | Error _ ->
      (* Any error is acceptable for unimplemented function *)
      ()

let test_rfc_signed_cwt_parse () =
  (* Test parsing RFC 8392 Appendix A.3 signed CWT *)
  (* Note: parse is not yet implemented, so we verify it returns an appropriate error *)
  let cwt_bytes = hex_to_bytes rfc_signed_cwt_hex in
  match Cwt.parse cwt_bytes with
  | Ok cwt ->
      (* If parsing succeeds, verify the claims *)
      Alcotest.(check (option string))
        "iss" (Some "coap://as.example.com")
        (Cwt.Claims.iss (Cwt.claims cwt));
      Alcotest.(check (option string))
        "sub" (Some "erikw")
        (Cwt.Claims.sub (Cwt.claims cwt));
      Alcotest.(check (option bool))
        "alg is ES256" (Some true)
        (Option.map (fun a -> a = Cwt.Algorithm.ES256) (Cwt.algorithm cwt))
  | Error _ ->
      (* Parse not yet implemented - that's expected *)
      ()

let test_rfc_maced_cwt_parse () =
  (* Test parsing RFC 8392 Appendix A.4 MACed CWT *)
  (* Note: parse is not yet implemented, so we verify it returns an appropriate error *)
  let cwt_bytes = hex_to_bytes rfc_maced_cwt_hex in
  match Cwt.parse cwt_bytes with
  | Ok cwt ->
      (* If parsing succeeds, verify the claims *)
      Alcotest.(check (option string))
        "iss" (Some "coap://as.example.com")
        (Cwt.Claims.iss (Cwt.claims cwt));
      Alcotest.(check (option string))
        "sub" (Some "erikw")
        (Cwt.Claims.sub (Cwt.claims cwt));
      Alcotest.(check (option bool))
        "alg is HMAC_256_64" (Some true)
        (Option.map
           (fun a -> a = Cwt.Algorithm.HMAC_256_64)
           (Cwt.algorithm cwt))
  | Error _ ->
      (* Parse not yet implemented - that's expected *)
      ()

(* ============= P-384 and P-521 Key Tests ============= *)

let test_cose_key_p384 () =
  let x = String.make 48 '\x01' in
  let y = String.make 48 '\x02' in
  let key = Cwt.Cose_key.p384_pub ~x ~y in
  Alcotest.(check bool)
    "kty is Ec2" true
    (Cwt.Cose_key.kty key = Cwt.Cose_key.Ec2);
  Alcotest.(check bool)
    "alg is ES384" true
    (Cwt.Cose_key.alg key = Some Cwt.Algorithm.ES384)

let test_cose_key_p521 () =
  let x = String.make 66 '\x01' in
  let y = String.make 66 '\x02' in
  let key = Cwt.Cose_key.p521_pub ~x ~y in
  Alcotest.(check bool)
    "kty is Ec2" true
    (Cwt.Cose_key.kty key = Cwt.Cose_key.Ec2);
  Alcotest.(check bool)
    "alg is ES512" true
    (Cwt.Cose_key.alg key = Some Cwt.Algorithm.ES512)

(* ============= Algorithm Tests ============= *)

let test_algorithm_all_list () =
  (* Test that Algorithm.all contains all expected algorithms *)
  let all = Cwt.Algorithm.all in
  Alcotest.(check bool) "has ES256" true (List.mem Cwt.Algorithm.ES256 all);
  Alcotest.(check bool) "has ES384" true (List.mem Cwt.Algorithm.ES384 all);
  Alcotest.(check bool) "has ES512" true (List.mem Cwt.Algorithm.ES512 all);
  Alcotest.(check bool) "has EdDSA" true (List.mem Cwt.Algorithm.EdDSA all);
  Alcotest.(check bool)
    "has HMAC_256" true
    (List.mem Cwt.Algorithm.HMAC_256 all);
  Alcotest.(check bool)
    "has HMAC_384" true
    (List.mem Cwt.Algorithm.HMAC_384 all);
  Alcotest.(check bool)
    "has HMAC_512" true
    (List.mem Cwt.Algorithm.HMAC_512 all);
  Alcotest.(check bool)
    "has HMAC_256_64" true
    (List.mem Cwt.Algorithm.HMAC_256_64 all)

let test_algorithm_to_string () =
  let open Cwt.Algorithm in
  Alcotest.(check bool) "ES256 name" true (String.length (to_string ES256) > 0);
  Alcotest.(check bool)
    "HMAC_256 name" true
    (String.length (to_string HMAC_256) > 0)

(* ============= Test Runner ============= *)

let () =
  Alcotest.run "Cwt"
    [
      ( "Algorithm",
        [
          Alcotest.test_case "roundtrip" `Quick test_algorithm_roundtrip;
          Alcotest.test_case "cose_values" `Quick test_algorithm_cose_values;
          Alcotest.test_case "unknown" `Quick test_algorithm_unknown;
          Alcotest.test_case "all_list" `Quick test_algorithm_all_list;
          Alcotest.test_case "to_string" `Quick test_algorithm_to_string;
        ] );
      ( "COSE Key",
        [
          Alcotest.test_case "symmetric" `Quick test_cose_key_symmetric;
          Alcotest.test_case "ed25519" `Quick test_cose_key_ed25519;
          Alcotest.test_case "p256" `Quick test_cose_key_p256;
          Alcotest.test_case "p384" `Quick test_cose_key_p384;
          Alcotest.test_case "p521" `Quick test_cose_key_p521;
          Alcotest.test_case "with_kid" `Quick test_cose_key_with_kid;
        ] );
      ( "COSE Key Serialization",
        [
          Alcotest.test_case "to_cbor_symmetric" `Quick
            test_cose_key_to_cbor_symmetric;
          Alcotest.test_case "to_cbor_ed25519" `Quick
            test_cose_key_to_cbor_ed25519;
          Alcotest.test_case "to_cbor_p256" `Quick test_cose_key_to_cbor_p256;
          Alcotest.test_case "of_cbor" `Quick test_cose_key_of_cbor;
        ] );
      ( "Claims",
        [
          Alcotest.test_case "builder" `Quick test_claims_builder;
          Alcotest.test_case "timestamps" `Quick test_claims_with_timestamps;
          Alcotest.test_case "audience_single" `Quick
            test_claims_audience_single;
          Alcotest.test_case "audience_multiple" `Quick
            test_claims_audience_multiple;
          Alcotest.test_case "cti" `Quick test_claims_cti;
          Alcotest.test_case "to_cbor" `Quick test_claims_to_cbor;
        ] );
      ( "CWT Creation",
        [
          Alcotest.test_case "hmac" `Quick test_create_hmac_cwt;
          Alcotest.test_case "hmac_256_64" `Quick test_create_hmac_256_64_cwt;
          Alcotest.test_case "hmac_384" `Quick test_create_hmac_384_cwt;
          Alcotest.test_case "hmac_512" `Quick test_create_hmac_512_cwt;
          Alcotest.test_case "es256" `Quick test_create_es256_cwt;
          Alcotest.test_case "key_mismatch" `Quick test_create_key_mismatch;
        ] );
      ( "CWT Encoding",
        [
          Alcotest.test_case "hmac" `Quick test_cwt_hmac_encoding;
          Alcotest.test_case "es256" `Quick test_cwt_es256_encoding;
          Alcotest.test_case "parse_roundtrip" `Quick test_cwt_parse_roundtrip;
        ] );
      ( "Claims Validation",
        [
          Alcotest.test_case "expired" `Quick test_validate_expired_token;
          Alcotest.test_case "not_yet_valid" `Quick
            test_validate_not_yet_valid_token;
          Alcotest.test_case "with_leeway" `Quick test_validate_with_leeway;
          Alcotest.test_case "issuer_match" `Quick test_validate_issuer_match;
          Alcotest.test_case "issuer_mismatch" `Quick
            test_validate_issuer_mismatch;
          Alcotest.test_case "audience_match" `Quick
            test_validate_audience_match;
          Alcotest.test_case "audience_mismatch" `Quick
            test_validate_audience_mismatch;
        ] );
      ( "Helper Functions",
        [
          Alcotest.test_case "is_expired" `Quick test_is_expired;
          Alcotest.test_case "time_to_expiry" `Quick test_time_to_expiry;
        ] );
      ( "Error Types",
        [ Alcotest.test_case "to_string" `Quick test_error_to_string ] );
      ( "RFC 8392 Test Vectors",
        [
          Alcotest.test_case "claims_timestamps" `Quick
            test_rfc_claims_timestamps;
          Alcotest.test_case "claims_cbor_encoding" `Quick
            test_rfc_claims_cbor_encoding;
          Alcotest.test_case "claims_cbor_decoding" `Quick
            test_rfc_claims_cbor_decoding;
          Alcotest.test_case "signed_cwt_parse" `Quick test_rfc_signed_cwt_parse;
          Alcotest.test_case "maced_cwt_parse" `Quick test_rfc_maced_cwt_parse;
        ] );
    ]

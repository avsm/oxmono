(* SPDX-License-Identifier: ISC *)
module C = Jsonwt_cwt
module B = Cbort.Cbor

let int = B.int
let encode = Cbort.encode_string Cbort.any
let ok = function Ok x -> x | Error e -> failwith (C.error_to_string e)
let check name value = Alcotest.(check bool) name true value
let secret = String.make 32 'k'

let key =
  C.Cose_key.symmetric secret |> C.Cose_key.with_alg C.Algorithm.HMAC_256

let now = Option.get (Ptime.of_float_s 1000.)
let header = encode (B.Map [ (int 1, int 5) ])
let payload = encode (B.Map [ (int 1, B.Text "issuer"); (int 4, int 1060) ])

let mac protected payload =
  let input =
    encode
      (B.Array [ B.Text "MAC0"; B.Bytes protected; B.Bytes ""; B.Bytes payload ])
  in
  Digestif.SHA256.(hmac_string ~key:secret input |> to_raw_string)

let envelope ?(tag = 17) ?(unprotected = B.Map []) protected payload sig_ =
  encode
    (B.Tag
       ( tag,
         B.Array
           [ B.Bytes protected; unprotected; B.Bytes payload; B.Bytes sig_ ] ))

let signed ?tag ?unprotected protected payload =
  envelope ?tag ?unprotected protected payload (mac protected payload)

let valid ?(key = key) raw =
  match C.parse raw with
  | Error _ -> false
  | Ok t ->
      Result.is_ok
        (C.verify_and_validate ~key ~now ~allowed_algs:[ C.Algorithm.HMAC_256 ]
           t)

let sample = signed header payload

let wire () =
  check "independently MACed control" (valid sample);
  check "bad MAC"
    (not (valid (envelope header payload (String.make 32 '\000'))));
  let reordered =
    encode (B.Map [ (int 4, int 1060); (int 1, B.Text "issuer") ])
  in
  check "valid MAC over reordered map" (valid (signed header reordered));
  check "unsigned payload reorder"
    (not (valid (envelope header reordered (mac header payload))));
  let duplicate =
    encode
      (B.Map [ (int 1, B.Text "issuer"); (int 4, int 1060); (int 4, int 900) ])
  in
  check "unsigned duplicate expiry"
    (not (valid (envelope header duplicate (mac header payload))));
  check "CWT tag 61" (valid ("\xd8\x3d" ^ sample));
  check "untagged COSE" (valid (String.sub sample 1 (String.length sample - 1)));
  check "indefinite envelope"
    (valid
       ("\xd1\x9f" ^ String.sub sample 2 (String.length sample - 2) ^ "\xff"));
  let indefinite = "\xbf\x01\x66issuer\x04\x19\x04\x24\xff" in
  check "indefinite claims authenticate original bytes"
    (valid (signed header indefinite));
  check "trailing bytes" (not (valid (sample ^ "junk")));
  check "signature tag on HMAC" (not (valid (signed ~tag:18 header payload)));
  check "parse/encode preserves wire bytes"
    (C.encode (ok (C.parse sample)) = sample)

let claims () =
  let empty = encode (B.Map []) in
  List.iter
    (fun value ->
      let bad = encode (B.Map [ (int 4, value) ]) in
      check "invalid expiry rejected" (Result.is_error (C.Claims.of_cbor bad));
      check "unsigned expiry injection"
        (not (valid (envelope header bad (mac header empty)))))
    [
      B.Text "900";
      B.Null;
      B.Tag (1, int 900);
      B.Int (Z.shift_left Z.one 100);
      B.Float infinity;
      B.Float nan;
    ];
  let fractional = encode (B.Map [ (int 4, B.Float 1000.5) ]) in
  check "fractional NumericDate" (valid (signed header fractional));
  check "fractional expiry stays signed"
    (not (valid (envelope header fractional (mac header empty))));
  check "expiration equality"
    (not (valid (signed header (encode (B.Map [ (int 4, int 1000) ])))));
  check "fractional past expiry"
    (not (valid (signed header (encode (B.Map [ (int 4, B.Float 999.5) ])))));
  check "not-before equality"
    (valid (signed header (encode (B.Map [ (int 5, int 1000) ]))));
  check "future not-before"
    (not (valid (signed header (encode (B.Map [ (int 5, B.Float 1000.5) ])))));
  List.iter
    (fun fields ->
      check "malformed claim"
        (Result.is_error (C.Claims.of_cbor (encode (B.Map fields)))))
    [
      [ (int 1, B.Text "\255") ];
      [ (int 1, B.Text "https://bad host") ];
      [ (int 2, int 1) ];
      [ (int 3, B.Array [ B.Text "a"; int 1 ]) ];
      [ (int 7, B.Text "cti must be bytes") ];
      [ (B.Int (Z.shift_left Z.one 100), B.Bool true) ];
    ];
  let uri = "https://EXAMPLE/%41" in
  let original =
    C.Claims.(
      empty |> set_iss uri
      |> set_exp (Option.get (Ptime.of_float_s 1060.5))
      |> set_int_key 42 (int 1)
      |> set_int_key 42 (int 2)
      |> build)
  in
  let key = C.Cose_key.with_kid "key-id" key in
  let created =
    ok (C.create ~key ~algorithm:C.Algorithm.HMAC_256 ~claims:original)
  in
  let parsed = ok (C.parse (C.encode created)) in
  check "fractional create/parse identity"
    (C.Claims.exp original = C.Claims.exp (C.claims parsed));
  check "key ID create/parse identity" (C.kid parsed = Some "key-id");
  check "StringOrURI identity" (C.Claims.iss (C.claims parsed) = Some uri);
  check "setters replace" (C.Claims.get_int_key 42 original = Some (int 2));
  check "negative leeway"
    (try
       ignore (C.validate ~now ~leeway:(Ptime.Span.of_int_s (-1)) parsed);
       false
     with Invalid_argument _ -> true)

let headers_keys () =
  List.iter
    (fun protected ->
      check "malformed or unsupported header"
        (not (valid (signed (encode (B.Map protected)) payload))))
    [
      [ (int 1, int 5); (int 2, B.Array [ int 100 ]); (int 100, B.Bool true) ];
      [ (int 1, int 5); (int 1, int 7) ];
      [ (int 1, int 5); (int 4, B.Text "key") ];
      [ (int 1, int 5); (int 3, B.Bool true) ];
      [ (int 1, int 5); (int 7, B.Array []) ];
      [ (int 1, int 5); (int 5, B.Bytes "iv") ];
      [ (int 1, B.Int (Z.shift_left Z.one 100)) ];
    ];
  check "unprotected map required"
    (not (valid (signed ~unprotected:B.Null header payload)));
  let protected =
    encode (B.Map [ (int 1, int 5); (int 4, B.Bytes "trusted") ])
  in
  check "unprotected cannot override protected"
    (not
       (valid
          (signed protected payload
             ~unprotected:(B.Map [ (int 4, B.Bytes "attacker") ]))));
  check "unprotected duplicate"
    (not
       (valid
          (signed header payload
             ~unprotected:(B.Map [ (int 4, B.Bytes "a"); (int 4, B.Bytes "b") ]))));
  check "key algorithm binding"
    (not (valid ~key:(C.Cose_key.with_alg C.Algorithm.HMAC_512 key) sample));
  check "unbound HMAC key"
    (not (valid ~key:(C.Cose_key.symmetric secret) sample));
  let key_fields =
    [ (int 1, int 4); (int (-1), B.Bytes secret); (int 3, int 5) ]
  in
  let restricted =
    ok
      (C.Cose_key.of_cbor
         (encode (B.Map (key_fields @ [ (int 4, B.Array [ int 9 ]) ]))))
  in
  check "MAC-create-only cannot verify" (not (valid ~key:restricted sample));
  let restricted =
    ok
      (C.Cose_key.of_cbor
         (encode (B.Map (key_fields @ [ (int 4, B.Array [ int 10 ]) ]))))
  in
  check "MAC-verify-only cannot sign"
    (Result.is_error
       (C.create ~key:restricted ~algorithm:C.Algorithm.HMAC_256
          ~claims:(C.Claims.build C.Claims.empty)));
  let roundtrip = ok (C.Cose_key.of_cbor (C.Cose_key.to_cbor restricted)) in
  check "key_ops retained by serialization"
    (Result.is_error
       (C.create ~key:roundtrip ~algorithm:C.Algorithm.HMAC_256
          ~claims:(C.Claims.build C.Claims.empty)));
  List.iter
    (fun fields ->
      check "bad key metadata"
        (Result.is_error (C.Cose_key.of_cbor (encode (B.Map fields)))))
    [
      key_fields @ [ (int 2, B.Text "wrong type") ];
      key_fields @ [ (int 4, B.Array []) ];
      key_fields @ [ (int 5, B.Bytes "unsupported IV") ];
      key_fields @ [ (int 3, int 7) ];
      key_fields @ [ (int 4, B.Array [ int 9; int 9 ]) ];
      [ (int 1, int 4); (int (-1), B.Bytes secret); (int 3, int 999) ];
    ];
  let short =
    C.Cose_key.symmetric "" |> C.Cose_key.with_alg C.Algorithm.HMAC_256
  in
  check "short HMAC key"
    (Result.is_error
       (C.create ~key:short ~algorithm:C.Algorithm.HMAC_256
          ~claims:(C.Claims.build C.Claims.empty)));
  check "invalid curve point"
    (try
       ignore
         (C.Cose_key.p256_pub ~x:(String.make 32 '\000')
            ~y:(String.make 32 '\000'));
       false
     with Invalid_argument _ -> true)

let bounds () =
  List.iter
    (fun raw -> check "bounded malformed input" (Result.is_error (C.parse raw)))
    [
      "\x5b\x00\x00\x00\x01\x00\x00\x00\x00";
      "\x5b\x40\x00\x00\x00\x00\x00\x00\x00";
      "\x5b\xff\xff\xff\xff\xff\xff\xff\xff";
      String.make 100000 '\x81' ^ "\xf6";
      sample ^ "trailing";
    ];
  check "invalid indefinite integer"
    (Result.is_error (C.Claims.of_cbor "\xa1\x04\x1f"));
  check "tagged NumericDate preserves tag and rejects it"
    (Result.is_error (C.Claims.of_cbor "\xa1\x04\xc2\x42\x03\xe8"));
  check "nonminimal valid integer accepted"
    (Result.is_ok (C.Claims.of_cbor "\xa1\x04\x1a\x00\x00\x03\xe8"));
  let nested n = "\xa1\x18\x2a" ^ String.make n '\x81' ^ "\xf6" in
  check "depth boundary accepted" (Result.is_ok (C.Claims.of_cbor (nested 31)));
  check "depth excess rejected" (Result.is_error (C.Claims.of_cbor (nested 32)));
  check "item limit"
    (Result.is_error
       (C.Claims.of_cbor ("\xa1\x18\x2a\x99\x10\x00" ^ String.make 4096 '\xf6')));
  check "UTF-8 chunk boundary"
    (Result.is_error (C.Claims.of_cbor "\xa1\x01\x7f\x61\xc3\x61\xa9\xff"));
  check "size limit" (Result.is_error (C.parse ~max_size:10 sample))

let () =
  Alcotest.run "CWT security"
    [
      ( "regressions",
        List.map
          (fun (name, f) -> Alcotest.test_case name `Quick f)
          [
            ("wire authentication", wire);
            ("strict claims", claims);
            ("headers and keys", headers_keys);
            ("CBOR bounds and syntax", bounds);
          ] );
    ]

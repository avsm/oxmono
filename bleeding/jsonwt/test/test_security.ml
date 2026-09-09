(* These standalone tests own their domains and do not use a thread pool. *)
[@@@alert "-do_not_spawn_domains"]

module J = Jsonwt
let key_bytes = String.make 64 'k'
let key = J.Jwk.symmetric key_bytes |> J.Jwk.with_alg J.Algorithm.HS256
let now = Option.get (Ptime.of_float_s 1000.)
let check label b = Alcotest.(check bool) label true b
let ok = function Ok x -> x | Error e -> failwith (J.error_to_string e)
let signed header payload =
  let input = J.base64url_encode header ^ "." ^ J.base64url_encode payload in
  let signature = Digestif.SHA256.hmac_string ~key:key_bytes input
      |> Digestif.SHA256.to_raw_string in
  input ^ "." ^ J.base64url_encode signature
let token payload = signed {|{"alg":"HS256"}|} payload
let verified raw =
  match J.parse raw with
  | Error _ -> false
  | Ok t -> Result.is_ok (J.verify_and_validate ~key ~now
      ~allowed_algs:[J.Algorithm.HS256] t)

let strict_json () =
  List.iter (fun header -> check "invalid JOSE header rejected"
      (not (verified (signed header {|{"exp":1060}|}))))
    [{|{"alg":"HS256","crit":["custom"],"custom":true}|};
     {|{"alg":"HS256","crit":[]}|};
     {|{"alg":"HS256","crit":false}|};
     {|{"alg":"HS256","b64":false}|};
     {|{"alg":"HS256","b64":true}|};
     {|{"alg":"HS256","alg":"HS512"}|};
     {|{"alg":"HS256","\u0061lg":"HS256"}|};
     {|{"alg":"HS256","typ":4}|}; {|{"alg":4}|};
     {|{"alg":"HS256","kid":null}|};
     {|{"alg":"HS256","cty":"JWT"}|}];
  List.iter (fun payload -> check "malformed claims rejected"
      (not (verified (token payload))))
    [{|{"exp":"900"}|}; {|{"exp":1e100}|}; {|{"exp":null}|};
     {|{"nbf":"2000"}|}; {|{"iat":false}|};
     {|{"aud":["service",4]}|}; {|{"aud":null}|};
     {|{"iss":42}|}; {|{"sub":"https://bad host"}|};
     {|{"iss":"x:%zz"}|}; {|{"jti":false}|};
     {|{"exp":900,"exp":1060}|};
     {|{"custom":{"a":1,"a":2}}|}; "[]"; "null";
     "{\"x\":\"\255\"}"; "{\"\255\":1}"; {|{"x":"\uD800"}|}];
  check "valid URI retains exact case and escapes"
    (J.Claims.iss (ok (J.Claims.of_json {|{"iss":"https://EXAMPLE/%41"}|}))
       = Some "https://EXAMPLE/%41");
  check "registered claims retain original JSON numbers"
    (J.Claims.get_number "exp"
       (ok (J.Claims.of_json {|{"exp":1060.125}|})) = Some 1060.125);
  check "fractional custom integer is not truncated"
    (J.Claims.get_int "n" (ok (J.Claims.of_json {|{"n":1.5}|})) = None);
  check "custom integer does not wrap on overflow"
    (J.Claims.get_int "n" (ok (J.Claims.of_json {|{"n":1e100}|})) = None);
  let claims = J.Claims.(empty |> set_string "x" "old"
      |> set_string "x" "new" |> build) in
  check "builder replaces a member"
    (J.Claims.get_string "x" claims = Some "new")

let times () =
  check "expired at equality" (not (verified (token {|{"exp":1000}|})));
  check "fractional future expiration" (verified (token {|{"exp":1000.125}|}));
  check "not-before equality allowed" (verified (token {|{"nbf":1000}|}));
  check "future not-before rejected"
    (not (verified (token {|{"nbf":1000.125}|})));
  let t = ok (J.parse (token {|{"exp":1000}|})) in
  check "helper agrees with expiration boundary" (J.is_expired ~now t);
  check "positive leeway accepted" (Result.is_ok
    (J.validate ~now ~leeway:(Ptime.Span.of_int_s 1) t));
  check "leeway equality expires" (Result.is_error
    (J.validate ~now:(Option.get (Ptime.of_float_s 1001.))
      ~leeway:(Ptime.Span.of_int_s 1) t));
  check "negative leeway rejected"
    (try ignore (J.validate ~now ~leeway:(Ptime.Span.of_int_s (-1)) t); false
     with Invalid_argument _ -> true)

let encoding () =
  let raw = token {|{"exp":1060}|} in
  check "original compact bytes retained" (J.encode (ok (J.parse raw)) = raw);
  check "padded signature rejected" (not (verified (raw ^ "=")));
  List.iter (fun s -> check "base64url rejects noncanonical input"
      (Result.is_error (J.base64url_decode s)))
    ["Zh"; "Zm9"; "Zg=="; "Z g"; "+/"; "A"; "\255"];
  List.iter (fun s -> check "base64url roundtrip"
      (J.base64url_decode (J.base64url_encode s) = Ok s))
    [""; "a"; "ab"; "abc"; String.init 256 Char.chr];
  check "token bound" (Result.is_error (J.parse ~max_size:10 raw));
  check "many components rejected"
    (Result.is_error (J.parse (String.make 8192 '.')));
  let nested n =
    {|{"x":|} ^ String.make n '[' ^ "0" ^ String.make n ']' ^ "}" in
  check "depth limit accepts boundary"
    (Result.is_ok (J.Claims.of_json (nested 31)));
  check "depth limit rejects excess"
    (Result.is_error (J.Claims.of_json (nested 32)));
  check "brackets in strings do not count"
    (Result.is_ok (J.Claims.of_json {|{"x":"[[[[\"[[["}|}));
  check "JSON size bound"
    (Result.is_error (J.Claims.of_json
      ("{\"x\":\"" ^ String.make 65536 'x' ^ "\"}")))

let policies () =
  let t = ok (J.parse (token {|{"exp":1060}|})) in
  check "key algorithm enforced" (Result.is_error (J.verify
    ~key:(J.Jwk.with_alg J.Algorithm.HS512 key)
    ~allowed_algs:[J.Algorithm.HS256] t));
  check "allowlist enforced"
    (Result.is_error (J.verify ~key ~allowed_algs:[] t));
  check "unbound HMAC key refused" (Result.is_error (J.verify
    ~key:(J.Jwk.symmetric key_bytes) ~allowed_algs:[J.Algorithm.HS256] t));
  check "short HMAC key refused" (Result.is_error (J.verify
    ~key:(J.Jwk.symmetric "weak" |> J.Jwk.with_alg J.Algorithm.HS256)
    ~allowed_algs:[J.Algorithm.HS256] t));
  let jwk = J.Jwk.to_json key in
  let restricted = String.sub jwk 0 (String.length jwk - 1)
      ^ ",\"key_ops\":[\"sign\"]}" in
  check "key_ops enforced" (Result.is_error (J.verify
    ~key:(ok (J.Jwk.of_json restricted)) ~allowed_algs:[J.Algorithm.HS256] t));
  List.iter (fun extra -> check "invalid JWK policy rejected"
    (Result.is_error (J.Jwk.of_json
      (String.sub jwk 0 (String.length jwk - 1) ^ "," ^ extra ^ "}"))))
    ["\"use\":\"enc\""; "\"x5c\":[]";
     "\"key_ops\":[\"verify\",\"verify\"]"; "\"kid\":42"];
  check "unknown JWK algorithm rejected" (Result.is_error
    (J.Jwk.of_json {|{"kty":"oct","k":"a2V5","alg":"unknown"}|}));
  check "duplicate JWK members rejected" (Result.is_error
    (J.Jwk.of_json {|{"kty":"oct","kty":"RSA","k":"a2V5"}|}));
  let unsecured = ok (J.parse
      (J.base64url_encode {|{"alg":"none"}|} ^ ".e30.")) in
  check "none needs separate opt-in" (Result.is_error
    (J.verify ~key ~allowed_algs:[J.Algorithm.None] unsecured));
  check "none still needs allowlist entry" (Result.is_error
    (J.verify ~key ~allow_none:true
      ~allowed_algs:[J.Algorithm.HS256] unsecured));
  check "explicit unsecured opt-in works" (Result.is_ok
    (J.verify ~key ~allow_none:true
      ~allowed_algs:[J.Algorithm.None] unsecured));
  check "none creation needs opt-in" (Result.is_error
    (J.create ~key ~header:(J.Header.make J.Algorithm.None)
      ~claims:(J.Claims.build J.Claims.empty) ()))

let members = function Jsont.Object (m, _) -> m | _ -> assert false
let field name obj = snd (List.find (fun ((n, _), _) -> n = name) (members obj))
let string = function Jsont.String (s, _) -> s | _ -> assert false
let json value = Result.get_ok (Jsont_bytesrw.encode_string Jsont.json value)

let openssl () =
  let text = In_channel.with_open_bin "fixtures.json" In_channel.input_all in
  let fixtures =
    match Result.get_ok (Jsont_bytesrw.decode_string Jsont.json text)
    with Jsont.Array (values, _) -> values | _ -> assert false in
  List.iter (fun fixture ->
    let name = string (field "alg" fixture) in
    let alg = ok (J.Algorithm.of_string name) in
    let key = ok (J.Jwk.of_json (json (field "jwk" fixture))) in
    let raw = string (field "token" fixture) in
    let t = ok (J.parse raw) in
    check (name ^ " OpenSSL signature") (Result.is_ok (J.verify_and_validate
      ~key ~now ~iss:"did:plc:fixture" ~aud:"did:web:spindle.test"
      ~allowed_algs:[alg] t));
    check (name ^ " JWK roundtrip") (Result.is_ok (J.verify
      ~key:(ok (J.Jwk.of_json (J.Jwk.to_json key))) ~allowed_algs:[alg] t));
    let input = String.sub raw 0 (String.rindex raw '.') in
    List.iter (fun signature ->
      let bad = input ^ "." ^ J.base64url_encode signature in
      check (name ^ " signature length") (Result.is_error
        (J.verify ~key ~allowed_algs:[alg] (ok (J.parse bad)))))
      [""; "short"; J.signature t ^ "x";
       String.make (String.length (J.signature t)) '\000'];
    let malformed name value =
      let fields = members (field "jwk" fixture) in
      let fields = List.filter (fun ((n, _), _) -> n <> name) fields in
      json (Jsont.Object (((name, Jsont.Meta.none),
        Jsont.Json.string value) :: fields, Jsont.Meta.none)) in
    List.iter (fun (field, value) ->
      check (name ^ " invalid JWK " ^ field)
        (Result.is_error (J.Jwk.of_json (malformed field value))))
      ["alg", "HS256"; "crv", "unsupported";
       "x", J.base64url_encode "short"];
    let damaged = Bytes.of_string (J.signature t) in
    Bytes.set damaged 0 (Char.chr (Char.code (Bytes.get damaged 0) lxor 1));
    let bad = input ^ "." ^ J.base64url_encode (Bytes.to_string damaged) in
    check (name ^ " tampered signature") (Result.is_error
      (J.verify ~key ~allowed_algs:[alg] (ok (J.parse bad))));
    if alg = J.Algorithm.ES256K then begin
      let verify_shared = (fun () ->
        for _ = 1 to 100 do
          if Result.is_error
              (J.verify ~key ~allowed_algs:[J.Algorithm.ES256K] t) then
            failwith "parallel ES256K verification"
        done : (unit -> unit) @ portable) in
      let domains = List.init 4 (fun _ -> Domain.Safe.spawn verify_shared) in
      List.iter Domain.join domains;
      let compressed =
        ok (J.base64url_decode (string (field "compressed" fixture))) in
      check "compressed secp256k1 point" (Result.is_ok (J.verify
        ~key:(ok (J.Jwk.secp256k1_pub compressed)) ~allowed_algs:[alg] t));
      List.iter (fun point -> check "invalid secp256k1 point"
        (Result.is_error (J.Jwk.secp256k1_pub point)))
        [""; "\000"; "\002" ^ String.make 32 '\255';
         "\004" ^ String.make 64 '\000'; "\006" ^ String.make 64 '\000']
    end) fixtures

let () = Alcotest.run "JSONWT security"
  ["regressions", List.map (fun (name, f) -> Alcotest.test_case name `Quick f)
    ["strict JSON and JOSE", strict_json; "NumericDate boundaries", times;
     "canonical encoding and bounds", encoding;
     "key and algorithm policies", policies;
     "independent OpenSSL vectors", openssl]]

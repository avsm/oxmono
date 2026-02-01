(** CBOR Encoding Tests

    Tests derived from RFC 8949 Appendix A (Examples of Encoded CBOR Data
    Items). *)

(* Helper to encode to hex string *)
let encode_to_hex f =
  let buf = Buffer.create 64 in
  let writer = Bytesrw.Bytes.Writer.of_buffer buf in
  let enc = Cbort.Rw.make_encoder writer in
  f enc;
  Cbort.Rw.flush_encoder enc;
  let bytes = Buffer.contents buf in
  String.concat ""
    (List.init (String.length bytes) (fun i ->
         Printf.sprintf "%02x" (Char.code (String.get bytes i))))

(* Helper to convert hex string to bytes for comparison *)
let hex_to_bytes hex =
  let hex = String.lowercase_ascii hex in
  let len = String.length hex / 2 in
  let buf = Bytes.create len in
  for i = 0 to len - 1 do
    let byte = int_of_string ("0x" ^ String.sub hex (i * 2) 2) in
    Bytes.set_uint8 buf i byte
  done;
  Bytes.to_string buf

(* ============= Integer Tests (RFC 8949 Appendix A) ============= *)

let test_uint_0 () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_int enc 0) in
  Alcotest.(check string) "0" "00" hex

let test_uint_1 () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_int enc 1) in
  Alcotest.(check string) "1" "01" hex

let test_uint_10 () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_int enc 10) in
  Alcotest.(check string) "10" "0a" hex

let test_uint_23 () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_int enc 23) in
  Alcotest.(check string) "23" "17" hex

let test_uint_24 () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_int enc 24) in
  Alcotest.(check string) "24" "1818" hex

let test_uint_25 () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_int enc 25) in
  Alcotest.(check string) "25" "1819" hex

let test_uint_100 () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_int enc 100) in
  Alcotest.(check string) "100" "1864" hex

let test_uint_1000 () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_int enc 1000) in
  Alcotest.(check string) "1000" "1903e8" hex

let test_uint_1000000 () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_int enc 1000000) in
  Alcotest.(check string) "1000000" "1a000f4240" hex

let test_uint_1000000000000 () =
  let hex =
    encode_to_hex (fun enc -> Cbort.Rw.write_int64 enc 1000000000000L)
  in
  Alcotest.(check string) "1000000000000" "1b000000e8d4a51000" hex

(* ============= Negative Integer Tests ============= *)

let test_nint_minus1 () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_int enc (-1)) in
  Alcotest.(check string) "-1" "20" hex

let test_nint_minus10 () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_int enc (-10)) in
  Alcotest.(check string) "-10" "29" hex

let test_nint_minus100 () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_int enc (-100)) in
  Alcotest.(check string) "-100" "3863" hex

let test_nint_minus1000 () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_int enc (-1000)) in
  Alcotest.(check string) "-1000" "3903e7" hex

(* ============= Boolean and Null Tests ============= *)

let test_false () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_bool enc false) in
  Alcotest.(check string) "false" "f4" hex

let test_true () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_bool enc true) in
  Alcotest.(check string) "true" "f5" hex

let test_null () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_null enc) in
  Alcotest.(check string) "null" "f6" hex

(* ============= Float Tests ============= *)

(* Note: RFC 8949 deterministic encoding uses the smallest float representation
   that preserves the value. Values like 1.0, infinity, and NaN can be represented
   exactly in half precision (16-bit), so they use f9 prefix. *)

let test_float_1_0 () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_float enc 1.0) in
  (* Half precision 1.0 = 0xf93c00 per RFC 8949 deterministic encoding *)
  Alcotest.(check string) "1.0" "f93c00" hex

let test_float_1_1 () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_float enc 1.1) in
  (* 1.1 cannot be exactly represented in half precision, uses double *)
  (* RFC: 0xfb3ff199999999999a *)
  Alcotest.(check string) "1.1" "fb3ff199999999999a" hex

let test_float_neg_4_1 () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_float enc (-4.1)) in
  (* -4.1 cannot be exactly represented in half precision, uses double *)
  (* RFC: 0xfbc010666666666666 *)
  Alcotest.(check string) "-4.1" "fbc010666666666666" hex

let test_float_1e300 () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_float enc 1.0e300) in
  (* 1.0e300 exceeds half/single precision range, uses double *)
  (* RFC: 0xfb7e37e43c8800759c *)
  Alcotest.(check string) "1.0e+300" "fb7e37e43c8800759c" hex

let test_float_infinity () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_float enc infinity) in
  (* Half precision infinity = 0xf97c00 per RFC 8949 deterministic encoding *)
  Alcotest.(check string) "Infinity" "f97c00" hex

let test_float_neg_infinity () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_float enc neg_infinity) in
  (* Half precision -infinity = 0xf9fc00 per RFC 8949 deterministic encoding *)
  Alcotest.(check string) "-Infinity" "f9fc00" hex

let test_float_nan () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_float enc nan) in
  (* Half precision NaN = 0xf97e00 per RFC 8949 deterministic encoding *)
  Alcotest.(check string) "NaN" "f97e00" hex

(* ============= Text String Tests ============= *)

let test_text_empty () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_text enc "") in
  Alcotest.(check string) "empty string" "60" hex

let test_text_a () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_text enc "a") in
  Alcotest.(check string) "\"a\"" "6161" hex

let test_text_ietf () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_text enc "IETF") in
  Alcotest.(check string) "\"IETF\"" "6449455446" hex

let test_text_quote_backslash () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_text enc "\"\\") in
  Alcotest.(check string) "\"\\\"\\\\\"" "62225c" hex

let test_text_utf8_umlaut () =
  (* U+00FC = ü = 0xc3 0xbc in UTF-8 *)
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_text enc "\xc3\xbc") in
  Alcotest.(check string) "ü" "62c3bc" hex

let test_text_utf8_water () =
  (* U+6C34 = 水 = 0xe6 0xb0 0xb4 in UTF-8 *)
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_text enc "\xe6\xb0\xb4") in
  Alcotest.(check string) "水" "63e6b0b4" hex

let test_text_utf8_emoji () =
  (* U+10151 = 𐅑 = 0xf0 0x90 0x85 0x91 in UTF-8 *)
  let hex =
    encode_to_hex (fun enc -> Cbort.Rw.write_text enc "\xf0\x90\x85\x91")
  in
  Alcotest.(check string) "𐅑" "64f0908591" hex

(* ============= Byte String Tests ============= *)

let test_bytes_empty () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_bytes_header enc 0) in
  Alcotest.(check string) "empty bytes" "40" hex

let test_bytes_01020304 () =
  let hex =
    encode_to_hex (fun enc ->
        Cbort.Rw.write_bytes_header enc 4;
        Cbort.Rw.write_bytes enc (hex_to_bytes "01020304"))
  in
  Alcotest.(check string) "h'01020304'" "4401020304" hex

(* ============= Array Tests ============= *)

let test_array_empty () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_array_start enc 0) in
  Alcotest.(check string) "[]" "80" hex

let test_array_123 () =
  let hex =
    encode_to_hex (fun enc ->
        Cbort.Rw.write_array_start enc 3;
        Cbort.Rw.write_int enc 1;
        Cbort.Rw.write_int enc 2;
        Cbort.Rw.write_int enc 3)
  in
  Alcotest.(check string) "[1, 2, 3]" "83010203" hex

let test_array_nested () =
  (* [1, [2, 3], [4, 5]] *)
  let hex =
    encode_to_hex (fun enc ->
        Cbort.Rw.write_array_start enc 3;
        Cbort.Rw.write_int enc 1;
        Cbort.Rw.write_array_start enc 2;
        Cbort.Rw.write_int enc 2;
        Cbort.Rw.write_int enc 3;
        Cbort.Rw.write_array_start enc 2;
        Cbort.Rw.write_int enc 4;
        Cbort.Rw.write_int enc 5)
  in
  Alcotest.(check string) "[1, [2, 3], [4, 5]]" "8301820203820405" hex

let test_array_25_items () =
  (* [1, 2, 3, ..., 25] - requires 1-byte length encoding *)
  let hex =
    encode_to_hex (fun enc ->
        Cbort.Rw.write_array_start enc 25;
        for i = 1 to 25 do
          Cbort.Rw.write_int enc i
        done)
  in
  (* 0x98 0x19 = array with 1-byte length (25) *)
  Alcotest.(check string)
    "[1..25]" "98190102030405060708090a0b0c0d0e0f101112131415161718181819" hex

(* ============= Map Tests ============= *)

let test_map_empty () =
  let hex = encode_to_hex (fun enc -> Cbort.Rw.write_map_start enc 0) in
  Alcotest.(check string) "{}" "a0" hex

let test_map_int_keys () =
  (* {1: 2, 3: 4} *)
  let hex =
    encode_to_hex (fun enc ->
        Cbort.Rw.write_map_start enc 2;
        Cbort.Rw.write_int enc 1;
        Cbort.Rw.write_int enc 2;
        Cbort.Rw.write_int enc 3;
        Cbort.Rw.write_int enc 4)
  in
  Alcotest.(check string) "{1: 2, 3: 4}" "a201020304" hex

let test_map_string_keys () =
  (* {"a": 1, "b": [2, 3]} *)
  let hex =
    encode_to_hex (fun enc ->
        Cbort.Rw.write_map_start enc 2;
        Cbort.Rw.write_text enc "a";
        Cbort.Rw.write_int enc 1;
        Cbort.Rw.write_text enc "b";
        Cbort.Rw.write_array_start enc 2;
        Cbort.Rw.write_int enc 2;
        Cbort.Rw.write_int enc 3)
  in
  Alcotest.(check string) "{\"a\": 1, \"b\": [2, 3]}" "a26161016162820203" hex

let test_mixed_array_map () =
  (* ["a", {"b": "c"}] *)
  let hex =
    encode_to_hex (fun enc ->
        Cbort.Rw.write_array_start enc 2;
        Cbort.Rw.write_text enc "a";
        Cbort.Rw.write_map_start enc 1;
        Cbort.Rw.write_text enc "b";
        Cbort.Rw.write_text enc "c")
  in
  Alcotest.(check string) "[\"a\", {\"b\": \"c\"}]" "826161a161626163" hex

let test_map_5_pairs () =
  (* {"a": "A", "b": "B", "c": "C", "d": "D", "e": "E"} *)
  let hex =
    encode_to_hex (fun enc ->
        Cbort.Rw.write_map_start enc 5;
        Cbort.Rw.write_text enc "a";
        Cbort.Rw.write_text enc "A";
        Cbort.Rw.write_text enc "b";
        Cbort.Rw.write_text enc "B";
        Cbort.Rw.write_text enc "c";
        Cbort.Rw.write_text enc "C";
        Cbort.Rw.write_text enc "d";
        Cbort.Rw.write_text enc "D";
        Cbort.Rw.write_text enc "e";
        Cbort.Rw.write_text enc "E")
  in
  Alcotest.(check string)
    "{a:A, b:B, c:C, d:D, e:E}" "a56161614161626142616361436164614461656145" hex

(* ============= Tag Tests ============= *)

let test_tag_epoch_timestamp () =
  (* 1(1363896240) - epoch-based date/time *)
  let hex =
    encode_to_hex (fun enc ->
        Cbort.Rw.write_type_arg enc Cbort.Rw.major_tag 1;
        Cbort.Rw.write_int enc 1363896240)
  in
  Alcotest.(check string) "1(1363896240)" "c11a514b67b0" hex

(* ============= Major Type Constants Test ============= *)

let test_major_type_constants () =
  Alcotest.(check int) "major_uint" 0 Cbort.Rw.major_uint;
  Alcotest.(check int) "major_nint" 1 Cbort.Rw.major_nint;
  Alcotest.(check int) "major_bytes" 2 Cbort.Rw.major_bytes;
  Alcotest.(check int) "major_text" 3 Cbort.Rw.major_text;
  Alcotest.(check int) "major_array" 4 Cbort.Rw.major_array;
  Alcotest.(check int) "major_map" 5 Cbort.Rw.major_map;
  Alcotest.(check int) "major_tag" 6 Cbort.Rw.major_tag;
  Alcotest.(check int) "major_simple" 7 Cbort.Rw.major_simple

let test_simple_value_constants () =
  Alcotest.(check int) "simple_false" 20 Cbort.Rw.simple_false;
  Alcotest.(check int) "simple_true" 21 Cbort.Rw.simple_true;
  Alcotest.(check int) "simple_null" 22 Cbort.Rw.simple_null;
  Alcotest.(check int) "simple_undefined" 23 Cbort.Rw.simple_undefined

let test_additional_info_constants () =
  Alcotest.(check int) "ai_1byte" 24 Cbort.Rw.ai_1byte;
  Alcotest.(check int) "ai_2byte" 25 Cbort.Rw.ai_2byte;
  Alcotest.(check int) "ai_4byte" 26 Cbort.Rw.ai_4byte;
  Alcotest.(check int) "ai_8byte" 27 Cbort.Rw.ai_8byte;
  Alcotest.(check int) "ai_indefinite" 31 Cbort.Rw.ai_indefinite

(* ============= High-level Codec API Tests ============= *)

(* Round-trip tests using Cbort.encode_string and Cbort.decode_string *)

let test_codec_int_roundtrip () =
  let values = [ 0; 1; 23; 24; 100; 1000; 1000000; -1; -10; -100; -1000 ] in
  List.iter
    (fun v ->
      let encoded = Cbort.encode_string Cbort.int v in
      match Cbort.decode_string Cbort.int encoded with
      | Ok decoded -> Alcotest.(check int) (Printf.sprintf "int %d" v) v decoded
      | Error e -> Alcotest.fail (Cbort.Error.to_string e))
    values

let test_codec_int64_roundtrip () =
  let values = [ 0L; 1L; 1000000000000L; -1L; Int64.max_int; Int64.min_int ] in
  List.iter
    (fun v ->
      let encoded = Cbort.encode_string Cbort.int64 v in
      match Cbort.decode_string Cbort.int64 encoded with
      | Ok decoded ->
          Alcotest.(check int64) (Printf.sprintf "int64 %Ld" v) v decoded
      | Error e -> Alcotest.fail (Cbort.Error.to_string e))
    values

let test_codec_bool_roundtrip () =
  List.iter
    (fun v ->
      let encoded = Cbort.encode_string Cbort.bool v in
      match Cbort.decode_string Cbort.bool encoded with
      | Ok decoded ->
          Alcotest.(check bool) (Printf.sprintf "bool %b" v) v decoded
      | Error e -> Alcotest.fail (Cbort.Error.to_string e))
    [ true; false ]

let test_codec_null_roundtrip () =
  let encoded = Cbort.encode_string Cbort.null () in
  match Cbort.decode_string Cbort.null encoded with
  | Ok () -> ()
  | Error e -> Alcotest.fail (Cbort.Error.to_string e)

let test_codec_float_roundtrip () =
  let values = [ 0.0; 1.0; -1.0; 1.5; 3.14159; 1e10; -1e-10 ] in
  List.iter
    (fun v ->
      let encoded = Cbort.encode_string Cbort.float v in
      match Cbort.decode_string Cbort.float encoded with
      | Ok decoded ->
          let diff = abs_float (v -. decoded) in
          Alcotest.(check bool) (Printf.sprintf "float %g" v) true (diff < 1e-10)
      | Error e -> Alcotest.fail (Cbort.Error.to_string e))
    values

let test_codec_string_roundtrip () =
  let values =
    [ ""; "a"; "hello"; "UTF-8: \xc3\xbc \xe6\xb0\xb4"; "with\nnewline" ]
  in
  List.iter
    (fun v ->
      let encoded = Cbort.encode_string Cbort.string v in
      match Cbort.decode_string Cbort.string encoded with
      | Ok decoded ->
          Alcotest.(check string) (Printf.sprintf "string %S" v) v decoded
      | Error e -> Alcotest.fail (Cbort.Error.to_string e))
    values

let test_codec_bytes_roundtrip () =
  let values = [ ""; "\x00\x01\x02\x03"; String.make 100 '\xff' ] in
  List.iter
    (fun v ->
      let encoded = Cbort.encode_string Cbort.bytes v in
      match Cbort.decode_string Cbort.bytes encoded with
      | Ok decoded -> Alcotest.(check string) "bytes" v decoded
      | Error e -> Alcotest.fail (Cbort.Error.to_string e))
    values

let test_codec_array_roundtrip () =
  let values = [ []; [ 1 ]; [ 1; 2; 3 ]; List.init 25 (fun i -> i) ] in
  let int_list = Cbort.array Cbort.int in
  List.iter
    (fun v ->
      let encoded = Cbort.encode_string int_list v in
      match Cbort.decode_string int_list encoded with
      | Ok decoded -> Alcotest.(check (list int)) "array" v decoded
      | Error e -> Alcotest.fail (Cbort.Error.to_string e))
    values

let test_codec_nested_array () =
  let nested = Cbort.array (Cbort.array Cbort.int) in
  let v = [ [ 1; 2 ]; [ 3; 4; 5 ]; [] ] in
  let encoded = Cbort.encode_string nested v in
  match Cbort.decode_string nested encoded with
  | Ok decoded -> Alcotest.(check (list (list int))) "nested array" v decoded
  | Error e -> Alcotest.fail (Cbort.Error.to_string e)

let test_codec_string_map_roundtrip () =
  let map = Cbort.string_map Cbort.int in
  let v = [ ("a", 1); ("b", 2); ("c", 3) ] in
  let encoded = Cbort.encode_string map v in
  match Cbort.decode_string map encoded with
  | Ok decoded ->
      (* Maps may reorder, so sort before comparing *)
      let sort = List.sort compare in
      Alcotest.(check (list (pair string int)))
        "string map" (sort v) (sort decoded)
  | Error e -> Alcotest.fail (Cbort.Error.to_string e)

let test_codec_int_map_roundtrip () =
  let map = Cbort.int_map Cbort.string in
  let v = [ (1, "one"); (2, "two"); (3, "three") ] in
  let encoded = Cbort.encode_string map v in
  match Cbort.decode_string map encoded with
  | Ok decoded ->
      let sort = List.sort compare in
      Alcotest.(check (list (pair int string)))
        "int map" (sort v) (sort decoded)
  | Error e -> Alcotest.fail (Cbort.Error.to_string e)

let test_codec_tuple2 () =
  let codec = Cbort.tuple2 Cbort.string Cbort.int in
  let v = ("hello", 42) in
  let encoded = Cbort.encode_string codec v in
  match Cbort.decode_string codec encoded with
  | Ok decoded -> Alcotest.(check (pair string int)) "tuple2" v decoded
  | Error e -> Alcotest.fail (Cbort.Error.to_string e)

let test_codec_tuple3 () =
  let codec = Cbort.tuple3 Cbort.int Cbort.string Cbort.bool in
  let v = (42, "hello", true) in
  let encoded = Cbort.encode_string codec v in
  match Cbort.decode_string codec encoded with
  | Ok decoded ->
      let a, b, c = decoded in
      Alcotest.(check int) "tuple3.0" 42 a;
      Alcotest.(check string) "tuple3.1" "hello" b;
      Alcotest.(check bool) "tuple3.2" true c
  | Error e -> Alcotest.fail (Cbort.Error.to_string e)

let test_codec_nullable () =
  let codec = Cbort.nullable Cbort.int in
  (* Test Some *)
  let v1 = Some 42 in
  let encoded1 = Cbort.encode_string codec v1 in
  (match Cbort.decode_string codec encoded1 with
  | Ok decoded -> Alcotest.(check (option int)) "nullable some" v1 decoded
  | Error e -> Alcotest.fail (Cbort.Error.to_string e));
  (* Test None *)
  let v2 = None in
  let encoded2 = Cbort.encode_string codec v2 in
  match Cbort.decode_string codec encoded2 with
  | Ok decoded -> Alcotest.(check (option int)) "nullable none" v2 decoded
  | Error e -> Alcotest.fail (Cbort.Error.to_string e)

(* ============= Obj Codec Tests (Records with String Keys) ============= *)

type person = { name : string; age : int; email : string option }

let person_codec =
  Cbort.Obj.finish
  @@
  let open Cbort.Obj in
  let* name = mem "name" (fun p -> p.name) Cbort.string in
  let* age = mem "age" (fun p -> p.age) Cbort.int in
  let* email = mem_opt "email" (fun p -> p.email) Cbort.string in
  return { name; age; email }

let test_obj_codec_basic () =
  let v = { name = "Alice"; age = 30; email = None } in
  let encoded = Cbort.encode_string person_codec v in
  match Cbort.decode_string person_codec encoded with
  | Ok decoded ->
      Alcotest.(check string) "name" v.name decoded.name;
      Alcotest.(check int) "age" v.age decoded.age;
      Alcotest.(check (option string)) "email" v.email decoded.email
  | Error e -> Alcotest.fail (Cbort.Error.to_string e)

let test_obj_codec_with_optional () =
  let v = { name = "Bob"; age = 25; email = Some "bob@example.com" } in
  let encoded = Cbort.encode_string person_codec v in
  match Cbort.decode_string person_codec encoded with
  | Ok decoded ->
      Alcotest.(check string) "name" v.name decoded.name;
      Alcotest.(check int) "age" v.age decoded.age;
      Alcotest.(check (option string)) "email" v.email decoded.email
  | Error e -> Alcotest.fail (Cbort.Error.to_string e)

(* ============= Obj_int Codec Tests (Records with Integer Keys) ============= *)

(* CWT-style claims with integer keys per RFC 8392:
   1=iss, 2=sub, 3=aud, 4=exp, 5=nbf, 6=iat, 7=cti *)
type cwt_claims = {
  iss : string option; (* key 1 *)
  sub : string option; (* key 2 *)
  exp : int64 option; (* key 4 *)
}

let cwt_claims_codec =
  Cbort.Obj_int.finish
  @@
  let open Cbort.Obj_int in
  let* iss = mem_opt 1 (fun c -> c.iss) Cbort.string in
  let* sub = mem_opt 2 (fun c -> c.sub) Cbort.string in
  let* exp = mem_opt 4 (fun c -> c.exp) Cbort.int64 in
  return { iss; sub; exp }

let test_obj_int_codec () =
  let v =
    {
      iss = Some "https://example.com";
      sub = Some "user123";
      exp = Some 1700000000L;
    }
  in
  let encoded = Cbort.encode_string cwt_claims_codec v in
  match Cbort.decode_string cwt_claims_codec encoded with
  | Ok decoded ->
      Alcotest.(check (option string)) "iss" v.iss decoded.iss;
      Alcotest.(check (option string)) "sub" v.sub decoded.sub;
      Alcotest.(check (option int64)) "exp" v.exp decoded.exp
  | Error e -> Alcotest.fail (Cbort.Error.to_string e)

let test_obj_int_partial () =
  let v = { iss = Some "issuer"; sub = None; exp = None } in
  let encoded = Cbort.encode_string cwt_claims_codec v in
  match Cbort.decode_string cwt_claims_codec encoded with
  | Ok decoded ->
      Alcotest.(check (option string)) "iss" v.iss decoded.iss;
      Alcotest.(check (option string)) "sub" v.sub decoded.sub;
      Alcotest.(check (option int64)) "exp" v.exp decoded.exp
  | Error e -> Alcotest.fail (Cbort.Error.to_string e)

(* ============= Tag Tests with Codec API ============= *)

let test_codec_tag () =
  (* Tag 1 = epoch timestamp *)
  let epoch_codec = Cbort.tag 1 Cbort.int64 in
  let v = 1363896240L in
  let encoded = Cbort.encode_string epoch_codec v in
  (* Should match RFC 8949 example: c11a514b67b0 *)
  let hex =
    String.concat ""
      (List.init (String.length encoded) (fun i ->
           Printf.sprintf "%02x" (Char.code (String.get encoded i))))
  in
  Alcotest.(check string) "epoch tag hex" "c11a514b67b0" hex;
  match Cbort.decode_string epoch_codec encoded with
  | Ok decoded -> Alcotest.(check int64) "epoch value" v decoded
  | Error e -> Alcotest.fail (Cbort.Error.to_string e)

let test_codec_tag_opt () =
  (* Tag 32 = URI (optional) *)
  let uri_codec = Cbort.tag_opt 32 Cbort.string in
  let v = "https://example.com" in
  (* Encode with tag *)
  let encoded = Cbort.encode_string uri_codec v in
  (match Cbort.decode_string uri_codec encoded with
  | Ok decoded -> Alcotest.(check string) "uri tagged" v decoded
  | Error e -> Alcotest.fail (Cbort.Error.to_string e));
  (* Decode without tag should also work *)
  let plain = Cbort.encode_string Cbort.string v in
  match Cbort.decode_string uri_codec plain with
  | Ok decoded -> Alcotest.(check string) "uri untagged" v decoded
  | Error e -> Alcotest.fail (Cbort.Error.to_string e)

(* ============= Decode from Hex Tests ============= *)

let test_decode_rfc_integers () =
  (* RFC 8949 Appendix A test vectors *)
  let tests =
    [
      ("00", 0L);
      ("01", 1L);
      ("0a", 10L);
      ("17", 23L);
      ("1818", 24L);
      ("1819", 25L);
      ("1864", 100L);
      ("1903e8", 1000L);
      ("1a000f4240", 1000000L);
      ("1b000000e8d4a51000", 1000000000000L);
      ("20", -1L);
      ("29", -10L);
      ("3863", -100L);
      ("3903e7", -1000L);
    ]
  in
  List.iter
    (fun (hex, expected) ->
      let bytes = hex_to_bytes hex in
      match Cbort.decode_string Cbort.int64 bytes with
      | Ok decoded -> Alcotest.(check int64) hex expected decoded
      | Error e ->
          Alcotest.fail (Printf.sprintf "%s: %s" hex (Cbort.Error.to_string e)))
    tests

let test_decode_rfc_strings () =
  let tests =
    [
      ("60", "");
      ("6161", "a");
      ("6449455446", "IETF");
      ("62225c", "\"\\");
      ("62c3bc", "\xc3\xbc");
      (* ü *)
      ("63e6b0b4", "\xe6\xb0\xb4");
      (* 水 *)
    ]
  in
  List.iter
    (fun (hex, expected) ->
      let bytes = hex_to_bytes hex in
      match Cbort.decode_string Cbort.string bytes with
      | Ok decoded -> Alcotest.(check string) hex expected decoded
      | Error e ->
          Alcotest.fail (Printf.sprintf "%s: %s" hex (Cbort.Error.to_string e)))
    tests

let test_decode_rfc_arrays () =
  let int_list = Cbort.array Cbort.int in
  let tests = [ ("80", []); ("83010203", [ 1; 2; 3 ]) ] in
  List.iter
    (fun (hex, expected) ->
      let bytes = hex_to_bytes hex in
      match Cbort.decode_string int_list bytes with
      | Ok decoded -> Alcotest.(check (list int)) hex expected decoded
      | Error e ->
          Alcotest.fail (Printf.sprintf "%s: %s" hex (Cbort.Error.to_string e)))
    tests

let test_decode_rfc_booleans () =
  let tests = [ ("f4", false); ("f5", true) ] in
  List.iter
    (fun (hex, expected) ->
      let bytes = hex_to_bytes hex in
      match Cbort.decode_string Cbort.bool bytes with
      | Ok decoded -> Alcotest.(check bool) hex expected decoded
      | Error e ->
          Alcotest.fail (Printf.sprintf "%s: %s" hex (Cbort.Error.to_string e)))
    tests

let test_decode_rfc_null () =
  let bytes = hex_to_bytes "f6" in
  match Cbort.decode_string Cbort.null bytes with
  | Ok () -> ()
  | Error e -> Alcotest.fail (Cbort.Error.to_string e)

(* ============= Error Handling Tests ============= *)

let test_decode_type_mismatch () =
  (* Try to decode an integer as a string *)
  let bytes = hex_to_bytes "01" in
  (* integer 1 *)
  match Cbort.decode_string Cbort.string bytes with
  | Ok _ -> Alcotest.fail "Expected type mismatch error"
  | Error e ->
      let msg = Cbort.Error.to_string e in
      Alcotest.(check bool)
        "error contains type info" true
        (String.length msg > 0)

let test_decode_truncated () =
  (* Truncated integer (header says 4 bytes follow but only 2 provided) *)
  let bytes = hex_to_bytes "1a0001" in
  match Cbort.decode_string Cbort.int bytes with
  | Ok _ -> Alcotest.fail "Expected parse error"
  | Error _ -> ()

(* ============= Test Runner ============= *)

let () =
  Alcotest.run "Cbort"
    [
      (* Low-level encoding tests *)
      ( "Unsigned Integers (RFC 8949)",
        [
          Alcotest.test_case "0" `Quick test_uint_0;
          Alcotest.test_case "1" `Quick test_uint_1;
          Alcotest.test_case "10" `Quick test_uint_10;
          Alcotest.test_case "23" `Quick test_uint_23;
          Alcotest.test_case "24" `Quick test_uint_24;
          Alcotest.test_case "25" `Quick test_uint_25;
          Alcotest.test_case "100" `Quick test_uint_100;
          Alcotest.test_case "1000" `Quick test_uint_1000;
          Alcotest.test_case "1000000" `Quick test_uint_1000000;
          Alcotest.test_case "1000000000000" `Quick test_uint_1000000000000;
        ] );
      ( "Negative Integers (RFC 8949)",
        [
          Alcotest.test_case "-1" `Quick test_nint_minus1;
          Alcotest.test_case "-10" `Quick test_nint_minus10;
          Alcotest.test_case "-100" `Quick test_nint_minus100;
          Alcotest.test_case "-1000" `Quick test_nint_minus1000;
        ] );
      ( "Booleans and Null (RFC 8949)",
        [
          Alcotest.test_case "false" `Quick test_false;
          Alcotest.test_case "true" `Quick test_true;
          Alcotest.test_case "null" `Quick test_null;
        ] );
      ( "Floats (RFC 8949)",
        [
          Alcotest.test_case "1.0" `Quick test_float_1_0;
          Alcotest.test_case "1.1" `Quick test_float_1_1;
          Alcotest.test_case "-4.1" `Quick test_float_neg_4_1;
          Alcotest.test_case "1.0e+300" `Quick test_float_1e300;
          Alcotest.test_case "Infinity" `Quick test_float_infinity;
          Alcotest.test_case "-Infinity" `Quick test_float_neg_infinity;
          Alcotest.test_case "NaN" `Quick test_float_nan;
        ] );
      ( "Text Strings (RFC 8949)",
        [
          Alcotest.test_case "empty" `Quick test_text_empty;
          Alcotest.test_case "a" `Quick test_text_a;
          Alcotest.test_case "IETF" `Quick test_text_ietf;
          Alcotest.test_case "quote_backslash" `Quick test_text_quote_backslash;
          Alcotest.test_case "utf8_umlaut" `Quick test_text_utf8_umlaut;
          Alcotest.test_case "utf8_water" `Quick test_text_utf8_water;
          Alcotest.test_case "utf8_emoji" `Quick test_text_utf8_emoji;
        ] );
      ( "Byte Strings (RFC 8949)",
        [
          Alcotest.test_case "empty" `Quick test_bytes_empty;
          Alcotest.test_case "01020304" `Quick test_bytes_01020304;
        ] );
      ( "Arrays (RFC 8949)",
        [
          Alcotest.test_case "empty" `Quick test_array_empty;
          Alcotest.test_case "[1,2,3]" `Quick test_array_123;
          Alcotest.test_case "nested" `Quick test_array_nested;
          Alcotest.test_case "25_items" `Quick test_array_25_items;
        ] );
      ( "Maps (RFC 8949)",
        [
          Alcotest.test_case "empty" `Quick test_map_empty;
          Alcotest.test_case "int_keys" `Quick test_map_int_keys;
          Alcotest.test_case "string_keys" `Quick test_map_string_keys;
          Alcotest.test_case "mixed" `Quick test_mixed_array_map;
          Alcotest.test_case "5_pairs" `Quick test_map_5_pairs;
        ] );
      ( "Tags (RFC 8949)",
        [ Alcotest.test_case "epoch_timestamp" `Quick test_tag_epoch_timestamp ]
      );
      ( "Constants",
        [
          Alcotest.test_case "major_types" `Quick test_major_type_constants;
          Alcotest.test_case "simple_values" `Quick test_simple_value_constants;
          Alcotest.test_case "additional_info" `Quick
            test_additional_info_constants;
        ] );
      (* High-level codec roundtrip tests *)
      ( "Codec Roundtrip",
        [
          Alcotest.test_case "int" `Quick test_codec_int_roundtrip;
          Alcotest.test_case "int64" `Quick test_codec_int64_roundtrip;
          Alcotest.test_case "bool" `Quick test_codec_bool_roundtrip;
          Alcotest.test_case "null" `Quick test_codec_null_roundtrip;
          Alcotest.test_case "float" `Quick test_codec_float_roundtrip;
          Alcotest.test_case "string" `Quick test_codec_string_roundtrip;
          Alcotest.test_case "bytes" `Quick test_codec_bytes_roundtrip;
          Alcotest.test_case "array" `Quick test_codec_array_roundtrip;
          Alcotest.test_case "nested_array" `Quick test_codec_nested_array;
          Alcotest.test_case "string_map" `Quick test_codec_string_map_roundtrip;
          Alcotest.test_case "int_map" `Quick test_codec_int_map_roundtrip;
          Alcotest.test_case "tuple2" `Quick test_codec_tuple2;
          Alcotest.test_case "tuple3" `Quick test_codec_tuple3;
          Alcotest.test_case "nullable" `Quick test_codec_nullable;
        ] );
      ( "Obj Codec (String Keys)",
        [
          Alcotest.test_case "basic" `Quick test_obj_codec_basic;
          Alcotest.test_case "with_optional" `Quick test_obj_codec_with_optional;
        ] );
      ( "Obj_int Codec (Integer Keys)",
        [
          Alcotest.test_case "full" `Quick test_obj_int_codec;
          Alcotest.test_case "partial" `Quick test_obj_int_partial;
        ] );
      ( "Tag Codec",
        [
          Alcotest.test_case "tag" `Quick test_codec_tag;
          Alcotest.test_case "tag_opt" `Quick test_codec_tag_opt;
        ] );
      ( "Decode RFC Vectors",
        [
          Alcotest.test_case "integers" `Quick test_decode_rfc_integers;
          Alcotest.test_case "strings" `Quick test_decode_rfc_strings;
          Alcotest.test_case "arrays" `Quick test_decode_rfc_arrays;
          Alcotest.test_case "booleans" `Quick test_decode_rfc_booleans;
          Alcotest.test_case "null" `Quick test_decode_rfc_null;
        ] );
      ( "Error Handling",
        [
          Alcotest.test_case "type_mismatch" `Quick test_decode_type_mismatch;
          Alcotest.test_case "truncated" `Quick test_decode_truncated;
        ] );
    ]

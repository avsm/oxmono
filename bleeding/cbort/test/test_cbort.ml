(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Test suite for cbort using RFC 8949 Appendix A test vectors *)

open Bytesrw

(* Module aliases for wrapped modules *)
module Cbor = Cbort.Cbor
module Cbor_rw = Cbort.Rw

(* Base64 decoding *)
module Base64 = struct
  let decode_char c =
    match c with
    | 'A' .. 'Z' -> Char.code c - Char.code 'A'
    | 'a' .. 'z' -> Char.code c - Char.code 'a' + 26
    | '0' .. '9' -> Char.code c - Char.code '0' + 52
    | '+' -> 62
    | '/' -> 63
    | '=' -> 0
    | _ -> failwith (Printf.sprintf "Invalid base64 character: %c" c)

  let[@warning "-32"] decode s =
    let len = String.length s in
    if len = 0 then ""
    else
      let padding =
        if len >= 2 && s.[len - 1] = '=' && s.[len - 2] = '=' then 2
        else if len >= 1 && s.[len - 1] = '=' then 1
        else 0
      in
      let out_len = (len / 4 * 3) - padding in
      let out = Bytes.create out_len in
      let rec loop i o =
        if i >= len then ()
        else begin
          let a = decode_char s.[i] in
          let b = if i + 1 < len then decode_char s.[i + 1] else 0 in
          let c = if i + 2 < len then decode_char s.[i + 2] else 0 in
          let d = if i + 3 < len then decode_char s.[i + 3] else 0 in
          let n = (a lsl 18) lor (b lsl 12) lor (c lsl 6) lor d in
          if o < out_len then Bytes.set out o (Char.chr ((n lsr 16) land 0xff));
          if o + 1 < out_len then
            Bytes.set out (o + 1) (Char.chr ((n lsr 8) land 0xff));
          if o + 2 < out_len then Bytes.set out (o + 2) (Char.chr (n land 0xff));
          loop (i + 4) (o + 3)
        end
      in
      loop 0 0;
      Bytes.unsafe_to_string out
end

(* Simple JSON parser for test vectors *)
type json =
  | Jnull
  | Jbool of bool
  | Jnumber of float
  | Jstring of string
  | Jarray of json list
  | Jobject of (string * json) list

let rec skip_ws s i =
  if i >= String.length s then i
  else match s.[i] with ' ' | '\t' | '\n' | '\r' -> skip_ws s (i + 1) | _ -> i

let parse_string s i =
  if s.[i] <> '"' then failwith "Expected string";
  let buf = Buffer.create 64 in
  let rec loop i =
    if i >= String.length s then failwith "Unterminated string";
    match s.[i] with
    | '"' -> (Buffer.contents buf, i + 1)
    | '\\' -> (
        if i + 1 >= String.length s then failwith "Unterminated escape";
        match s.[i + 1] with
        | '"' ->
            Buffer.add_char buf '"';
            loop (i + 2)
        | '\\' ->
            Buffer.add_char buf '\\';
            loop (i + 2)
        | '/' ->
            Buffer.add_char buf '/';
            loop (i + 2)
        | 'b' ->
            Buffer.add_char buf '\b';
            loop (i + 2)
        | 'f' ->
            Buffer.add_char buf '\012';
            loop (i + 2)
        | 'n' ->
            Buffer.add_char buf '\n';
            loop (i + 2)
        | 'r' ->
            Buffer.add_char buf '\r';
            loop (i + 2)
        | 't' ->
            Buffer.add_char buf '\t';
            loop (i + 2)
        | 'u' ->
            let hex = String.sub s (i + 2) 4 in
            let code = int_of_string ("0x" ^ hex) in
            if code < 0x80 then Buffer.add_char buf (Char.chr code)
            else if code < 0x800 then begin
              Buffer.add_char buf (Char.chr (0xC0 lor (code lsr 6)));
              Buffer.add_char buf (Char.chr (0x80 lor (code land 0x3F)))
            end
            else begin
              Buffer.add_char buf (Char.chr (0xE0 lor (code lsr 12)));
              Buffer.add_char buf (Char.chr (0x80 lor ((code lsr 6) land 0x3F)));
              Buffer.add_char buf (Char.chr (0x80 lor (code land 0x3F)))
            end;
            loop (i + 6)
        | _ -> failwith "Invalid escape")
    | c ->
        Buffer.add_char buf c;
        loop (i + 1)
  in
  loop (i + 1)

let parse_number s i =
  let start = i in
  let rec loop i =
    if i >= String.length s then i
    else
      match s.[i] with
      | '0' .. '9' | '-' | '+' | '.' | 'e' | 'E' -> loop (i + 1)
      | _ -> i
  in
  let end_i = loop i in
  let num_str = String.sub s start (end_i - start) in
  (float_of_string num_str, end_i)

let rec parse_json s i =
  let i = skip_ws s i in
  if i >= String.length s then failwith "Unexpected end of input";
  match s.[i] with
  | 'n' ->
      if String.sub s i 4 = "null" then (Jnull, i + 4)
      else failwith "Expected null"
  | 't' ->
      if String.sub s i 4 = "true" then (Jbool true, i + 4)
      else failwith "Expected true"
  | 'f' ->
      if String.sub s i 5 = "false" then (Jbool false, i + 5)
      else failwith "Expected false"
  | '"' ->
      let str, i = parse_string s i in
      (Jstring str, i)
  | '[' ->
      let rec parse_array acc i =
        let i = skip_ws s i in
        if s.[i] = ']' then (List.rev acc, i + 1)
        else
          let v, i = parse_json s i in
          let i = skip_ws s i in
          if s.[i] = ',' then parse_array (v :: acc) (i + 1)
          else if s.[i] = ']' then (List.rev (v :: acc), i + 1)
          else failwith "Expected , or ]"
      in
      let i = skip_ws s (i + 1) in
      if s.[i] = ']' then (Jarray [], i + 1)
      else
        let arr, i = parse_array [] i in
        (Jarray arr, i)
  | '{' ->
      let rec parse_object acc i =
        let i = skip_ws s i in
        if s.[i] = '}' then (List.rev acc, i + 1)
        else
          let key, i = parse_string s i in
          let i = skip_ws s i in
          if s.[i] <> ':' then failwith "Expected :";
          let v, i = parse_json s (i + 1) in
          let i = skip_ws s i in
          if s.[i] = ',' then parse_object ((key, v) :: acc) (i + 1)
          else if s.[i] = '}' then (List.rev ((key, v) :: acc), i + 1)
          else failwith "Expected , or }"
      in
      let i = skip_ws s (i + 1) in
      if s.[i] = '}' then (Jobject [], i + 1)
      else
        let obj, i = parse_object [] i in
        (Jobject obj, i)
  | '-' | '0' .. '9' ->
      let num, i = parse_number s i in
      (Jnumber num, i)
  | c -> failwith (Printf.sprintf "Unexpected character: %c" c)

let parse_json_string s =
  let json, _ = parse_json s 0 in
  json

(* Test result tracking *)
let passed = ref 0
let failed = ref 0
let skipped = ref 0

let test name f =
  try
    f ();
    incr passed;
    Printf.printf "PASS: %s\n%!" name
  with e ->
    incr failed;
    Printf.printf "FAIL: %s - %s\n%!" name (Printexc.to_string e)

(* Compare CBOR values for equality, handling floats specially *)
let rec cbor_equal (a : Cbor.t) (b : Cbor.t) =
  match (a, b) with
  | Cbor.Int x, Cbor.Int y -> x = y
  | Cbor.Bytes x, Cbor.Bytes y -> x = y
  | Cbor.Text x, Cbor.Text y -> x = y
  | Cbor.Array xs, Cbor.Array ys ->
      List.length xs = List.length ys && List.for_all2 cbor_equal xs ys
  | Cbor.Map xs, Cbor.Map ys ->
      List.length xs = List.length ys
      && List.for_all2
           (fun (k1, v1) (k2, v2) -> cbor_equal k1 k2 && cbor_equal v1 v2)
           xs ys
  | Cbor.Tag (n1, v1), Cbor.Tag (n2, v2) -> n1 = n2 && cbor_equal v1 v2
  | Cbor.Bool x, Cbor.Bool y -> x = y
  | Cbor.Null, Cbor.Null -> true
  | Cbor.Undefined, Cbor.Undefined -> true
  | Cbor.Simple x, Cbor.Simple y -> x = y
  | Cbor.Float x, Cbor.Float y ->
      (Float.is_nan x && Float.is_nan y)
      || x = y
      ||
      (* Handle -0.0 == 0.0 *)
      (x = 0.0 && y = 0.0)
  (* Handle Int/Float comparisons - floats that are exact integers *)
  | Cbor.Float x, Cbor.Int y ->
      let yi = Z.to_float y in
      x = yi
  | Cbor.Int x, Cbor.Float y ->
      let xi = Z.to_float x in
      xi = y
  | _ -> false

(* Convert JSON decoded value to Cbor.t *)
let rec json_to_cbor = function
  | Jnull -> Cbor.Null
  | Jbool b -> Cbor.Bool b
  | Jnumber f ->
      let n = Z.of_float f in
      if Z.to_float n = f && f >= -9007199254740992.0 && f <= 9007199254740992.0
      then Cbor.Int n
      else Cbor.Float f
  | Jstring s -> Cbor.Text s
  | Jarray items -> Cbor.Array (List.map json_to_cbor items)
  | Jobject pairs ->
      Cbor.Map (List.map (fun (k, v) -> (Cbor.Text k, json_to_cbor v)) pairs)

(* Run RFC 8949 Appendix A test vectors *)
let run_rfc_tests () =
  let test_file = "../test-vectors/appendix_a.json" in
  let content =
    let ic = open_in test_file in
    let len = in_channel_length ic in
    let s = really_input_string ic len in
    close_in ic;
    s
  in
  let json = parse_json_string content in
  let vectors =
    match json with
    | Jarray items -> items
    | _ -> failwith "Expected array of test vectors"
  in
  Printf.printf "Running %d RFC 8949 test vectors...\n%!" (List.length vectors);
  List.iteri
    (fun i vec ->
      match vec with
      | Jobject fields ->
          let hex =
            match List.assoc_opt "hex" fields with
            | Some (Jstring s) -> s
            | _ -> failwith "Missing hex field"
          in
          let cbor_bytes = Ohex.decode hex in
          let roundtrip =
            match List.assoc_opt "roundtrip" fields with
            | Some (Jbool b) -> b
            | _ -> true
          in
          let decoded_opt = List.assoc_opt "decoded" fields in
          let _diagnostic_opt = List.assoc_opt "diagnostic" fields in
          let test_name = Printf.sprintf "vector %d (hex: %s)" i hex in

          (* Test decoding *)
          test (test_name ^ " decode") (fun () ->
              let reader = Bytes.Reader.of_string cbor_bytes in
              let dec = Cbor_rw.make_decoder reader in
              let cbor = Cbor_rw.read_cbor dec in
              (* Verify decoded value matches expected if present *)
              match decoded_opt with
              | Some expected_json ->
                  let expected = json_to_cbor expected_json in
                  if not (cbor_equal cbor expected) then
                    failwith
                      (Printf.sprintf
                         "Decoded value mismatch: got %s, expected %s"
                         (Cbor.to_diagnostic cbor)
                         (Cbor.to_diagnostic expected))
              | None ->
                  (* For diagnostic-only tests, just verify decode succeeds *)
                  ignore cbor);

          (* Test roundtrip if expected *)
          if roundtrip then
            test (test_name ^ " roundtrip") (fun () ->
                let reader = Bytes.Reader.of_string cbor_bytes in
                let dec = Cbor_rw.make_decoder reader in
                let cbor = Cbor_rw.read_cbor dec in
                let buf = Buffer.create 64 in
                let writer = Bytes.Writer.of_buffer buf in
                let enc = Cbor_rw.make_encoder writer in
                Cbor_rw.write_cbor enc cbor;
                Cbor_rw.flush_encoder enc;
                let encoded = Buffer.contents buf in
                if encoded <> cbor_bytes then
                  failwith
                    (Printf.sprintf
                       "Roundtrip mismatch: encoded %d bytes, expected %d bytes"
                       (String.length encoded) (String.length cbor_bytes)))
      | _ -> failwith "Expected object in test vector")
    vectors

(* Additional unit tests *)
let run_unit_tests () =
  Printf.printf "\nRunning unit tests...\n%!";

  (* Test basic codec operations *)
  test "codec: null" (fun () ->
      let encoded = Cbort.encode_string Cbort.null () in
      let decoded = Cbort.decode_string_exn Cbort.null encoded in
      assert (decoded = ()));

  test "codec: bool true" (fun () ->
      let encoded = Cbort.encode_string Cbort.bool true in
      let decoded = Cbort.decode_string_exn Cbort.bool encoded in
      assert (decoded = true));

  test "codec: bool false" (fun () ->
      let encoded = Cbort.encode_string Cbort.bool false in
      let decoded = Cbort.decode_string_exn Cbort.bool encoded in
      assert (decoded = false));

  test "codec: int positive" (fun () ->
      let encoded = Cbort.encode_string Cbort.int 42 in
      let decoded = Cbort.decode_string_exn Cbort.int encoded in
      assert (decoded = 42));

  test "codec: int negative" (fun () ->
      let encoded = Cbort.encode_string Cbort.int (-100) in
      let decoded = Cbort.decode_string_exn Cbort.int encoded in
      assert (decoded = -100));

  test "codec: int64" (fun () ->
      let n = 1000000000000L in
      let encoded = Cbort.encode_string Cbort.int64 n in
      let decoded = Cbort.decode_string_exn Cbort.int64 encoded in
      assert (decoded = n));

  test "codec: float" (fun () ->
      let encoded = Cbort.encode_string Cbort.float 3.14159 in
      let decoded = Cbort.decode_string_exn Cbort.float encoded in
      assert (abs_float (decoded -. 3.14159) < 0.00001));

  test "codec: string" (fun () ->
      let s = "Hello, CBOR!" in
      let encoded = Cbort.encode_string Cbort.string s in
      let decoded = Cbort.decode_string_exn Cbort.string encoded in
      assert (decoded = s));

  test "codec: bytes" (fun () ->
      let s = "\x00\x01\x02\x03" in
      let encoded = Cbort.encode_string Cbort.bytes s in
      let decoded = Cbort.decode_string_exn Cbort.bytes encoded in
      assert (decoded = s));

  test "codec: array" (fun () ->
      let arr = [ 1; 2; 3; 4; 5 ] in
      let encoded = Cbort.encode_string (Cbort.array Cbort.int) arr in
      let decoded = Cbort.decode_string_exn (Cbort.array Cbort.int) encoded in
      assert (decoded = arr));

  test "codec: tuple2" (fun () ->
      let t = ("hello", 42) in
      let encoded =
        Cbort.encode_string (Cbort.tuple2 Cbort.string Cbort.int) t
      in
      let decoded =
        Cbort.decode_string_exn (Cbort.tuple2 Cbort.string Cbort.int) encoded
      in
      assert (decoded = t));

  test "codec: string_map" (fun () ->
      let m = [ ("a", 1); ("b", 2) ] in
      let encoded = Cbort.encode_string (Cbort.string_map Cbort.int) m in
      let decoded =
        Cbort.decode_string_exn (Cbort.string_map Cbort.int) encoded
      in
      assert (decoded = m));

  test "codec: nullable Some" (fun () ->
      let v = Some 42 in
      let encoded = Cbort.encode_string (Cbort.nullable Cbort.int) v in
      let decoded =
        Cbort.decode_string_exn (Cbort.nullable Cbort.int) encoded
      in
      assert (decoded = v));

  test "codec: nullable None" (fun () ->
      let v = None in
      let encoded = Cbort.encode_string (Cbort.nullable Cbort.int) v in
      let decoded =
        Cbort.decode_string_exn (Cbort.nullable Cbort.int) encoded
      in
      assert (decoded = v));

  test "codec: tag" (fun () ->
      let v = 12345 in
      let encoded = Cbort.encode_string (Cbort.tag 1 Cbort.int) v in
      let decoded = Cbort.decode_string_exn (Cbort.tag 1 Cbort.int) encoded in
      assert (decoded = v));

  test "codec: Obj" (fun () ->
      let open Cbort.Obj in
      let codec =
        let* name = mem "name" fst Cbort.string in
        let* age = mem "age" snd Cbort.int in
        return (name, age)
      in
      let codec = finish codec in
      let v = ("Alice", 30) in
      let encoded = Cbort.encode_string codec v in
      let decoded = Cbort.decode_string_exn codec encoded in
      assert (decoded = v));

  test "codec: map transform" (fun () ->
      let codec =
        Cbort.map
          (fun s -> String.uppercase_ascii s)
          (fun s -> String.lowercase_ascii s)
          Cbort.string
      in
      let encoded = Cbort.encode_string codec "HELLO" in
      let decoded = Cbort.decode_string_exn codec encoded in
      assert (decoded = "HELLO"))

(* Define a recursive tree type for testing fix *)
type tree = Leaf of int | Node of tree * tree

let () =
  test "codec: fix (recursive)" (fun () ->
      (* Test recursive codec with a tree type *)
      let tree_codec =
        Cbort.fix (fun self ->
            Cbort.Variant.(
              variant
                [
                  case 0 Cbort.int
                    (fun x -> Leaf x)
                    (function Leaf x -> Some x | _ -> None);
                  case 1 (Cbort.tuple2 self self)
                    (fun (l, r) -> Node (l, r))
                    (function Node (l, r) -> Some (l, r) | _ -> None);
                ]))
      in
      let v = Node (Leaf 1, Node (Leaf 2, Leaf 3)) in
      let encoded = Cbort.encode_string tree_codec v in
      let decoded = Cbort.decode_string_exn tree_codec encoded in
      let rec tree_equal a b =
        match (a, b) with
        | Leaf x, Leaf y -> x = y
        | Node (l1, r1), Node (l2, r2) -> tree_equal l1 l2 && tree_equal r1 r2
        | _ -> false
      in
      assert (tree_equal decoded v))

let () =
  Printf.printf "cbort test suite\n";
  Printf.printf "================\n\n";

  run_rfc_tests ();
  run_unit_tests ();

  Printf.printf "\n================\n";
  Printf.printf "Results: %d passed, %d failed, %d skipped\n" !passed !failed
    !skipped;

  if !failed > 0 then exit 1

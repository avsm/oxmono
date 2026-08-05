(*---------------------------------------------------------------------------
   Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
   SPDX-License-Identifier: MIT
  ---------------------------------------------------------------------------*)

(* Crowbar-based fuzz testing for Punycode encoding/decoding *)

open Crowbar

(* Test that encode_utf8 never crashes on arbitrary input *)
let test_encode_no_crash input =
  (try ignore (Punycode.encode_utf8 input) with Punycode.Error _ -> ());
  check true

(* Test that decode_utf8 never crashes on arbitrary input *)
let test_decode_no_crash input =
  (try ignore (Punycode.decode_utf8 input) with Punycode.Error _ -> ());
  check true

(* Test roundtrip: encode then decode should give back original (case-insensitive)
   IDNA/Punycode lowercases ASCII characters during encoding per RFC 5891 *)
let test_roundtrip input =
  (try
    let encoded = Punycode.encode_utf8 input in
    let decoded = Punycode.decode_utf8 encoded in
    (* Compare lowercase versions since IDNA lowercases ASCII *)
    check_eq ~pp:Format.pp_print_string
      (String.lowercase_ascii input)
      (String.lowercase_ascii decoded)
  with Punycode.Error _ ->
    (* Some inputs might not encode/decode, that's ok *)
    check true)

(* Test ASCII-only strings (should pass through mostly unchanged) *)
let test_ascii_string input =
  if String.length input > 0 then begin
    let ascii_only =
      String.init
        (String.length input mod 64)
        (fun i ->
          Char.chr (Char.code input.[i mod String.length input] mod 128))
    in
    if String.length ascii_only > 0 then
      (try ignore (Punycode.encode_utf8 ascii_only) with Punycode.Error _ -> ())
  end;
  check true

(* Test inputs starting with ACE prefix "xn--" *)
let test_ace_prefix input =
  let ace_input = "xn--" ^ input in
  (try ignore (Punycode.decode_utf8 ace_input) with Punycode.Error _ -> ());
  check true

let () =
  add_test ~name:"punycode: encode no crash" [ bytes ] test_encode_no_crash;
  add_test ~name:"punycode: decode no crash" [ bytes ] test_decode_no_crash;
  add_test ~name:"punycode: roundtrip" [ bytes ] test_roundtrip;
  add_test ~name:"punycode: ascii string" [ bytes ] test_ascii_string;
  add_test ~name:"punycode: ace prefix" [ bytes ] test_ace_prefix

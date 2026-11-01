(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** RFC 8620 core data type and request/response conformance tests.

    Covers Section 1.2 (Id), Section 1.3 (Int/UnsignedInt), Section 1.4
    (Date/UTCDate), Section 3.7 (result references) and Section 4 (Core/echo).
*)

open Jmap.Proto

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let decode jsont json_str = Jsont_bytesrw.decode_string' jsont json_str
let encode jsont value = Jsont_bytesrw.encode_string' jsont value

(* {1 Fixture-driven tables}

   Fixtures live under [fixtures/<type>/<expectation>/*.json]. Every file in a
   directory is picked up automatically, so a new fixture needs no test code. *)

let fixture_dir parts = List.fold_left Filename.concat "fixtures" parts

let fixtures parts =
  let dir = fixture_dir parts in
  let files = Sys.readdir dir in
  Array.sort String.compare files;
  Array.to_list files
  |> List.filter (fun f -> Filename.check_suffix f ".json")
  |> List.map (fun f -> (Filename.remove_extension f, Filename.concat dir f))

(* A directory whose every fixture must decode. *)
let decodes_table label jsont parts =
  fixtures parts
  |> List.map (fun (name, path) ->
      ( Printf.sprintf "%s: %s" label name,
        `Quick,
        fun () ->
          match decode jsont (read_file path) with
          | Ok _ -> ()
          | Error e ->
              Alcotest.failf "%s: expected decode to succeed, got: %s" path
                (Jsont.Error.to_string e) ))

(* A directory whose every fixture must be rejected. *)
let rejects_table label jsont parts =
  fixtures parts
  |> List.map (fun (name, path) ->
      ( Printf.sprintf "%s: %s" label name,
        `Quick,
        fun () ->
          match decode jsont (read_file path) with
          | Ok _ -> Alcotest.failf "%s: expected decode to fail" path
          | Error _ -> () ))

(* {1 Section 1.2: Id} *)

module Id_tests = struct
  let decode_id s =
    match decode Id.jsont s with
    | Ok id -> Ok (Id.to_string id)
    | Error e -> Error (Jsont.Error.to_string e)

  let test_roundtrip () =
    match decode Id.jsont "\"msg-2024_01\"" with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok id -> (
        Alcotest.(check string) "value" "msg-2024_01" (Id.to_string id);
        match encode Id.jsont id with
        | Ok s -> Alcotest.(check string) "encoded" "\"msg-2024_01\"" s
        | Error e ->
            Alcotest.failf "encode failed: %s" (Jsont.Error.to_string e))

  (* RFC 8620 1.2: "a string of at least 1 and a maximum of 255 octets". The
     bound is in octets, not characters. *)
  let test_length_bounds_are_octets () =
    let of_len n = "\"" ^ String.make n 'a' ^ "\"" in
    (match decode_id (of_len 255) with
    | Ok s -> Alcotest.(check int) "255 octets accepted" 255 (String.length s)
    | Error e -> Alcotest.failf "255 octets rejected: %s" e);
    match decode_id (of_len 256) with
    | Ok _ -> Alcotest.fail "256 octets should be rejected"
    | Error _ -> ()

  (* RFC 8620 1.2 restricts an Id to the base64url alphabet, but that is a rule
     for the server allocating the id. Decoding takes what a server sent, so
     that a record with an off-alphabet id can still be read; constructing an
     id with Id.of_string is where the alphabet is enforced. *)
  let test_off_alphabet_decodes_but_is_not_constructible () =
    let off = [ "caf\xc3\xa9"; "a b"; "a.b"; "a+b"; "a/b"; "urn:uuid:1-2" ] in
    let check s =
      (match decode_id ("\"" ^ s ^ "\"") with
      | Ok decoded -> Alcotest.(check string) "decoded verbatim" s decoded
      | Error e -> Alcotest.failf "%S: a server id should decode: %s" s e);
      match Id.of_string s with
      | Ok _ -> Alcotest.failf "%S: of_string should enforce the alphabet" s
      | Error _ -> ()
    in
    List.iter check off

  (* RFC 8620 3.3/3.7: a creation reference is "#"-prefixed and is NOT an Id. *)
  let test_creation_ref_is_not_an_id () =
    match decode_id "\"#newEmail1\"" with
    | Ok _ -> Alcotest.fail "\"#newEmail1\" should not decode as an Id"
    | Error _ -> ()

  let test_error_escapes_control_bytes () =
    match Id.of_string "bad\027[2J" with
    | Ok _ -> Alcotest.fail "an escape byte decoded as an Id"
    | Error msg ->
        Alcotest.(check bool)
          "no literal escape byte" false
          (String.contains msg '\027');
        Alcotest.(check bool)
          "escaped byte is visible" true (String.contains msg '\\')

  let tests =
    decodes_table "valid" Id.jsont [ "id"; "valid" ]
    (* RFC 8620 1.2's "SHOULD avoid" list is advice to servers allocating ids,
       not a wire-format restriction, so these must still decode. *)
    @ decodes_table "discouraged" Id.jsont [ "id"; "discouraged" ]
    @ rejects_table "invalid" Id.jsont [ "id"; "invalid" ]
    (* Off the Section 1.2 alphabet, so Id.of_string rejects them, but a server
       may still have sent them and they decode. *)
    @ decodes_table "off the alphabet" Id.jsont [ "id"; "off_alphabet" ]
    @ [
        ("roundtrip", `Quick, test_roundtrip);
        ("length bound is in octets", `Quick, test_length_bounds_are_octets);
        ( "off the alphabet decodes but is not constructible",
          `Quick,
          test_off_alphabet_decodes_but_is_not_constructible );
        ("creation ref is not an Id", `Quick, test_creation_ref_is_not_an_id);
        ("errors escape control bytes", `Quick, test_error_escapes_control_bytes);
      ]
end

(* {1 Section 1.3: Int and UnsignedInt} *)

module Int53_tests = struct
  let ok_int64 name jsont s expected =
    match decode jsont s with
    | Ok n -> Alcotest.(check int64) name expected n
    | Error e ->
        Alcotest.failf "%s: decode failed: %s" name (Jsont.Error.to_string e)

  let fails name jsont s =
    match decode jsont s with
    | Ok n -> Alcotest.failf "%s: expected failure, decoded %Ld" name n
    | Error _ -> ()

  (* RFC 8620 1.3: "-2^53+1 <= value <= 2^53-1". *)
  let test_bounds () =
    Alcotest.(check int64) "signed max" 9007199254740991L Int53.Signed.max_value;
    Alcotest.(check int64)
      "signed min" (-9007199254740991L) Int53.Signed.min_value;
    Alcotest.(check int64) "unsigned min" 0L Int53.Unsigned.min_value;
    Alcotest.(check int64)
      "unsigned max" 9007199254740991L Int53.Unsigned.max_value;
    ok_int64 "max" Int53.Signed.jsont "9007199254740991" 9007199254740991L;
    ok_int64 "min" Int53.Signed.jsont "-9007199254740991" (-9007199254740991L);
    fails "over max" Int53.Signed.jsont "9007199254740992";
    fails "under min" Int53.Signed.jsont "-9007199254740992"

  (* B4: non-integral numbers must be rejected, not silently truncated. *)
  let test_non_integral_rejected () =
    fails "3.7" Int53.Signed.jsont "3.7";
    fails "123.456" Int53.Signed.jsont "123.456";
    fails "-0.5" Int53.Signed.jsont "-0.5";
    fails "1.5 unsigned" Int53.Unsigned.jsont "1.5"

  (* An integral JSON number written with a fractional part still denotes an
     integer, so 3.0 is a valid Int. *)
  let test_integral_float_accepted () =
    ok_int64 "3.0" Int53.Signed.jsont "3.0" 3L;
    ok_int64 "-7.0" Int53.Signed.jsont "-7.0" (-7L)

  (* B4: an explicit null reaches the number codec as NaN; it must be rejected
     cleanly rather than converted by unspecified Int64.of_float behaviour. *)
  let test_null_rejected () =
    fails "null signed" Int53.Signed.jsont "null";
    fails "null unsigned" Int53.Unsigned.jsont "null"

  (* Out-of-float64-range literals must not rely on Int64.of_float. *)
  let test_huge_exponent_rejected () =
    fails "1e400" Int53.Signed.jsont "1e400";
    fails "-1e400" Int53.Signed.jsont "-1e400";
    fails "1e300" Int53.Signed.jsont "1e300"

  (* Exponent notation denoting a value in range is a plain JSON number and is
     accepted: the codec sees the numeric value, not the source spelling. *)
  let test_scientific_in_range () =
    ok_int64 "1e5" Int53.Signed.jsont "1e5" 100000L;
    fails "1.5e0" Int53.Signed.jsont "1.5e0"

  (* Leading zeros are not valid JSON at all. *)
  let test_leading_zero_rejected () = fails "0123" Int53.Signed.jsont "0123"

  let test_string_rejected () =
    fails "\"12345\"" Int53.Signed.jsont "\"12345\"";
    fails "\"42\"" Int53.Unsigned.jsont "\"42\""

  let test_unsigned_negative_rejected () = fails "-1" Int53.Unsigned.jsont "-1"

  (* B9: of_int must range-check; an OCaml int is 63 bits. *)
  let test_of_int_range_checked () =
    (match Int53.Signed.of_int 42 with
    | Ok n -> Alcotest.(check int64) "42" 42L n
    | Error e -> Alcotest.failf "of_int 42 failed: %s" e);
    (match Int53.Signed.of_int max_int with
    | Ok n -> Alcotest.failf "of_int max_int should fail, got %Ld" n
    | Error _ -> ());
    (match Int53.Signed.of_int min_int with
    | Ok n -> Alcotest.failf "of_int min_int should fail, got %Ld" n
    | Error _ -> ());
    (match Int53.Unsigned.of_int (1 lsl 60) with
    | Ok n -> Alcotest.failf "of_int (1 lsl 60) should fail, got %Ld" n
    | Error _ -> ());
    (match Int53.Unsigned.of_int (-1) with
    | Ok n -> Alcotest.failf "of_int (-1) should fail, got %Ld" n
    | Error _ -> ());
    match Int53.Unsigned.of_int 0 with
    | Ok n -> Alcotest.(check int64) "0" 0L n
    | Error e -> Alcotest.failf "of_int 0 failed: %s" e

  (* A boundary value must not be emitted in exponent form. *)
  let test_encode_boundary () =
    match encode Int53.Signed.jsont Int53.Signed.max_value with
    | Ok s -> Alcotest.(check string) "max" "9007199254740991" s
    | Error e -> Alcotest.failf "encode failed: %s" (Jsont.Error.to_string e)

  let tests =
    decodes_table "signed valid" Int53.Signed.jsont [ "int53"; "signed_valid" ]
    @ rejects_table "signed invalid" Int53.Signed.jsont
        [ "int53"; "signed_invalid" ]
    @ decodes_table "unsigned valid" Int53.Unsigned.jsont
        [ "int53"; "unsigned_valid" ]
    @ rejects_table "unsigned invalid" Int53.Unsigned.jsont
        [ "int53"; "unsigned_invalid" ]
    @ [
        ("bounds", `Quick, test_bounds);
        ("non-integral rejected", `Quick, test_non_integral_rejected);
        ("integral float accepted", `Quick, test_integral_float_accepted);
        ("null rejected", `Quick, test_null_rejected);
        ("huge exponent rejected", `Quick, test_huge_exponent_rejected);
        ("scientific in range", `Quick, test_scientific_in_range);
        ("leading zero rejected", `Quick, test_leading_zero_rejected);
        ("string rejected", `Quick, test_string_rejected);
        ("unsigned negative rejected", `Quick, test_unsigned_negative_rejected);
        ("of_int range checked", `Quick, test_of_int_range_checked);
        ("encode boundary", `Quick, test_encode_boundary);
      ]
end

(* {1 Section 1.4: Date and UTCDate} *)

module Date_tests = struct
  let decode_str jsont s =
    match decode jsont ("\"" ^ s ^ "\"") with
    | Ok t -> Ok t
    | Error e -> Error (Jsont.Error.to_string e)

  let encode_str jsont t =
    match encode jsont t with
    | Ok s -> s
    | Error e -> Alcotest.failf "encode failed: %s" (Jsont.Error.to_string e)

  (* B7: RFC 8620 1.4 "any letters in the string (e.g., "T" and "Z") MUST be
     uppercase", so Ptime must be used in strict mode. *)
  let test_case_and_separator_strictness () =
    let bad =
      [
        "2024-01-15t10:30:00Z";
        "2024-01-15T10:30:00z";
        "2024-01-15t10:30:00z";
        "2024-01-15 10:30:00Z";
        "2024-01-15T10:30:00+0800";
        "2024-01-15T10:30:00+08";
      ]
    in
    List.iter
      (fun s ->
        match decode_str Date.jsont s with
        | Ok _ -> Alcotest.failf "Date: %S should be rejected" s
        | Error _ -> ())
      bad;
    List.iter
      (fun s ->
        match decode_str Date.utc_jsont s with
        | Ok _ -> Alcotest.failf "UTCDate: %S should be rejected" s
        | Error _ -> ())
      bad

  (* B8: RFC 8620 1.4 "the time-secfrac MUST always be omitted if zero" - so a
     non-zero fraction must survive a decode/encode round trip, and a zero one
     must not be emitted. *)
  let test_fractional_seconds_roundtrip () =
    let check input expected =
      match decode_str Date.utc_jsont input with
      | Error e -> Alcotest.failf "%S: decode failed: %s" input e
      | Ok t ->
          Alcotest.(check string)
            input
            ("\"" ^ expected ^ "\"")
            (encode_str Date.utc_jsont t)
    in
    check "2024-01-15T10:30:00Z" "2024-01-15T10:30:00Z";
    check "2024-01-15T10:30:00.0Z" "2024-01-15T10:30:00Z";
    check "2024-01-15T10:30:00.000Z" "2024-01-15T10:30:00Z";
    check "2024-01-15T10:30:00.5Z" "2024-01-15T10:30:00.5Z";
    check "2024-01-15T10:30:00.500Z" "2024-01-15T10:30:00.5Z";
    check "2024-01-15T10:30:00.123Z" "2024-01-15T10:30:00.123Z";
    check "2024-01-15T10:30:00.123456Z" "2024-01-15T10:30:00.123456Z";
    check "2024-01-15T10:30:00.000001Z" "2024-01-15T10:30:00.000001Z"

  (* An offset date-time is legal for Date; it denotes the same instant when
     re-encoded in UTC. *)
  let test_offset_forms () =
    let check input expected =
      match decode_str Date.jsont input with
      | Error e -> Alcotest.failf "%S: decode failed: %s" input e
      | Ok t ->
          Alcotest.(check string)
            input
            ("\"" ^ expected ^ "\"")
            (encode_str Date.jsont t)
    in
    (* The example from RFC 8620 1.4. *)
    check "2014-10-30T14:12:00+08:00" "2014-10-30T06:12:00Z";
    check "2024-01-15T10:30:00-08:00" "2024-01-15T18:30:00Z";
    check "2024-01-15T10:30:00Z" "2024-01-15T10:30:00Z"

  (* RFC 8620 1.4: for UTCDate "the time-offset component MUST be Z". *)
  let test_utc_requires_z () =
    List.iter
      (fun s ->
        match decode_str Date.utc_jsont s with
        | Ok _ -> Alcotest.failf "UTCDate: %S should be rejected" s
        | Error _ -> ())
      [
        "2024-01-15T10:30:00+05:30";
        "2024-01-15T10:30:00-08:00";
        "2024-01-15T10:30:00+00:00";
      ]

  let test_of_string_to_string () =
    match Date.of_utc_string "2024-01-15T10:30:00.25Z" with
    | Error e -> Alcotest.failf "of_string failed: %s" e
    | Ok t ->
        Alcotest.(check string)
          "roundtrip" "2024-01-15T10:30:00.25Z" (Date.to_utc_string t)

  let tests =
    decodes_table "Date valid" Date.jsont [ "date"; "valid" ]
    @ rejects_table "Date invalid" Date.jsont [ "date"; "invalid" ]
    @ decodes_table "UTCDate valid" Date.utc_jsont [ "date"; "utc_valid" ]
    @ rejects_table "UTCDate invalid" Date.utc_jsont [ "date"; "utc_invalid" ]
    @ [
        ("strict case and separator", `Quick, test_case_and_separator_strictness);
        ( "fractional seconds roundtrip",
          `Quick,
          test_fractional_seconds_roundtrip );
        ("offset forms", `Quick, test_offset_forms);
        ("UTCDate requires Z", `Quick, test_utc_requires_z);
        ("of_string/to_string", `Quick, test_of_string_to_string);
      ]
end

(* {1 Section 3.2: Invocation} *)

module Invocation_tests = struct
  let test_roundtrip () =
    let json = "[\"Core/echo\",{\"hello\":true,\"high\":5},\"b3ff\"]" in
    match decode Invocation.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok inv -> (
        Alcotest.(check string) "name" "Core/echo" inv.Invocation.name;
        Alcotest.(check string) "call id" "b3ff" inv.Invocation.method_call_id;
        match encode Invocation.jsont inv with
        | Ok s -> Alcotest.(check string) "encoded" json s
        | Error e ->
            Alcotest.failf "encode failed: %s" (Jsont.Error.to_string e))

  (* G7: RFC 8620 3.2 slot 1 is "a String[*] object containing named
     arguments"; anything else must be rejected. *)
  let test_arguments_must_be_object () =
    List.iter
      (fun json ->
        match decode Invocation.jsont json with
        | Ok _ -> Alcotest.failf "%s: expected failure" json
        | Error _ -> ())
      [
        "[\"Foo/get\",3,\"c1\"]";
        "[\"Foo/get\",\"args\",\"c1\"]";
        "[\"Foo/get\",[],\"c1\"]";
        "[\"Foo/get\",null,\"c1\"]";
        "[\"Foo/get\",true,\"c1\"]";
      ]

  let test_shape_errors () =
    List.iter
      (fun json ->
        match decode Invocation.jsont json with
        | Ok _ -> Alcotest.failf "%s: expected failure" json
        | Error _ -> ())
      [
        "{\"method\":\"Email/get\"}";
        "[\"Email/get\",{}]";
        "[\"Email/get\",{},\"c1\",\"extra\"]";
        "[3,{},\"c1\"]";
        "[\"Email/get\",{},3]";
      ]

  let tests =
    [
      ("roundtrip", `Quick, test_roundtrip);
      ("arguments must be an object", `Quick, test_arguments_must_be_object);
      ("shape errors", `Quick, test_shape_errors);
    ]
end

(* {1 Section 3.7: result references} *)

module Result_reference_tests = struct
  let test_result_reference_codec () =
    let json =
      "{\"resultOf\":\"t0\",\"name\":\"Foo/changes\",\"path\":\"/created\"}"
    in
    match decode Invocation.result_reference_jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok (r : Invocation.result_reference) -> (
        Alcotest.(check string) "resultOf" "t0" r.result_of;
        Alcotest.(check string) "name" "Foo/changes" r.name;
        Alcotest.(check (list string))
          "path tokens" [ "created" ]
          (Json_pointer.tokens r.path);
        match encode Invocation.result_reference_jsont r with
        | Ok s -> Alcotest.(check string) "encoded" json s
        | Error e ->
            Alcotest.failf "encode failed: %s" (Jsont.Error.to_string e))

  let tests = [ ("ResultReference codec", `Quick, test_result_reference_codec) ]
end

(* {1 Section 3.3/3.4 and Section 4: Core/echo} *)

module Core_echo_tests = struct
  let core_urn = "urn:ietf:params:jmap:core"

  let echo_request_json =
    {|{"using":["urn:ietf:params:jmap:core"],"methodCalls":[["Core/echo",{"hello":true,"high":5},"b3ff"]]}|}

  let echo_response_json =
    {|{"methodResponses":[["Core/echo",{"hello":true,"high":5},"b3ff"]],"sessionState":"75128aab4b1b"}|}

  (* RFC 8620 4: "Core/echo returns exactly the same arguments as it is
     given". *)
  let test_request_roundtrip () =
    match decode Request.jsont echo_request_json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok req -> (
        Alcotest.(check (list string)) "using" [ core_urn ] req.Request.using;
        (match req.Request.method_calls with
        | [ inv ] ->
            Alcotest.(check string) "name" "Core/echo" inv.Invocation.name;
            Alcotest.(check string) "id" "b3ff" inv.Invocation.method_call_id
        | l -> Alcotest.failf "expected 1 method call, got %d" (List.length l));
        match encode Request.jsont req with
        | Ok s -> Alcotest.(check string) "encoded" echo_request_json s
        | Error e ->
            Alcotest.failf "encode failed: %s" (Jsont.Error.to_string e))

  let test_response_roundtrip () =
    match decode Response.jsont echo_response_json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok resp -> (
        Alcotest.(check string)
          "sessionState" "75128aab4b1b" resp.Response.session_state;
        (match Response.find_response "b3ff" resp with
        | None -> Alcotest.fail "expected a response for b3ff"
        | Some inv ->
            Alcotest.(check string) "name" "Core/echo" inv.Invocation.name;
            Alcotest.(check bool) "not an error" false (Response.is_error inv));
        match encode Response.jsont resp with
        | Ok s -> Alcotest.(check string) "encoded" echo_response_json s
        | Error e ->
            Alcotest.failf "encode failed: %s" (Jsont.Error.to_string e))

  (* The arguments must survive the round trip unchanged. *)
  let test_echo_arguments_preserved () =
    let args = {|{"hello":true,"high":5,"nested":{"a":[1,2,3]}}|} in
    let json = "[\"Core/echo\"," ^ args ^ ",\"b3ff\"]" in
    match decode Invocation.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok inv -> (
        let echoed =
          Invocation.create ~name:"Core/echo"
            ~arguments:inv.Invocation.arguments ~method_call_id:"b3ff"
        in
        match encode Invocation.jsont echoed with
        | Ok s -> Alcotest.(check string) "echoed" json s
        | Error e ->
            Alcotest.failf "encode failed: %s" (Jsont.Error.to_string e))

  (* G9: RFC 8620 3.2 - a method may return more than one response, all sharing
     the method call id. *)
  let test_find_responses () =
    let json =
      {|{"methodResponses":[["Foo/set",{"a":1},"c2"],["Foo/get",{"b":2},"c2"],["Bar/get",{},"c3"]],"sessionState":"s"}|}
    in
    match decode Response.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok resp ->
        let names =
          List.map
            (fun (i : Invocation.t) -> i.Invocation.name)
            (Response.find_responses "c2" resp)
        in
        Alcotest.(check (list string)) "all c2" [ "Foo/set"; "Foo/get" ] names;
        (match Response.find_response "c2" resp with
        | Some inv ->
            Alcotest.(check string) "first c2" "Foo/set" inv.Invocation.name
        | None -> Alcotest.fail "expected a first response for c2");
        Alcotest.(check (list string))
          "none" []
          (List.map
             (fun (i : Invocation.t) -> i.Invocation.name)
             (Response.find_responses "c9" resp))

  let test_error_response () =
    let json =
      {|{"methodResponses":[["error",{"type":"unknownMethod"},"c1"]],"sessionState":"s"}|}
    in
    match decode Response.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok resp -> (
        match Response.find_response "c1" resp with
        | None -> Alcotest.fail "expected a response for c1"
        | Some inv -> (
            Alcotest.(check bool) "is_error" true (Response.is_error inv);
            match Response.error inv with
            | Some _ -> ()
            | None -> Alcotest.fail "expected a decoded method error"))

  let test_request_builders () =
    let inv =
      Invocation.create ~name:"Core/echo"
        ~arguments:(Jsont.Object ([], Jsont.Meta.none))
        ~method_call_id:"c0"
    in
    let req = Request.create ~using:[ core_urn ] ~method_calls:[ inv ] () in
    Alcotest.(check int) "one call" 1 (List.length req.Request.method_calls);
    let req =
      Request.create ~using:[ core_urn ] ~method_calls:[ inv; inv ] ()
    in
    Alcotest.(check int) "two calls" 2 (List.length req.Request.method_calls);
    match encode Request.jsont req with
    | Ok s ->
        Alcotest.(check string)
          "encoded"
          {|{"using":["urn:ietf:params:jmap:core"],"methodCalls":[["Core/echo",{},"c0"],["Core/echo",{},"c0"]]}|}
          s
    | Error e -> Alcotest.failf "encode failed: %s" (Jsont.Error.to_string e)

  let tests =
    [
      ("request roundtrip", `Quick, test_request_roundtrip);
      ("response roundtrip", `Quick, test_response_roundtrip);
      ("echo arguments preserved", `Quick, test_echo_arguments_preserved);
      ("find_responses returns all", `Quick, test_find_responses);
      ("error response", `Quick, test_error_response);
      ("request builders", `Quick, test_request_builders);
    ]
end

(* Session resource (RFC 8620 Section 2) *)
module Session_tests = struct
  let core_capabilities =
    {|{"urn:ietf:params:jmap:core":{"maxSizeUpload":1,"maxConcurrentUpload":1,"maxSizeRequest":1,"maxConcurrentRequests":1,"maxCallsInRequest":1,"maxObjectsInGet":1,"maxObjectsInSet":1,"collationAlgorithms":[]}}|}

  let minimal ?(capabilities = core_capabilities) ?(extra = "") () =
    Printf.sprintf
      {|{"capabilities":%s,"accounts":{},"primaryAccounts":{},"username":"u@example.com","apiUrl":"https://example.com/jmap/","downloadUrl":"https://example.com/dl","uploadUrl":"https://example.com/ul","eventSourceUrl":"https://example.com/es","state":"s1"%s}|}
      capabilities extra

  let decode_ok name json =
    match decode Session.jsont json with
    | Ok s -> s
    | Error e ->
        Alcotest.failf "%s: decode failed: %s" name (Jsont.Error.to_string e)

  let expect_failure name json =
    match decode Session.jsont json with
    | Ok _ -> Alcotest.failf "%s: expected a decode failure" name
    | Error _ -> ()

  (* Section 2: capabilities is "String[Object]" - "The value for each of
     these keys is an object with further information". *)
  let test_capability_values_must_be_objects () =
    ignore (decode_ok "object capability" (minimal ()));
    expect_failure "string capability"
      (minimal ~capabilities:{|{"urn:ietf:params:jmap:core":"yes"}|} ());
    expect_failure "true capability"
      (minimal ~capabilities:{|{"urn:ietf:params:jmap:core":true}|} ());
    expect_failure "null capability"
      (minimal ~capabilities:{|{"urn:ietf:params:jmap:core":null}|} ());
    expect_failure "array capability"
      (minimal ~capabilities:{|{"urn:ietf:params:jmap:core":[]}|} ())

  (* accountCapabilities is "String[Object]" too. *)
  let test_account_capability_values_must_be_objects () =
    let account caps =
      minimal
        ~extra:
          (Printf.sprintf
             {|,"accounts":{"A1":{"name":"u@example.com","isPersonal":true,"isReadOnly":false,"accountCapabilities":%s}}|}
             caps)
        ()
    in
    (* The later "accounts" member wins over the empty one in [minimal]. *)
    ignore
      (decode_ok "object account capability"
         (account
            {|{"urn:ietf:params:jmap:mail":{"maxMailboxesPerEmail":null,"maxMailboxDepth":null,"maxSizeMailboxName":100,"maxSizeAttachmentsPerEmail":0,"emailQuerySortOptions":[],"mayCreateTopLevelMailbox":false}}|}));
    expect_failure "string account capability"
      (account {|{"urn:ietf:params:jmap:mail":"yes"}|});
    expect_failure "malformed known account capability"
      (account {|{"urn:ietf:params:jmap:mail":{"maxMailboxDepth":-1}}|})

  (* Section 2: "other properties MAY be included on the Session object.
     Clients MUST ignore any properties they are not expecting" - and a
     client that re-encodes should hand them back unchanged. *)
  let test_unknown_members_are_kept () =
    let s =
      decode_ok "session with extensions"
        (minimal ~extra:{|,"cyrusVersion":"3.9","x-quota":{"used":17}|} ())
    in
    Alcotest.(check bool)
      "cyrusVersion kept" true
      (Session.unknown_member s "cyrusVersion" <> None);
    Alcotest.(check bool)
      "x-quota kept" true
      (Session.unknown_member s "x-quota" <> None);
    Alcotest.(check bool)
      "no such member" true
      (Session.unknown_member s "nope" = None);
    match encode Session.jsont s with
    | Error e -> Alcotest.failf "encode failed: %s" (Jsont.Error.to_string e)
    | Ok json ->
        let has needle =
          let n = String.length needle and m = String.length json in
          let rec go i =
            i + n <= m && (String.sub json i n = needle || go (i + 1))
          in
          go 0
        in
        Alcotest.(check bool)
          "cyrusVersion re-encoded" true
          (has {|"cyrusVersion":"3.9"|});
        Alcotest.(check bool) "x-quota re-encoded" true (has {|"x-quota"|})

  let test_account_unknown_members_are_kept () =
    let s =
      decode_ok "account with extensions"
        (minimal
           ~extra:
             {|,"accounts":{"A1":{"name":"u@example.com","isPersonal":true,"isReadOnly":false,"accountCapabilities":{},"x-owner":"root"}}|}
           ())
    in
    match s.Session.accounts with
    | [ (_, a) ] ->
        Alcotest.(check bool)
          "x-owner kept" true
          (Session.Account.unknown_member a "x-owner" <> None)
    | l -> Alcotest.failf "expected one account, got %d" (List.length l)

  let tests =
    [
      ( "capability values must be objects",
        `Quick,
        test_capability_values_must_be_objects );
      ( "accountCapabilities values must be objects",
        `Quick,
        test_account_capability_values_must_be_objects );
      ("unknown session members are kept", `Quick, test_unknown_members_are_kept);
      ( "unknown account members are kept",
        `Quick,
        test_account_unknown_members_are_kept );
    ]
end

let () =
  Alcotest.run "JMAP RFC 8620 core"
    [
      ("Id (1.2)", Id_tests.tests);
      ("Int53 (1.3)", Int53_tests.tests);
      ("Date (1.4)", Date_tests.tests);
      ("Invocation (3.2)", Invocation_tests.tests);
      ("ResultReference (3.7)", Result_reference_tests.tests);
      ("Core/echo (3.3, 3.4, 4)", Core_echo_tests.tests);
      ("Session (2)", Session_tests.tests);
    ]

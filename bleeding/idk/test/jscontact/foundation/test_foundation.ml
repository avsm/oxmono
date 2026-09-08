(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Jscontact

let decode t s = Jsont_bytesrw.decode_string' t s
let encode t v = Jsont_bytesrw.encode_string' t v

let enc_ok name t v =
  match encode t v with
  | Ok s -> s
  | Error e ->
      Alcotest.failf "%s: encode failed: %s" name (Jsont.Error.to_string e)

let dec_ok name t s =
  match decode t s with
  | Ok v -> v
  | Error e ->
      Alcotest.failf "%s: decode failed: %s" name (Jsont.Error.to_string e)

let roundtrip name t s expected =
  let v = dec_ok name t s in
  let s1 = enc_ok name t v in
  Alcotest.(check string) name expected s1;
  let v2 = dec_ok name t s1 in
  Alcotest.(check string) (name ^ " (stable)") s1 (enc_ok name t v2)

let test_utc () =
  let t = Date.Utc.jsont in
  roundtrip "utc" t {|"2022-09-30T14:35:10Z"|} {|"2022-09-30T14:35:10Z"|};
  roundtrip "utc frac" t {|"2010-10-10T10:10:10.003Z"|}
    {|"2010-10-10T10:10:10.003Z"|};
  roundtrip "utc canonicalised" t {|"2010-10-10T10:10:10.000Z"|}
    {|"2010-10-10T10:10:10Z"|};
  Alcotest.(check bool)
    "canonical" true
    (Date.Utc.is_canonical "2010-10-10T10:10:10.003Z");
  Alcotest.(check bool)
    "not canonical" false
    (Date.Utc.is_canonical "2010-10-10T10:10:10.000Z");
  match decode t {|"2010-10-10t10:10:10z"|} with
  | Ok _ -> Alcotest.fail "lowercase date-time should not decode"
  | Error _ -> ()

let test_date_union () =
  let t = Date.jsont in
  roundtrip "partial date" t {|{"year":1953,"month":4,"day":15}|}
    {|{"year":1953,"month":4,"day":15}|};
  roundtrip "partial date with @type" t {|{"@type":"PartialDate","year":1953}|}
    {|{"year":1953}|};
  roundtrip "timestamp" t {|{"@type":"Timestamp","utc":"2019-10-15T23:10:00Z"}|}
    {|{"@type":"Timestamp","utc":"2019-10-15T23:10:00Z"}|};
  (* Section 1.3.4 implies PartialDate when @type is absent, so a Timestamp
     that omits it is a PartialDate whose "utc" is an unknown member, which
     Section 1.7.4 requires to be preserved. *)
  roundtrip "a Timestamp without @type is a PartialDate" t
    {|{"utc":"2019-10-15T23:10:00Z"}|} {|{"utc":"2019-10-15T23:10:00Z"}|}

let test_partial_date_validate () =
  let d = Date.Partial_date.make ~day:15 () in
  (match Date.Partial_date.validate d with
  | Ok _ -> Alcotest.fail "a day without a month is invalid"
  | Error _ -> ());
  let d = Date.Partial_date.make ~year:1953 ~month:4 ~day:15 () in
  match Date.Partial_date.validate d with
  | Ok _ -> ()
  | Error msg -> Alcotest.failf "valid date rejected: %s" msg

let test_unknown_kept () =
  let t = Date.Partial_date.jsont in
  roundtrip "unknown members are kept" t
    {|{"year":1953,"example.com:foo":{"bar":true}}|}
    {|{"year":1953,"example.com:foo":{"bar":true}}|}

(* Section 1.4.2: an UnsignedInt is an integer in the range 0 to 2^53-1,
   represented as a JSON Number. Jsont.int alone would also accept a string,
   truncate a fraction, and encode a large value as a string. *)
let test_unsigned () =
  let t = Json.unsigned ~kind:"pref" in
  roundtrip "an integer" t "1" "1";
  roundtrip "the largest safe integer" t "9007199254740991" "9007199254740991";
  let rejects name s =
    match decode t s with
    | Ok v -> Alcotest.failf "%s: decoded %S as %d" name s v
    | Error _ -> ()
  in
  rejects "a fraction" "1.5";
  rejects "a string" {|"7"|};
  rejects "a hexadecimal string" {|"0x10"|};
  rejects "a negative number" "-1";
  rejects "null" "null";
  rejects "a number above 2^53-1" "9007199254740992";
  (* An exponent that denotes an integer is a JSON number like any other. *)
  roundtrip "an exponent" t "1e2" "100";
  match encode t max_int with
  | Ok s -> Alcotest.failf "max_int encoded as %s" s
  | Error _ -> ()

(* A JMAP ContactCard carries id and addressBookIds, which RFC 9553 does not
   define, so a wrapper has to lift them out of the unknown members and put
   them back. *)
let test_unknown_add_remove () =
  let u = Unknown.empty in
  let u = Unknown.add u "id" (Jsont.Json.string "K1") in
  let u = Unknown.add u "addressBookIds" (Jsont.Json.bool true) in
  Alcotest.(check (list string))
    "names" [ "id"; "addressBookIds" ] (Unknown.names u);
  Alcotest.(check bool) "found" true (Unknown.find u "id" <> None);
  let u = Unknown.add u "id" (Jsont.Json.string "K2") in
  Alcotest.(check int) "adding again replaces" 2 (List.length (Unknown.names u));
  let u = Unknown.remove u "id" in
  Alcotest.(check bool) "removed" true (Unknown.find u "id" = None);
  Alcotest.(check bool)
    "removing again is a no-op" true
    (Unknown.find (Unknown.remove u "id") "id" = None);
  Alcotest.(check bool)
    "the rest survives" true
    (Unknown.find u "addressBookIds" <> None)

let test_id () =
  (match Id.of_string "k23_-A" with
  | Ok _ -> ()
  | Error msg -> Alcotest.failf "valid Id rejected: %s" msg);
  match Id.of_string "k23!" with
  | Ok _ -> Alcotest.fail "an Id with a bang is invalid"
  | Error _ -> ()

let test_vendor () =
  Alcotest.(check bool) "extension" true (Vendor.is_extension "example.com:foo");
  Alcotest.(check bool) "no colon" false (Vendor.is_extension "example.com");
  Alcotest.(check bool)
    "bad prefix" false
    (Vendor.is_extension "-example.com:foo");
  Alcotest.(check bool)
    "solidus in name" false
    (Vendor.is_extension "example.com:a/b");
  Alcotest.(check bool) "registered" true (Registry.is_registrable_name "@type");
  Alcotest.(check bool)
    "registered camel" true
    (Registry.is_registrable_name "phoneticSystem");
  (* Section 1.7.3: "extra" is reserved and must not be set on any object. *)
  Alcotest.(check bool) "extra is reserved" true (Registry.is_reserved "extra");
  let rejects name s =
    match Registry.validate_property_name s with
    | Ok _ -> Alcotest.failf "%s: %S should not be a valid property name" name s
    | Error _ -> ()
  in
  let accepts name s =
    match Registry.validate_property_name s with
    | Ok _ -> ()
    | Error msg -> Alcotest.failf "%s: %S rejected: %s" name s msg
  in
  rejects "reserved" "extra";
  (* Section 1.7.1: a name differing only in case from a registered one. *)
  rejects "case variant" "uID";
  rejects "case variant of a reserved name" "Extra";
  accepts "registered" "uid";
  accepts "unknown but registrable" "favouriteColour";
  accepts "vendor" "example.com:foo"

let test_contexts () =
  let t = Context.set_jsont in
  roundtrip "contexts" t {|{"work":true,"private":true}|}
    {|{"private":true,"work":true}|};
  (match decode t {|{"work":false}|} with
  | Ok _ -> Alcotest.fail "a context mapped to false should not decode"
  | Error _ -> ());
  match Context.validate_set [ `Billing ] with
  | Ok _ -> Alcotest.fail "billing is an Address context alone"
  | Error _ -> ()

let json_of s = dec_ok "json" Jsont.json s

let test_patch () =
  let t = Patch.jsont in
  roundtrip "patch" t {|{"titles/t1/name":"escritor"}|}
    {|{"titles/t1/name":"escritor"}|};
  let p = dec_ok "patch" t {|{"titles/t1/name":"escritor"}|} in
  let card = json_of {|{"titles":{"t1":{"name":"novelist"}}}|} in
  (match Patch.apply p card with
  | Ok j ->
      Alcotest.(check string)
        "patched" {|{"titles":{"t1":{"name":"escritor"}}}|}
        (enc_ok "patched" Jsont.json j)
  | Error msg -> Alcotest.failf "apply failed: %s" msg);
  let remove = Patch.v [ ("titles/t1/name", Patch.Remove) ] in
  (match Patch.apply remove card with
  | Ok j ->
      Alcotest.(check string)
        "removed" {|{"titles":{"t1":{}}}|}
        (enc_ok "removed" Jsont.json j)
  | Error msg -> Alcotest.failf "removal failed: %s" msg);
  let missing = Patch.v [ ("nope/deep", Patch.Set (Jsont.Json.bool true)) ] in
  (match Patch.apply missing card with
  | Ok _ -> Alcotest.fail "a patch into a missing parent is invalid"
  | Error _ -> ());
  let overlap =
    Patch.v
      [
        ("titles", Patch.Set (Jsont.Json.null ()));
        ("titles/t1/name", Patch.Remove);
      ]
  in
  (match Patch.validate overlap with
  | Ok _ -> Alcotest.fail "a key that prefixes another is invalid"
  | Error _ -> ());
  (* Every character below "/" sorts between a key and its extensions, so a
     third key may separate the two in sort order. Section 1.4.3 condition 3
     rejects the patch all the same. *)
  let separated =
    Patch.v
      [
        ("name", Patch.Set (Jsont.Json.bool true));
        ("name-vendor", Patch.Set (Jsont.Json.bool true));
        ("name/full", Patch.Set (Jsont.Json.bool true));
      ]
  in
  (match Patch.validate separated with
  | Ok _ -> Alcotest.fail "a key that prefixes a non-adjacent key is invalid"
  | Error _ -> ());
  (* A reference token that is not valid UTF-8 is not JSON Pointer syntax, so
     the patch cannot be built. *)
  (match Patch.of_list [ ("na\xffme", Patch.Set (Jsont.Json.bool true)) ] with
  | Ok _ -> Alcotest.fail "a key that is not valid UTF-8 is invalid"
  | Error _ -> ());
  (* The codec does not check keys, so a malformed one still arrives from the
     wire. apply returns a result, so nothing may escape as an exception. *)
  let decoded =
    match Jsont_bytesrw.decode_string Patch.jsont {|{"a~2b": 1}|} with
    | Ok p -> p
    | Error m -> Alcotest.fail m
  in
  match Patch.apply decoded card with
  | Ok _ -> Alcotest.fail "a key with a bad escape is invalid"
  | Error _ -> ()
  | exception Jsont.Error _ ->
      Alcotest.fail "apply raised instead of returning an error"

(* RFC 7529 registers hyphenated CLDR calendar names such as islamic-civil. *)
let test_calendar_scale () =
  let d =
    Date.Partial_date.make ~year:1445 ~calendar_scale:"islamic-civil" ()
  in
  (match Date.Partial_date.validate d with
  | Ok _ -> ()
  | Error msg -> Alcotest.failf "islamic-civil rejected: %s" msg);
  let d = Date.Partial_date.make ~year:1445 ~calendar_scale:"Islamic" () in
  match Date.Partial_date.validate d with
  | Ok _ -> Alcotest.fail "an uppercase calendar scale is invalid"
  | Error _ -> ()

let test_patch_construction () =
  (match Patch.of_list [ ("a", Patch.Remove); ("a", Patch.Remove) ] with
  | Ok _ -> Alcotest.fail "a duplicate key is invalid"
  | Error _ -> ());
  (match Patch.of_list [ ("a~2", Patch.Remove) ] with
  | Ok _ -> Alcotest.fail "a bad escape is invalid"
  | Error _ -> ());
  (match Patch.v [ ("", Patch.Remove) ] with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "v raises on an empty key");
  match Patch.find (Patch.v [ ("a", Patch.Remove) ]) "a" with
  | Some Patch.Remove -> ()
  | _ -> Alcotest.fail "find returns the entry"

(* RFC 7493 Section 2.3 forbids an object from naming a member twice. *)
let test_duplicate_member () =
  match decode Name.Nickname.jsont {|{"name":"x","a":1,"a":2}|} with
  | Ok _ -> Alcotest.fail "a duplicate unknown member should not decode"
  | Error _ -> ()

let () =
  Alcotest.run "jscontact"
    [
      ( "foundation",
        [
          Alcotest.test_case "UTCDateTime" `Quick test_utc;
          Alcotest.test_case "PartialDate|Timestamp" `Quick test_date_union;
          Alcotest.test_case "PartialDate validate" `Quick
            test_partial_date_validate;
          Alcotest.test_case "calendarScale" `Quick test_calendar_scale;
          Alcotest.test_case "unknown members" `Quick test_unknown_kept;
          Alcotest.test_case "UnsignedInt" `Quick test_unsigned;
          Alcotest.test_case "unknown add and remove" `Quick
            test_unknown_add_remove;
          Alcotest.test_case "Id" `Quick test_id;
          Alcotest.test_case "vendor names" `Quick test_vendor;
          Alcotest.test_case "contexts" `Quick test_contexts;
          Alcotest.test_case "PatchObject" `Quick test_patch;
          Alcotest.test_case "PatchObject construction" `Quick
            test_patch_construction;
          Alcotest.test_case "duplicate members" `Quick test_duplicate_member;
        ] );
    ]

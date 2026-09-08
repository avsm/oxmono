(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Card tests, over the complete examples of RFC 9553.

    Several figures in the RFC are fragments rather than whole Cards: they show
    one property and leave out the mandatory [@type], [version] and [uid]. Those
    are spliced into a minimal Card here, and noted where that happens. *)

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

let roundtrip name s expected =
  let t = Card.jsont in
  let v = dec_ok name t s in
  let s1 = enc_ok name t v in
  Alcotest.(check string) name expected s1;
  let v2 = dec_ok name t s1 in
  Alcotest.(check string) (name ^ " (stable)") s1 (enc_ok name t v2)

(* For a Card too large to pin byte for byte, check that a decode and encode
   cycle reaches a fixed point and that the value survives it unchanged. *)
let stable name s =
  let t = Card.jsont in
  let v = dec_ok name t s in
  let s1 = enc_ok name t v in
  let v2 = dec_ok name t s1 in
  Alcotest.(check string) (name ^ " (stable)") s1 (enc_ok name t v2);
  Alcotest.(check bool) (name ^ " (equal)") true (Card.equal v v2);
  v

let dec_fails name s =
  match decode Card.jsont s with
  | Ok _ -> Alcotest.failf "%s: should not decode" name
  | Error _ -> ()

let invalid name c =
  match Card.validate c with
  | Ok _ -> Alcotest.failf "%s: should not validate" name
  | Error _ -> ()

let valid name c =
  match Card.validate c with
  | Ok _ -> ()
  | Error msg -> Alcotest.failf "%s: rejected: %s" name msg

let localized name s ~language expected =
  let c = dec_ok name Card.jsont s in
  match Card.localize c ~language with
  | Error msg -> Alcotest.failf "%s: localize failed: %s" name msg
  | Ok None -> Alcotest.failf "%s: no localization for %S" name language
  | Ok (Some c) ->
      Alcotest.(check string) name expected (enc_ok name Card.jsont c)

(* Figure 6. *)
let figure6 =
  {|{"@type":"Card","version":"1.0","uid":"22B2C7DF-9120-4969-8460-05956FE6B065","kind":"individual","name":{"components":[{"kind":"given","value":"John"},{"kind":"surname","value":"Doe"}],"isOrdered":true}}|}

let test_basic () =
  (* "individual" is the default kind of Section 2.1.4, so it is not written
     back. *)
  roundtrip "Figure 6" figure6
    {|{"@type":"Card","version":"1.0","uid":"22B2C7DF-9120-4969-8460-05956FE6B065","name":{"components":[{"value":"John","kind":"given"},{"value":"Doe","kind":"surname"}],"isOrdered":true}}|};
  valid "Figure 6" (dec_ok "Figure 6" Card.jsont figure6)

(* Section 2.1.1, 2.1.2 and 2.1.9 make @type, version and uid mandatory. *)
let test_mandatory () =
  dec_fails "no @type" {|{"version":"1.0","uid":"u1"}|};
  dec_fails "no version" {|{"@type":"Card","uid":"u1"}|};
  dec_fails "no uid" {|{"@type":"Card","version":"1.0"}|};
  dec_fails "wrong @type" {|{"@type":"ContactCard","version":"1.0","uid":"u1"}|}

(* Figure 11, spliced into a Card. *)
let test_group () =
  let s =
    {|{"@type":"Card","version":"1.0","kind":"group","name":{"full":"The Doe family"},"uid":"urn:uuid:ab4310aa-fa43-11e9-8f0b-362b9e155667","members":{"urn:uuid:03a0e51f-d1aa-4385-8a53-e29025acd8af":true,"urn:uuid:b8767877-b4a1-4c70-9acc-505d3819e519":true}}|}
  in
  let c = stable "Figure 11" s in
  valid "Figure 11" c;
  (* Section 2.1.6: if members is set, kind MUST be "group". *)
  invalid "members without a group kind" { c with Card.kind = `Individual }

(* Figure 42, and a vendor property Section 1.8.1 requires to be preserved. *)
let test_keywords_and_vendor () =
  roundtrip "Figure 42"
    {|{"@type":"Card","version":"1.0","uid":"u1","keywords":{"internet":true,"IETF":true},"example.com:pinned":true}|}
    {|{"@type":"Card","version":"1.0","uid":"u1","keywords":{"IETF":true,"internet":true},"example.com:pinned":true}|}

(* Figure 40: localizing a nested property. Note that the "title" kind is the
   default of Section 2.2.5, so it is not written back. *)
let test_localize_nested () =
  let s =
    {|{"@type":"Card","version":"1.0","uid":"u1","name":{"full":"Gabriel García Márquez"},"titles":{"t1":{"kind":"title","name":"novelist"}},"localizations":{"es":{"titles/t1/name":"escritor"}}}|}
  in
  localized "Figure 40" s ~language:"es"
    {|{"@type":"Card","version":"1.0","language":"es","uid":"u1","name":{"full":"Gabriel García Márquez"},"titles":{"t1":{"name":"escritor"}}}|};
  let c = dec_ok "Figure 40" Card.jsont s in
  match Card.localize c ~language:"de" with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "there is no German localization"
  | Error msg -> Alcotest.failf "localize failed: %s" msg

(* Figure 39: localizing a top-level property by replacing it wholesale. *)
let test_localize_toplevel () =
  localized "Figure 39"
    {|{"@type":"Card","version":"1.0","uid":"u2","name":{"components":[{"kind":"title","value":"Mr."},{"kind":"given","value":"Ivan"},{"kind":"given2","value":"Petrovich"},{"kind":"surname","value":"Vasiliev"}]},"localizations":{"uk-Cyrl":{"name":{"components":[{"kind":"title","value":"г-н"},{"kind":"given","value":"Иван"},{"kind":"given2","value":"Петрович"},{"kind":"surname","value":"Васильев"}]}}}}|}
    ~language:"uk-Cyrl"
    {|{"@type":"Card","version":"1.0","language":"uk-Cyrl","uid":"u2","name":{"components":[{"value":"г-н","kind":"title"},{"value":"Иван","kind":"given"},{"value":"Петрович","kind":"given2"},{"value":"Васильев","kind":"surname"}]}}|}

(* Figure 20: a localization that patches into an array, which Section 1.4.3
   allows for a replacement but not for an insertion or a removal. *)
let test_localize_into_array () =
  localized "Figure 20"
    {|{"@type":"Card","version":"1.0","uid":"u3","language":"zh-Hant","name":{"components":[{"kind":"surname","value":"孫"},{"kind":"given","value":"中山"},{"kind":"given2","value":"文"},{"kind":"given2","value":"逸仙"}]},"localizations":{"yue":{"name/phoneticSystem":"jyut","name/phoneticScript":"Latn","name/components/0/phonetic":"syun1","name/components/1/phonetic":"zung1saan1","name/components/2/phonetic":"man4","name/components/3/phonetic":"jat6sin1"}}}|}
    ~language:"yue"
    {|{"@type":"Card","version":"1.0","language":"yue","uid":"u3","name":{"components":[{"value":"孫","kind":"surname","phonetic":"syun1"},{"value":"中山","kind":"given","phonetic":"zung1saan1"},{"value":"文","kind":"given2","phonetic":"man4"},{"value":"逸仙","kind":"given2","phonetic":"jat6sin1"}],"phoneticScript":"Latn","phoneticSystem":"jyut"}}|}

(* Figure 33: an address and its Japanese localization. *)
let test_localize_address () =
  let s =
    {|{"@type":"Card","version":"1.0","uid":"u4","addresses":{"k26":{"components":[{"kind":"block","value":"2-7"},{"kind":"separator","value":"-"},{"kind":"number","value":"2"},{"kind":"separator","value":" "},{"kind":"district","value":"Marunouchi"},{"kind":"locality","value":"Chiyoda-ku"},{"kind":"region","value":"Tokyo"},{"kind":"separator","value":" "},{"kind":"postcode","value":"100-8994"}],"defaultSeparator":", ","full":"2-7-2 Marunouchi, Chiyoda-ku, Tokyo 100-8994","isOrdered":true}},"localizations":{"jp":{"addresses/k26":{"components":[{"kind":"region","value":"東京都"},{"kind":"locality","value":"千代田区"},{"kind":"district","value":"丸ノ内"},{"kind":"block","value":"2-7"},{"kind":"separator","value":"-"},{"kind":"number","value":"2"},{"kind":"postcode","value":"〒100-8994"}],"defaultSeparator":"","full":"〒100-8994東京都千代田区丸ノ内2-7-2","isOrdered":true}}}}|}
  in
  let c = stable "Figure 33" s in
  valid "Figure 33" c;
  match Card.localize c ~language:"jp" with
  | Error msg -> Alcotest.failf "localize failed: %s" msg
  | Ok None -> Alcotest.fail "no Japanese localization"
  | Ok (Some c) ->
      valid "Figure 33 localized" c;
      Alcotest.(check bool)
        "the localizations are dropped from the localized copy" true
        (c.Card.localizations = None)

(* Section 2.7.1: "A patch MUST NOT target the localizations property." *)
let test_localize_self_reference () =
  let c =
    dec_ok "self reference" Card.jsont
      {|{"@type":"Card","version":"1.0","uid":"u5","localizations":{"fr":{"localizations":{}}}}|}
  in
  invalid "a localization that patches localizations" c

let test_version () =
  let c =
    dec_ok "version" Card.jsont {|{"@type":"Card","version":"1.0","uid":"u6"}|}
  in
  valid "1.0" c;
  (* Section 1.9: a later minor version is additive, so it is accepted; a later
     major version is backwards incompatible and is not. *)
  valid "a later minor version" { c with Card.version = "1.5" };
  invalid "a later major version" { c with Card.version = "2.0" };
  invalid "a version with no minor" { c with Card.version = "1" };
  invalid "a version that is not numeric" { c with Card.version = "1.0-draft" }

(* Section 1.7.4 keeps unknown members, but their order on the wire is not part
   of the Card: two Cards that differ only in it are the same Card. *)
let test_equal_ignores_member_order () =
  let a =
    dec_ok "a" Card.jsont
      {|{"@type":"Card","version":"1.0","uid":"u7","example.com:a":1,"example.com:b":2}|}
  in
  let b =
    dec_ok "b" Card.jsont
      {|{"@type":"Card","version":"1.0","uid":"u7","example.com:b":2,"example.com:a":1}|}
  in
  Alcotest.(check bool) "the same Card" true (Card.equal a b);
  Alcotest.(check bool)
    "a different Card" false
    (Card.equal a { a with Card.uid = "u8" })

(* Section 1.3 requires I-JSON, which forbids naming a member twice. *)
let test_duplicate_members () =
  dec_fails "a duplicate unknown member"
    {|{"@type":"Card","version":"1.0","uid":"u9","example.com:a":1,"example.com:a":2}|}

(* Section 1.7.3 reserves "extra"; Section 1.7.1 rejects a name that differs
   from a registered one only in case. *)
let test_reserved_and_case () =
  let c =
    dec_ok "extra" Card.jsont
      {|{"@type":"Card","version":"1.0","uid":"u10","extra":1}|}
  in
  invalid "the reserved property extra" c;
  let c =
    dec_ok "case" Card.jsont
      {|{"@type":"Card","version":"1.0","uid":"u11","uID":"other"}|}
  in
  invalid "a property differing only in case from uid" c

(* One Card carrying a property from every section of RFC 9553 Section 2. *)
let kitchen_sink =
  {|{"@type":"Card","version":"1.0","created":"2022-09-30T14:35:10Z","kind":"individual","language":"en","prodId":"ACME Contacts App version 1.23.5","relatedTo":{"urn:uuid:f81d4fae-7dec-11d0-a765-00a0c91e6bf6":{"relation":{"friend":true}}},"uid":"urn:uuid:f81d4fae-7dec-11d0-a765-00a0c91e6bf6","updated":"2021-10-31T22:27:10Z","name":{"components":[{"kind":"given","value":"Robert"},{"kind":"given2","value":"Pau"},{"kind":"surname","value":"Shou Chang"}],"sortAs":{"surname":"Pau Shou Chang","given":"Robert"},"isOrdered":true},"nicknames":{"k391":{"name":"Johnny"}},"organizations":{"o1":{"name":"ABC, Inc.","units":[{"name":"North American Division"},{"name":"Marketing"}],"sortAs":"ABC"}},"speakToAs":{"grammaticalGender":"neuter","pronouns":{"k19":{"pronouns":"they/them","pref":2}}},"titles":{"le9":{"kind":"title","name":"Research Scientist"}},"emails":{"e1":{"contexts":{"work":true},"address":"jqpublic@xyz.example.com"}},"onlineServices":{"x2":{"service":"Mastodon","user":"@alice@example2.com","uri":"https://example2.com/@alice"}},"phones":{"tel0":{"contexts":{"private":true},"features":{"voice":true},"number":"tel:+1-555-555-5555;ext=5555","pref":1}},"preferredLanguages":{"l1":{"language":"en","contexts":{"work":true},"pref":1}},"calendars":{"calA":{"kind":"calendar","uri":"webcal://calendar.example.com/calA.ics"}},"schedulingAddresses":{"sched1":{"uri":"mailto:janedoe@example.com"}},"addresses":{"k23":{"contexts":{"work":true},"components":[{"kind":"number","value":"54321"},{"kind":"separator","value":" "},{"kind":"name","value":"Oak St"},{"kind":"locality","value":"Reston"},{"kind":"region","value":"VA"},{"kind":"separator","value":" "},{"kind":"postcode","value":"20190"},{"kind":"country","value":"USA"}],"countryCode":"US","defaultSeparator":", ","isOrdered":true}},"cryptoKeys":{"mykey1":{"uri":"https://www.example.com/keys/jdoe.cer"}},"directories":{"dir2":{"kind":"directory","uri":"ldap://ldap.example/o=Example%20Tech,ou=Engineering","pref":1}},"links":{"link3":{"kind":"contact","uri":"mailto:contact@example.com","pref":1}},"media":{"res47":{"kind":"logo","uri":"https://www.example.com/pub/logos/abccorp.jpg"}},"anniversaries":{"k8":{"kind":"birth","date":{"year":1953,"month":4,"day":15}},"k9":{"kind":"death","date":{"@type":"Timestamp","utc":"2019-10-15T23:10:00Z"},"place":{"full":"4445 Tree Street\nNew England, ND 58647\nUSA"}}},"keywords":{"internet":true,"IETF":true},"notes":{"n1":{"note":"Open office hours are 1600 to 1715 EST, Mon-Fri","created":"2022-11-23T15:01:32Z","author":{"name":"John"}}},"personalInfo":{"pi2":{"kind":"expertise","value":"chemistry","level":"high"}}}|}

let test_kitchen_sink () =
  let c = stable "every property" kitchen_sink in
  valid "every property" c;
  Alcotest.(check bool) "the name is set" true (c.Card.name <> None);
  Alcotest.(check bool) "the media is set" true (c.Card.media <> None)

(* RFC 8620 Section 5.1: a JMAP /get may name the properties it wants, and a
   server then returns those alone. The oracle Cyrus answers a ContactCard/get
   with properties ["name"] by sending name, id and addressBookIds and nothing
   else, so the mandatory @type, version and uid are all absent. *)
let test_partial () =
  let t = Card.partial_jsont in
  let partial =
    {|{"name":{"full":"Partial Probe"},"id":"K1","addressBookIds":{"Default":true}}|}
  in
  (match decode Card.jsont partial with
  | Ok _ -> Alcotest.fail "a partial Card should not decode as a whole one"
  | Error _ -> ());
  let c = dec_ok "partial" t partial in
  Alcotest.(check string) "uid is empty" "" c.Card.uid;
  Alcotest.(check string) "version defaults" Card.version_1_0 c.Card.version;
  invalid "a partial Card is not a valid Card" c;
  (* The JMAP properties are unknown to RFC 9553 and are kept as members. *)
  Alcotest.(check bool)
    "id is kept" true
    (Unknown.find c.Card.unknown "id" <> None);
  (* Encoding writes @type and version, which a server requires on a create,
     and omits the empty uid rather than writing one no Card may have. *)
  Alcotest.(check string)
    "re-encoded"
    {|{"@type":"Card","version":"1.0","name":{"full":"Partial Probe"},"id":"K1","addressBookIds":{"Default":true}}|}
    (enc_ok "partial" t c);
  (* A whole Card round trips through partial_jsont unchanged. *)
  let whole = dec_ok "whole" t figure6 in
  valid "a whole Card read as partial" whole

(* RFC 9610 Section 7.5 reserves id and addressBookIds in a Card and blobId in
   a Media, so a JMAP ContactCard is not a valid standalone JSContact Card. *)
let test_jmap_reserved_names () =
  let c =
    dec_ok "jmap card" Card.partial_jsont
      {|{"@type":"Card","version":"1.0","uid":"u12","id":"K1","addressBookIds":{"Default":true}}|}
  in
  invalid "a Card carrying the JMAP properties" c;
  (* They are reserved in a Card alone, so the same names elsewhere are not. *)
  Alcotest.(check bool)
    "reserved in a Card" true
    (Registry.is_reserved ~in_type:"Card" "id");
  Alcotest.(check bool)
    "not reserved in a Nickname" false
    (Registry.is_reserved ~in_type:"Nickname" "id");
  Alcotest.(check bool)
    "reserved in a Media" true
    (Registry.is_reserved ~in_type:"Media" "blobId");
  Alcotest.(check bool)
    "extra is reserved everywhere" true
    (Registry.is_reserved "extra")

(* A name reserved in one object type is a valid vendor member in another, and
   a name that differs from a reserved one only in case is invalid there. *)
let test_reserved_scoping () =
  (match Registry.validate_property_name ~in_type:"Nickname" "id" with
  | Ok _ -> ()
  | Error msg -> Alcotest.failf "id is not reserved in a Nickname: %s" msg);
  (match Registry.validate_property_name ~in_type:"Card" "ID" with
  | Ok _ -> Alcotest.fail "ID differs from the reserved id only in case"
  | Error _ -> ());
  match Registry.validate_property_name "blobId" with
  | Ok _ -> ()
  | Error msg -> Alcotest.failf "blobId is reserved in a Media alone: %s" msg

let () =
  Alcotest.run "jscontact"
    [
      ( "card",
        [
          Alcotest.test_case "a basic Card" `Quick test_basic;
          Alcotest.test_case "mandatory properties" `Quick test_mandatory;
          Alcotest.test_case "a group Card" `Quick test_group;
          Alcotest.test_case "keywords and a vendor property" `Quick
            test_keywords_and_vendor;
          Alcotest.test_case "version" `Quick test_version;
          Alcotest.test_case "every property" `Quick test_kitchen_sink;
          Alcotest.test_case "equality ignores member order" `Quick
            test_equal_ignores_member_order;
          Alcotest.test_case "duplicate members" `Quick test_duplicate_members;
          Alcotest.test_case "reserved and mis-cased names" `Quick
            test_reserved_and_case;
          Alcotest.test_case "a partial Card" `Quick test_partial;
          Alcotest.test_case "the JMAP reserved names" `Quick
            test_jmap_reserved_names;
          Alcotest.test_case "reserved name scoping" `Quick
            test_reserved_scoping;
        ] );
      ( "localizations",
        [
          Alcotest.test_case "a nested property" `Quick test_localize_nested;
          Alcotest.test_case "a top-level property" `Quick
            test_localize_toplevel;
          Alcotest.test_case "into an array" `Quick test_localize_into_array;
          Alcotest.test_case "an address" `Quick test_localize_address;
          Alcotest.test_case "patching localizations" `Quick
            test_localize_self_reference;
        ] );
    ]

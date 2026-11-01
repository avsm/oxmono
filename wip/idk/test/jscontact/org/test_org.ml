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

let dec_fails name t s =
  match decode t s with
  | Ok _ -> Alcotest.failf "%s: should not decode" name
  | Error _ -> ()

let valid_fails name r =
  match r with
  | Ok _ -> Alcotest.failf "%s: should not validate" name
  | Error _ -> ()

let valid_ok name r =
  match r with
  | Ok _ -> ()
  | Error msg -> Alcotest.failf "%s: valid value rejected: %s" name msg

(* Section 2.2.3 *)

let organizations = Json.Map.of_id Org.Organization.jsont

let test_organizations () =
  (* Figure 22. *)
  roundtrip "organizations" organizations
    {|{"o1":{"name":"ABC, Inc.","units":[{"name":"North American Division"},{"name":"Marketing"}],"sortAs":"ABC"}}|}
    {|{"o1":{"name":"ABC, Inc.","units":[{"name":"North American Division"},{"name":"Marketing"}],"sortAs":"ABC"}}|};
  let t = Org.Organization.jsont in
  roundtrip "organization contexts" t
    {|{"name":"Choir","contexts":{"private":true}}|}
    {|{"name":"Choir","contexts":{"private":true}}|};
  (* Section 1.3.4 implies the type of a property value, so @type decodes and
     is not written back. *)
  roundtrip "organization @type" t {|{"@type":"Organization","name":"ABC"}|}
    {|{"name":"ABC"}|};
  dec_fails "organization of another type" t {|{"@type":"OrgUnit","name":"x"}|}

let test_organization_unknown () =
  roundtrip "unknown members are kept" Org.Organization.jsont
    {|{"name":"ABC","example.com:foo":{"bar":true}}|}
    {|{"name":"ABC","example.com:foo":{"bar":true}}|};
  roundtrip "unknown members of a unit are kept" Org.Organization.Org_unit.jsont
    {|{"name":"Marketing","example.com:foo":[1]}|}
    {|{"name":"Marketing","example.com:foo":[1]}|}

let test_org_unit () =
  let t = Org.Organization.Org_unit.jsont in
  roundtrip "unit" t {|{"name":"Marketing","sortAs":"Mkt"}|}
    {|{"name":"Marketing","sortAs":"Mkt"}|};
  dec_fails "a unit without a name" t {|{"sortAs":"Mkt"}|};
  valid_ok "unit"
    (Org.Organization.Org_unit.validate
       (Org.Organization.Org_unit.make "Marketing"))

let test_organization_validate () =
  let make = Org.Organization.make in
  valid_fails "neither name nor units" (Org.Organization.validate (make ()));
  valid_fails "units with no entry"
    (Org.Organization.validate (make ~units:[] ()));
  valid_fails "an Address context"
    (Org.Organization.validate (make ~name:"ABC" ~contexts:[ `Billing ] ()));
  valid_fails "a malformed vendor context"
    (Org.Organization.validate
       (make ~name:"ABC" ~contexts:[ `Vendor "nocolon" ] ()));
  valid_fails "an invalid unit"
    (Org.Organization.validate
       (make
          ~units:
            [
              Org.Organization.Org_unit.make
                ~unknown:
                  (Unknown.of_list [ ("bad name", Jsont.Json.bool true) ])
                "Marketing";
            ]
          ()));
  valid_ok "units alone"
    (Org.Organization.validate
       (make ~units:[ Org.Organization.Org_unit.make "Marketing" ] ()));
  valid_ok "name and contexts"
    (Org.Organization.validate (make ~name:"ABC" ~contexts:[ `Work ] ()))

(* Section 2.2.4 *)

let test_speak_to_as () =
  let t = Org.Speak_to_as.jsont in
  (* Figure 23. *)
  roundtrip "speakToAs" t
    {|{"grammaticalGender":"neuter","pronouns":{"k19":{"pronouns":"they/them","pref":2},"k32":{"pronouns":"xe/xir","pref":1}}}|}
    {|{"grammaticalGender":"neuter","pronouns":{"k19":{"pronouns":"they/them","pref":2},"k32":{"pronouns":"xe/xir","pref":1}}}|};
  roundtrip "a vendor-specific gender" t
    {|{"grammaticalGender":"example.com:epicene"}|}
    {|{"grammaticalGender":"example.com:epicene"}|};
  roundtrip "speakToAs @type" t
    {|{"@type":"SpeakToAs","grammaticalGender":"feminine"}|}
    {|{"grammaticalGender":"feminine"}|}

let test_speak_to_as_unknown () =
  roundtrip "unknown members are kept" Org.Speak_to_as.jsont
    {|{"grammaticalGender":"common","example.com:foo":"bar"}|}
    {|{"grammaticalGender":"common","example.com:foo":"bar"}|}

let test_pronouns () =
  let t = Org.Speak_to_as.Pronouns.jsont in
  roundtrip "pronouns" t
    {|{"pronouns":"she/her","contexts":{"work":true},"pref":1}|}
    {|{"pronouns":"she/her","contexts":{"work":true},"pref":1}|};
  roundtrip "unknown members are kept" t
    {|{"pronouns":"they/them","example.com:foo":null}|}
    {|{"pronouns":"they/them","example.com:foo":null}|};
  dec_fails "pronouns without a pronouns member" t {|{"pref":1}|};
  dec_fails "a negative pref" t {|{"pronouns":"they/them","pref":-1}|}

let test_pronouns_validate () =
  let make = Org.Speak_to_as.Pronouns.make in
  let validate = Org.Speak_to_as.Pronouns.validate in
  valid_fails "a pref of 0" (validate (make ~pref:0 "they/them"));
  valid_fails "a pref of 101" (validate (make ~pref:101 "they/them"));
  valid_fails "an Address context"
    (validate (make ~contexts:[ `Delivery ] "they/them"));
  valid_ok "pronouns" (validate (make ~contexts:[ `Work ] ~pref:1 "they/them"))

let test_speak_to_as_validate () =
  let make = Org.Speak_to_as.make in
  let validate = Org.Speak_to_as.validate in
  valid_fails "neither grammaticalGender nor pronouns" (validate (make ()));
  valid_fails "a malformed vendor gender"
    (validate (make ~grammatical_gender:(`Vendor "nocolon") ()));
  valid_fails "an invalid entry"
    (validate
       (make
          ~pronouns:
            [ (Id.v "k19", Org.Speak_to_as.Pronouns.make ~pref:0 "they/them") ]
          ()));
  valid_ok "a gender alone" (validate (make ~grammatical_gender:`Neuter ()));
  valid_ok "pronouns alone"
    (validate
       (make
          ~pronouns:[ (Id.v "k19", Org.Speak_to_as.Pronouns.make "they/them") ]
          ()))

(* Section 2.2.5 *)

let titles = Json.Map.of_id Org.Title.jsont

let test_titles () =
  (* Figure 24. The kind of "le9" is the default "title", which Section 2.2.5
     lets an encoder omit. *)
  roundtrip "titles" titles
    {|{"le9":{"kind":"title","name":"Research Scientist"},"k2":{"kind":"role","name":"Project Leader","organizationId":"o2"}}|}
    {|{"k2":{"name":"Project Leader","kind":"role","organizationId":"o2"},"le9":{"name":"Research Scientist"}}|};
  let t = Org.Title.jsont in
  roundtrip "a default kind is not written back" t
    {|{"name":"Research Scientist"}|} {|{"name":"Research Scientist"}|};
  roundtrip "a vendor-specific kind" t
    {|{"name":"Bard","kind":"example.com:honorific"}|}
    {|{"name":"Bard","kind":"example.com:honorific"}|};
  roundtrip "title @type" t {|{"@type":"Title","name":"Bard"}|}
    {|{"name":"Bard"}|};
  dec_fails "a title without a name" t {|{"kind":"role"}|};
  dec_fails "an organizationId outside the Id alphabet" t
    {|{"name":"Bard","organizationId":"o 2"}|};
  let v = dec_ok "kind" t {|{"name":"Research Scientist"}|} in
  Alcotest.(check bool)
    "an absent kind is `Title" true
    (Org.Title.Kind.equal v.Org.Title.kind `Title)

let test_titles_unknown () =
  roundtrip "unknown members are kept" Org.Title.jsont
    {|{"name":"Bard","example.com:foo":{"bar":true}}|}
    {|{"name":"Bard","example.com:foo":{"bar":true}}|}

let test_title_validate () =
  valid_fails "a malformed vendor kind"
    (Org.Title.validate (Org.Title.make ~kind:(`Vendor "nocolon") "Bard"));
  valid_fails "a malformed unknown member"
    (Org.Title.validate
       (Org.Title.make
          ~unknown:(Unknown.of_list [ ("bad name", Jsont.Json.bool true) ])
          "Bard"));
  valid_ok "a title with an organizationId"
    (Org.Title.validate
       (Org.Title.make ~kind:`Role ~organization_id:(Id.v "o2") "Project Leader"))

let () =
  Alcotest.run "jscontact"
    [
      ( "org",
        [
          Alcotest.test_case "organizations" `Quick test_organizations;
          Alcotest.test_case "Organization unknown members" `Quick
            test_organization_unknown;
          Alcotest.test_case "OrgUnit" `Quick test_org_unit;
          Alcotest.test_case "Organization validate" `Quick
            test_organization_validate;
          Alcotest.test_case "speakToAs" `Quick test_speak_to_as;
          Alcotest.test_case "SpeakToAs unknown members" `Quick
            test_speak_to_as_unknown;
          Alcotest.test_case "Pronouns" `Quick test_pronouns;
          Alcotest.test_case "Pronouns validate" `Quick test_pronouns_validate;
          Alcotest.test_case "SpeakToAs validate" `Quick
            test_speak_to_as_validate;
          Alcotest.test_case "titles" `Quick test_titles;
          Alcotest.test_case "Title unknown members" `Quick test_titles_unknown;
          Alcotest.test_case "Title validate" `Quick test_title_validate;
        ] );
    ]

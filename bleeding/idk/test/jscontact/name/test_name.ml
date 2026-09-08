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

let invalid name r =
  match r with
  | Ok _ -> Alcotest.failf "%s: should not validate" name
  | Error _ -> ()

let valid name r =
  match r with
  | Ok _ -> ()
  | Error msg -> Alcotest.failf "%s: rejected: %s" name msg

(* Section 2.2.1.2. *)
let test_component () =
  let t = Name.Component.jsont in
  roundtrip "component" t {|{"kind":"given","value":"Vincent"}|}
    {|{"value":"Vincent","kind":"given"}|};
  (* Section 1.3.4 implies @type, so it is accepted and not written back. *)
  roundtrip "component with @type" t
    {|{"@type":"NameComponent","kind":"surname","value":"van Gogh"}|}
    {|{"value":"van Gogh","kind":"surname"}|};
  roundtrip "vendor kind" t {|{"kind":"example.com:foo","value":"x"}|}
    {|{"value":"x","kind":"example.com:foo"}|};
  (* Section 2.2.1.2 makes both value and kind mandatory. *)
  dec_fails "component without value" t {|{"kind":"given"}|};
  dec_fails "component without kind" t {|{"value":"Vincent"}|};
  dec_fails "component with a wrong @type" t
    {|{"@type":"Name","kind":"given","value":"Vincent"}|}

(* Section 1.7.4 requires an unknown member to survive a decode and encode
   cycle. *)
let test_component_unknown () =
  roundtrip "component keeps unknown members" Name.Component.jsont
    {|{"kind":"given","value":"Vincent","example.com:foo":{"bar":true}}|}
    {|{"value":"Vincent","kind":"given","example.com:foo":{"bar":true}}|}

let test_component_validate () =
  let c = Name.Component.make `Given "Vincent" in
  valid "a given name" (Name.Component.validate c);
  let c = Name.Component.make (`Vendor "notAVendorName") "x" in
  invalid "a vendor kind with no prefix" (Name.Component.validate c)

(* Figures 16, 17, 18, 19 and 20. *)
let test_name () =
  let t = Name.jsont in
  roundtrip "Figure 16" t
    {|{"components":[{"kind":"given","value":"Vincent"},{"kind":"surname","value":"van Gogh"}],"isOrdered":true}|}
    {|{"components":[{"value":"Vincent","kind":"given"},{"value":"van Gogh","kind":"surname"}],"isOrdered":true}|};
  roundtrip "Figure 17" t
    {|{"components":[{"kind":"given","value":"Diego"},{"kind":"surname","value":"Rivera"},{"kind":"surname2","value":"Barrientos"}],"isOrdered":true}|}
    {|{"components":[{"value":"Diego","kind":"given"},{"value":"Rivera","kind":"surname"},{"value":"Barrientos","kind":"surname2"}],"isOrdered":true}|};
  roundtrip "Figure 18" t {|{"full":"Mr. John Q. Public, Esq."}|}
    {|{"full":"Mr. John Q. Public, Esq."}|};
  (* A sortAs map decodes ordered by key, whatever the order on the wire. *)
  roundtrip "Figure 19" t
    {|{"components":[{"kind":"given","value":"Robert"},{"kind":"given2","value":"Pau"},{"kind":"surname","value":"Shou Chang"}],"sortAs":{"surname":"Pau Shou Chang","given":"Robert"},"isOrdered":true}|}
    {|{"components":[{"value":"Robert","kind":"given"},{"value":"Pau","kind":"given2"},{"value":"Shou Chang","kind":"surname"}],"isOrdered":true,"sortAs":{"given":"Robert","surname":"Pau Shou Chang"}}|};
  roundtrip "Figure 20" t
    {|{"components":[{"kind":"surname","value":"孫"},{"kind":"given","value":"中山"},{"kind":"given2","value":"文"},{"kind":"given2","value":"逸仙"}]}|}
    {|{"components":[{"value":"孫","kind":"surname"},{"value":"中山","kind":"given"},{"value":"文","kind":"given2"},{"value":"逸仙","kind":"given2"}]}|};
  (* Section 2.2.1.1 defaults isOrdered to false, so it is not written back. *)
  roundtrip "isOrdered false is the default" t
    {|{"full":"Mr. John Q. Public, Esq.","isOrdered":false}|}
    {|{"full":"Mr. John Q. Public, Esq."}|};
  roundtrip "name with @type" t {|{"@type":"Name","full":"John"}|}
    {|{"full":"John"}|};
  roundtrip "defaultSeparator" t
    {|{"components":[{"kind":"given","value":"John"}],"isOrdered":true,"defaultSeparator":" "}|}
    {|{"components":[{"value":"John","kind":"given"}],"isOrdered":true,"defaultSeparator":" "}|}

(* Figure 1 of Section 1.5.4. *)
let test_name_phonetic () =
  roundtrip "Figure 1" Name.jsont
    {|{"components":[{"kind":"given","value":"John","phonetic":"/ˈdʒɑːn/"},{"kind":"surname","value":"Smith","phonetic":"/smɪθ/"}],"phoneticSystem":"ipa"}|}
    {|{"components":[{"value":"John","kind":"given","phonetic":"/ˈdʒɑːn/"},{"value":"Smith","kind":"surname","phonetic":"/smɪθ/"}],"phoneticSystem":"ipa"}|};
  let n =
    dec_ok "Figure 1" Name.jsont
      {|{"components":[{"kind":"given","value":"John","phonetic":"/ˈdʒɑːn/"}],"phoneticSystem":"ipa"}|}
  in
  valid "a phonetic with a phoneticSystem" (Name.validate n)

let test_name_unknown () =
  roundtrip "name keeps unknown members" Name.jsont
    {|{"full":"John","example.com:foo":{"bar":true}}|}
    {|{"full":"John","example.com:foo":{"bar":true}}|}

let sep = Name.Component.make `Separator " "
let given = Name.Component.make `Given "Vincent"

let test_name_validate () =
  valid "Figure 16"
    (Name.validate
       (Name.make ~is_ordered:true
          ~components:[ given; Name.Component.make `Surname "van Gogh" ]
          ()));
  valid "Figure 18" (Name.validate (Name.make ~full:"Mr. John Q. Public" ()));
  (* Section 2.2.1.1: the component list must have at least one entry whose
     kind is not "separator". *)
  invalid "only separators"
    (Name.validate (Name.make ~is_ordered:true ~components:[ sep ] ()));
  invalid "no components"
    (Name.validate (Name.make ~is_ordered:true ~components:[] ()));
  (* components MUST be set if full is not. *)
  invalid "neither components nor full" (Name.validate (Name.make ()));
  (* A separator component and defaultSeparator need an ordered name. *)
  invalid "a separator in an unordered name"
    (Name.validate (Name.make ~components:[ given; sep ] ()));
  invalid "defaultSeparator in an unordered name"
    (Name.validate (Name.make ~components:[ given ] ~default_separator:" " ()));
  (* sortAs and defaultSeparator need components. *)
  invalid "sortAs without components"
    (Name.validate
       (Name.make ~full:"Vincent" ~sort_as:[ (`Given, "Vincent") ] ()));
  invalid "defaultSeparator without components"
    (Name.validate (Name.make ~full:"Vincent" ~default_separator:" " ()));
  (* Each sortAs key names a kind that some component has. *)
  invalid "sortAs by an absent kind"
    (Name.validate
       (Name.make ~components:[ given ] ~sort_as:[ (`Surname, "van Gogh") ] ()));
  valid "sortAs by a present kind"
    (Name.validate
       (Name.make ~components:[ given ] ~sort_as:[ (`Given, "Vincent") ] ()));
  (* Section 2.2.1.2: a phonetic needs a phoneticScript or phoneticSystem. *)
  invalid "a phonetic with neither script nor system"
    (Name.validate
       (Name.make
          ~components:[ Name.Component.make ~phonetic:"syun1" `Surname "孫" ]
          ()));
  valid "a phonetic with a phoneticScript"
    (Name.validate
       (Name.make ~phonetic_script:"Latn"
          ~components:[ Name.Component.make ~phonetic:"syun1" `Surname "孫" ]
          ()));
  (* Section 1.5.4: phoneticScript is a script subtag, which is four letters. *)
  invalid "a malformed phoneticScript"
    (Name.validate (Name.make ~full:"John" ~phonetic_script:"Latin" ()));
  invalid "a vendor phoneticSystem with no prefix"
    (Name.validate
       (Name.make ~full:"John" ~phonetic_system:(`Vendor "nope") ()));
  (* A validation failure deep in a name names the path that reaches it. *)
  invalid "a component with a vendor kind that has no prefix"
    (Name.validate
       (Name.make ~components:[ Name.Component.make (`Vendor "nope") "x" ] ()))

(* Section 2.2.2 and Figure 21. *)
let test_nickname () =
  let t = Name.Nickname.jsont in
  roundtrip "nickname" t {|{"name":"Johnny"}|} {|{"name":"Johnny"}|};
  roundtrip "nickname with @type" t
    {|{"@type":"Nickname","name":"Johnny","pref":1}|}
    {|{"name":"Johnny","pref":1}|};
  roundtrip "nickname contexts" t
    {|{"name":"Johnny","contexts":{"work":true,"private":true}}|}
    {|{"name":"Johnny","contexts":{"private":true,"work":true}}|};
  roundtrip "nickname keeps unknown members" t
    {|{"name":"Johnny","example.com:foo":{"bar":true}}|}
    {|{"name":"Johnny","example.com:foo":{"bar":true}}|};
  (* Section 2.2.2 makes name mandatory. *)
  dec_fails "nickname without a name" t {|{"pref":1}|};
  (* Figure 21: the nicknames property is an Id[Nickname] map. *)
  roundtrip "Figure 21" (Json.Map.of_id t) {|{"k391":{"name":"Johnny"}}|}
    {|{"k391":{"name":"Johnny"}}|}

let test_nickname_validate () =
  valid "a nickname"
    (Name.Nickname.validate
       (Name.Nickname.make ~pref:1 ~contexts:[ `Work ] "Johnny"));
  (* Section 1.5.3: pref is in the range 1 to 100. *)
  invalid "pref 0"
    (Name.Nickname.validate (Name.Nickname.make ~pref:0 "Johnny"));
  invalid "pref 101"
    (Name.Nickname.validate (Name.Nickname.make ~pref:101 "Johnny"));
  (* Section 2.5.1.1 defines billing for an Address alone. *)
  invalid "an Address context"
    (Name.Nickname.validate
       (Name.Nickname.make ~contexts:[ `Billing ] "Johnny"))

(* Section 2.2.1.2: two consecutive separator components are invalid. *)
let test_consecutive_separators () =
  let surname = Name.Component.make `Surname "van Gogh" in
  invalid "two consecutive separators"
    (Name.validate
       (Name.make ~is_ordered:true ~components:[ given; sep; sep; surname ] ()));
  valid "separators apart"
    (Name.validate
       (Name.make ~is_ordered:true ~components:[ given; sep; surname ] ()))

(* sortAs is a map, so the order of its entries is immaterial to equality. *)
let test_sort_as_equal () =
  let surname = Name.Component.make `Surname "van Gogh" in
  let a =
    Name.make ~components:[ given; surname ]
      ~sort_as:[ (`Given, "Vincent"); (`Surname, "Gogh") ]
      ()
  in
  let b =
    Name.make ~components:[ given; surname ]
      ~sort_as:[ (`Surname, "Gogh"); (`Given, "Vincent") ]
      ()
  in
  Alcotest.(check bool) "sortAs order" true (Name.equal a b)

let () =
  Alcotest.run "jscontact"
    [
      ( "name",
        [
          Alcotest.test_case "NameComponent" `Quick test_component;
          Alcotest.test_case "NameComponent unknown members" `Quick
            test_component_unknown;
          Alcotest.test_case "NameComponent validate" `Quick
            test_component_validate;
          Alcotest.test_case "Name" `Quick test_name;
          Alcotest.test_case "Name phonetic" `Quick test_name_phonetic;
          Alcotest.test_case "Name unknown members" `Quick test_name_unknown;
          Alcotest.test_case "Name validate" `Quick test_name_validate;
          Alcotest.test_case "consecutive separators" `Quick
            test_consecutive_separators;
          Alcotest.test_case "sortAs equality" `Quick test_sort_as_equal;
          Alcotest.test_case "Nickname" `Quick test_nickname;
          Alcotest.test_case "Nickname validate" `Quick test_nickname_validate;
        ] );
    ]

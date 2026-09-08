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

let dec_fails name t s =
  match decode t s with
  | Ok _ -> Alcotest.failf "%s: expected a decode failure" name
  | Error _ -> ()

let roundtrip name t s expected =
  let v = dec_ok name t s in
  let s1 = enc_ok name t v in
  Alcotest.(check string) name expected s1;
  let v2 = dec_ok name t s1 in
  Alcotest.(check string) (name ^ " (stable)") s1 (enc_ok name t v2)

let valid name = function
  | Ok _ -> ()
  | Error msg -> Alcotest.failf "%s: valid value rejected: %s" name msg

let invalid name = function
  | Ok _ -> Alcotest.failf "%s: expected a validation failure" name
  | Error _ -> ()

(* Addresses, Section 2.5 *)

let component_t = Address.Component.jsont
let address_t = Address.jsont

let test_component () =
  roundtrip "component" component_t {|{"kind":"number","value":"54321"}|}
    {|{"value":"54321","kind":"number"}|};
  roundtrip "component with @type" component_t
    {|{"@type":"AddressComponent","kind":"postOfficeBox","value":"PO Box 1"}|}
    {|{"value":"PO Box 1","kind":"postOfficeBox"}|};
  roundtrip "component phonetic" component_t
    {|{"kind":"locality","value":"Tokyo","phonetic":"toukyou"}|}
    {|{"value":"Tokyo","kind":"locality","phonetic":"toukyou"}|};
  roundtrip "a vendor kind decodes" component_t
    {|{"kind":"example.com:quadrant","value":"NW"}|}
    {|{"value":"NW","kind":"example.com:quadrant"}|};
  roundtrip "unknown members are kept" component_t
    {|{"kind":"name","value":"Oak St","example.com:foo":{"bar":true}}|}
    {|{"value":"Oak St","kind":"name","example.com:foo":{"bar":true}}|};
  dec_fails "a component without a kind" component_t {|{"value":"54321"}|};
  dec_fails "a component without a value" component_t {|{"kind":"number"}|};
  dec_fails "a component of another type" component_t
    {|{"@type":"Address","kind":"number","value":"1"}|};
  invalid "a vendor kind that is not a v-extension"
    (Address.Component.validate
       (Address.Component.make (`Vendor "quadrant") "NW"))

(* Figure 31: "54321 Oak St, Reston, CA 20190, USA" *)
let test_address_figure_31 () =
  roundtrip "figure 31" address_t
    {|{"contexts":{"work":true},"components":[{"kind":"number","value":"54321"},{"kind":"separator","value":" "},{"kind":"name","value":"Oak St"},{"kind":"locality","value":"Reston"},{"kind":"region","value":"VA"},{"kind":"separator","value":" "},{"kind":"postcode","value":"20190"},{"kind":"country","value":"USA"}],"countryCode":"US","defaultSeparator":", ","isOrdered":true}|}
    {|{"components":[{"value":"54321","kind":"number"},{"value":" ","kind":"separator"},{"value":"Oak St","kind":"name"},{"value":"Reston","kind":"locality"},{"value":"VA","kind":"region"},{"value":" ","kind":"separator"},{"value":"20190","kind":"postcode"},{"value":"USA","kind":"country"}],"isOrdered":true,"countryCode":"US","contexts":{"work":true},"defaultSeparator":", "}|};
  let a =
    dec_ok "figure 31" address_t
      {|{"components":[{"kind":"number","value":"54321"},{"kind":"separator","value":" "},{"kind":"name","value":"Oak St"}],"countryCode":"US","defaultSeparator":", ","isOrdered":true}|}
  in
  valid "figure 31" (Address.validate a)

(* Figure 32: an address in Thailand, with no separator component. *)
let test_address_figure_32 () =
  let s =
    {|{"components":[{"value":"46","kind":"number"},{"value":"1 Sukhumvit 51 Alley","kind":"name"},{"value":"Khlong Tan Nuea","kind":"subdistrict"},{"value":" Watthana","kind":"district"},{"value":"Bangkok","kind":"locality"},{"value":"Thailand","kind":"country"},{"value":"10110","kind":"postcode"}],"isOrdered":true,"defaultSeparator":", "}|}
  in
  roundtrip "figure 32" address_t
    {|{"components":[{"kind":"number","value":"46"},{"kind":"name","value":"1 Sukhumvit 51 Alley"},{"kind":"subdistrict","value":"Khlong Tan Nuea"},{"kind":"district","value":" Watthana"},{"kind":"locality","value":"Bangkok"},{"kind":"country","value":"Thailand"},{"kind":"postcode","value":"10110"}],"defaultSeparator":", ","isOrdered":true}|}
    s;
  valid "figure 32" (Address.validate (dec_ok "figure 32" address_t s))

(* Figure 33: an address in Tokyo and its Japanese localization. *)
let test_address_figure_33 () =
  let s =
    {|{"components":[{"value":"2-7","kind":"block"},{"value":"-","kind":"separator"},{"value":"2","kind":"number"},{"value":" ","kind":"separator"},{"value":"Marunouchi","kind":"district"},{"value":"Chiyoda-ku","kind":"locality"},{"value":"Tokyo","kind":"region"},{"value":" ","kind":"separator"},{"value":"100-8994","kind":"postcode"}],"isOrdered":true,"full":"2-7-2 Marunouchi, Chiyoda-ku, Tokyo 100-8994","defaultSeparator":", "}|}
  in
  roundtrip "figure 33" address_t
    {|{"components":[{"kind":"block","value":"2-7"},{"kind":"separator","value":"-"},{"kind":"number","value":"2"},{"kind":"separator","value":" "},{"kind":"district","value":"Marunouchi"},{"kind":"locality","value":"Chiyoda-ku"},{"kind":"region","value":"Tokyo"},{"kind":"separator","value":" "},{"kind":"postcode","value":"100-8994"}],"defaultSeparator":", ","full":"2-7-2 Marunouchi, Chiyoda-ku, Tokyo 100-8994","isOrdered":true}|}
    s;
  valid "figure 33" (Address.validate (dec_ok "figure 33" address_t s))

let test_address_codec () =
  roundtrip "a minimal address" address_t {|{"full":"1 Oak St"}|}
    {|{"full":"1 Oak St"}|};
  roundtrip "an address with @type" address_t
    {|{"@type":"Address","timeZone":"Australia/Sydney"}|}
    {|{"timeZone":"Australia/Sydney"}|};
  roundtrip "isOrdered false is not written back" address_t
    {|{"full":"1 Oak St","isOrdered":false}|} {|{"full":"1 Oak St"}|};
  roundtrip "the billing and delivery contexts" address_t
    {|{"full":"1 Oak St","contexts":{"delivery":true,"billing":true}}|}
    {|{"contexts":{"billing":true,"delivery":true},"full":"1 Oak St"}|};
  roundtrip "unknown members are kept" address_t
    {|{"full":"1 Oak St","example.com:foo":{"bar":true}}|}
    {|{"full":"1 Oak St","example.com:foo":{"bar":true}}|};
  dec_fails "a context mapped to false" address_t
    {|{"full":"1 Oak St","contexts":{"work":false}}|};
  dec_fails "isOrdered is a boolean" address_t
    {|{"full":"1 Oak St","isOrdered":"yes"}|};
  dec_fails "a negative pref" address_t {|{"full":"1 Oak St","pref":-1}|}

let test_address_validate () =
  let c = Address.Component.make in
  invalid "an empty address" (Address.validate (Address.make ()));
  invalid "components of separators alone"
    (Address.validate
       (Address.make ~is_ordered:true ~components:[ c `Separator " " ] ()));
  invalid "a separator component when the address is not ordered"
    (Address.validate
       (Address.make ~components:[ c `Name "Oak St"; c `Separator " " ] ()));
  invalid "defaultSeparator when the address is not ordered"
    (Address.validate
       (Address.make
          ~components:[ c `Name "Oak St" ]
          ~default_separator:", " ()));
  invalid "defaultSeparator without components"
    (Address.validate
       (Address.make ~full:"1 Oak St" ~is_ordered:true ~default_separator:", "
          ()));
  valid "defaultSeparator with ordered components"
    (Address.validate
       (Address.make
          ~components:[ c `Name "Oak St" ]
          ~is_ordered:true ~default_separator:", " ()));
  let phonetic = [ c ~phonetic:"toukyou" `Locality "Tokyo" ] in
  invalid "a phonetic without a script or system"
    (Address.validate (Address.make ~components:phonetic ()));
  valid "a phonetic with a system"
    (Address.validate
       (Address.make ~components:phonetic ~phonetic_system:`Piny ()));
  valid "a phonetic with a script"
    (Address.validate
       (Address.make ~components:phonetic ~phonetic_script:"Jpan" ()));
  invalid "a phoneticScript that is not four letters"
    (Address.validate
       (Address.make ~components:phonetic ~phonetic_script:"Jpn" ()));
  invalid "a countryCode of three letters"
    (Address.validate (Address.make ~country_code:"USA" ()));
  valid "a countryCode of two letters"
    (Address.validate (Address.make ~country_code:"US" ()));
  invalid "coordinates that are not a geo: URI"
    (Address.validate (Address.make ~coordinates:"https://example.com/geo" ()));
  valid "a geo: URI"
    (Address.validate (Address.make ~coordinates:"geo:46.772673,-71.282945" ()));
  invalid "a pref outside 1 to 100"
    (Address.validate (Address.make ~full:"1 Oak St" ~pref:0 ()));
  valid "the billing context, which an Address alone may use"
    (Address.validate (Address.make ~full:"1 Oak St" ~contexts:[ `Billing ] ()));
  invalid "an unknown member that is not a valid property name"
    (Address.validate
       (Address.make ~full:"1 Oak St"
          ~unknown:(Unknown.of_list [ ("not a name", Jsont.Json.bool true) ])
          ()))

(* relatedTo, Section 2.1.8 *)

let relation_t = Info.Relation.jsont
let related_to_t = Json.Map.of_string relation_t

(* Figure 13: the relatedTo property. *)
let test_relation () =
  roundtrip "figure 13" related_to_t
    {|{"urn:uuid:f81d4fae-7dec-11d0-a765-00a0c91e6bf6":{"relation":{"friend":true}},"8cacdfb7d1ffdb59@example.com":{"relation":{}}}|}
    {|{"8cacdfb7d1ffdb59@example.com":{},"urn:uuid:f81d4fae-7dec-11d0-a765-00a0c91e6bf6":{"relation":{"friend":true}}}|};
  roundtrip "the hyphenated relation types" relation_t
    {|{"relation":{"co-worker":true,"co-resident":true}}|}
    {|{"relation":{"co-resident":true,"co-worker":true}}|};
  roundtrip "an absent relation is the empty set" relation_t {|{}|} {|{}|};
  roundtrip "unknown members are kept" relation_t
    {|{"relation":{"me":true},"example.com:foo":true}|}
    {|{"relation":{"me":true},"example.com:foo":true}|};
  dec_fails "a relation type mapped to false" relation_t
    {|{"relation":{"friend":false}}|};
  dec_fails "an object of another type" relation_t {|{"@type":"Anniversary"}|};
  let r = dec_ok "figure 13" relation_t {|{"relation":{"friend":true}}|} in
  valid "figure 13" (Info.Relation.validate r);
  invalid "a vendor relation type that is not a v-extension"
    (Info.Relation.validate (Info.Relation.make ~relation:[ `Vendor "pal" ] ()))

(* anniversaries, Section 2.8.1 *)

let anniversary_t = Info.Anniversary.jsont
let anniversaries_t = Json.Map.of_id anniversary_t

(* Figure 41: the anniversaries property. *)
let test_anniversary () =
  let s =
    {|{"k8":{"kind":"birth","date":{"year":1953,"month":4,"day":15}},"k9":{"kind":"death","date":{"@type":"Timestamp","utc":"2019-10-15T23:10:00Z"},"place":{"full":"4445 Tree Street\nNew England, ND 58647\nUSA"}}}|}
  in
  roundtrip "figure 41" anniversaries_t s s;
  roundtrip "unknown members are kept" anniversary_t
    {|{"kind":"wedding","date":{"year":1953},"example.com:foo":[1]}|}
    {|{"kind":"wedding","date":{"year":1953},"example.com:foo":[1]}|};
  dec_fails "an anniversary without a date" anniversary_t {|{"kind":"birth"}|};
  dec_fails "an anniversary without a kind" anniversary_t
    {|{"date":{"year":1953}}|};
  let as_ = dec_ok "figure 41" anniversaries_t s in
  List.iter
    (fun (id, a) -> valid (Id.to_string id) (Info.Anniversary.validate a))
    as_;
  let date = Date.Partial (Date.Partial_date.make ~day:15 ()) in
  invalid "a day without a month"
    (Info.Anniversary.validate (Info.Anniversary.make `Birth date));
  let date = Date.Partial (Date.Partial_date.make ~year:1953 ()) in
  invalid "a place that sets no property"
    (Info.Anniversary.validate
       (Info.Anniversary.make ~place:(Address.make ()) `Birth date));
  invalid "a vendor kind that is not a v-extension"
    (Info.Anniversary.validate
       (Info.Anniversary.make (`Vendor "graduation") date))

(* notes, Section 2.8.3 *)

let note_t = Info.Note.jsont
let notes_t = Json.Map.of_id note_t

(* Figure 43: the notes property. *)
let test_note () =
  let s =
    {|{"n1":{"note":"Open office hours are 1600 to 1715 EST, Mon-Fri","created":"2022-11-23T15:01:32Z","author":{"name":"John"}}}|}
  in
  roundtrip "figure 43" notes_t s s;
  roundtrip "unknown members are kept" note_t
    {|{"note":"hi","author":{"uri":"https://example.com/j","example.com:foo":1}}|}
    {|{"note":"hi","author":{"uri":"https://example.com/j","example.com:foo":1}}|};
  dec_fails "a note without a note property" note_t
    {|{"created":"2022-11-23T15:01:32Z"}|};
  dec_fails "a created that is not a UTCDateTime" note_t
    {|{"note":"hi","created":"2022-11-23"}|};
  let ns = dec_ok "figure 43" notes_t s in
  List.iter (fun (id, n) -> valid (Id.to_string id) (Info.Note.validate n)) ns;
  invalid "an author that sets no property"
    (Info.Note.validate
       (Info.Note.make ~author:(Info.Note.Author.make ()) "hi"));
  valid "an author with a name"
    (Info.Note.Author.validate (Info.Note.Author.make ~name:"John" ()))

(* personalInfo, Section 2.8.4 *)

let personal_info_t = Info.Personal_info.jsont
let personal_infos_t = Json.Map.of_id personal_info_t

(* Figure 44: the personalInfo property. *)
let test_personal_info () =
  roundtrip "figure 44" personal_infos_t
    {|{"pi2":{"kind":"expertise","value":"chemistry","level":"high"},"pi1":{"kind":"hobby","value":"reading","level":"high"},"pi6":{"kind":"interest","value":"r&b music","level":"medium"}}|}
    {|{"pi1":{"kind":"hobby","value":"reading","level":"high"},"pi2":{"kind":"expertise","value":"chemistry","level":"high"},"pi6":{"kind":"interest","value":"r&b music","level":"medium"}}|};
  roundtrip "listAs and label" personal_info_t
    {|{"kind":"hobby","value":"reading","listAs":1,"label":"books"}|}
    {|{"kind":"hobby","value":"reading","listAs":1,"label":"books"}|};
  roundtrip "unknown members are kept" personal_info_t
    {|{"kind":"interest","value":"jazz","example.com:foo":null}|}
    {|{"kind":"interest","value":"jazz","example.com:foo":null}|};
  dec_fails "personal information without a kind" personal_info_t
    {|{"value":"chemistry"}|};
  dec_fails "personal information without a value" personal_info_t
    {|{"kind":"expertise"}|};
  dec_fails "a negative listAs" personal_info_t
    {|{"kind":"hobby","value":"reading","listAs":-1}|};
  valid "figure 44"
    (Info.Personal_info.validate
       (Info.Personal_info.make ~level:`High `Expertise "chemistry"));
  invalid "a listAs of zero"
    (Info.Personal_info.validate
       (Info.Personal_info.make ~list_as:0 `Hobby "reading"));
  invalid "a vendor level that is not a v-extension"
    (Info.Personal_info.validate
       (Info.Personal_info.make ~level:(`Vendor "expert") `Expertise "chemistry"))

(* Section 2.5.1.2: two consecutive separator components are invalid. *)
let test_address_separators () =
  let street = Address.Component.make `Name "Main Street" in
  let number = Address.Component.make `Number "42" in
  let sep = Address.Component.make `Separator " " in
  invalid "two consecutive separators"
    (Address.validate
       (Address.make ~is_ordered:true
          ~components:[ number; sep; sep; street ]
          ()));
  valid "separators apart"
    (Address.validate
       (Address.make ~is_ordered:true ~components:[ number; sep; street ] ()));
  invalid "coordinates with a control character"
    (Address.validate (Address.make ~coordinates:"geo:1,2\x01" ()))

(* A contexts set and a relation set compare as sets. *)
let test_set_equality () =
  let a = Address.make ~full:"x" ~contexts:[ `Work; `Billing ] () in
  let b = Address.make ~full:"x" ~contexts:[ `Billing; `Work ] () in
  Alcotest.(check bool) "contexts order" true (Address.equal a b);
  let r = Info.Relation.make ~relation:[ `Friend; `Colleague ] () in
  let r' = Info.Relation.make ~relation:[ `Colleague; `Friend ] () in
  Alcotest.(check bool) "relation order" true (Info.Relation.equal r r')

let () =
  Alcotest.run "jscontact"
    [
      ( "address",
        [
          Alcotest.test_case "AddressComponent" `Quick test_component;
          Alcotest.test_case "Address codec" `Quick test_address_codec;
          Alcotest.test_case "Address figure 31" `Quick test_address_figure_31;
          Alcotest.test_case "Address figure 32" `Quick test_address_figure_32;
          Alcotest.test_case "Address figure 33" `Quick test_address_figure_33;
          Alcotest.test_case "Address validate" `Quick test_address_validate;
          Alcotest.test_case "separators" `Quick test_address_separators;
          Alcotest.test_case "set equality" `Quick test_set_equality;
        ] );
      ( "info",
        [
          Alcotest.test_case "Relation" `Quick test_relation;
          Alcotest.test_case "Anniversary" `Quick test_anniversary;
          Alcotest.test_case "Note" `Quick test_note;
          Alcotest.test_case "PersonalInfo" `Quick test_personal_info;
        ] );
    ]

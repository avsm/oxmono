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

(* Section 2.3.1 *)

let test_emails () =
  let t = Json.Map.of_id Contact.Email_address.jsont in
  (* Figure 25 *)
  roundtrip "emails" t
    {|{"e1":{"contexts":{"work":true},"address":"jqpublic@xyz.example.com"},"e2":{"address":"jane_doe@example.com","pref":1}}|}
    {|{"e1":{"address":"jqpublic@xyz.example.com","contexts":{"work":true}},"e2":{"address":"jane_doe@example.com","pref":1}}|};
  let t = Contact.Email_address.jsont in
  roundtrip "email with @type and label" t
    {|{"@type":"EmailAddress","address":"jdoe@example.com","label":"home"}|}
    {|{"address":"jdoe@example.com","label":"home"}|};
  roundtrip "email keeps unknown members" t
    {|{"address":"jdoe@example.com","example.com:foo":{"bar":true}}|}
    {|{"address":"jdoe@example.com","example.com:foo":{"bar":true}}|};
  dec_fails "email without address" t {|{"pref":1}|};
  dec_fails "email with a wrong @type" t
    {|{"@type":"Phone","address":"jdoe@example.com"}|}

let test_email_validate () =
  let open Contact.Email_address in
  valid_ok "email"
    (validate (make ~contexts:[ `Work ] ~pref:1 "jdoe@example.com"));
  valid_fails "no at" (validate (make "jdoe.example.com"));
  valid_fails "empty local part" (validate (make "@example.com"));
  valid_fails "space in address" (validate (make "j doe@example.com"));
  valid_fails "pref out of range" (validate (make ~pref:200 "jdoe@example.com"));
  valid_fails "billing context"
    (validate (make ~contexts:[ `Billing ] "jdoe@example.com"));
  valid_fails "bad unknown member name"
    (validate
       (make
          ~unknown:(Unknown.of_list [ ("not a name", Jsont.Json.bool true) ])
          "jdoe@example.com"))

(* Section 2.3.2 *)

let test_online_services () =
  let t = Json.Map.of_id Contact.Online_service.jsont in
  (* Figure 26 *)
  roundtrip "onlineServices" t
    {|{"x1":{"uri":"xmpp:alice@example.com"},"x2":{"service":"Mastodon","user":"@alice@example2.com","uri":"https://example2.com/@alice"}}|}
    {|{"x1":{"uri":"xmpp:alice@example.com"},"x2":{"service":"Mastodon","uri":"https://example2.com/@alice","user":"@alice@example2.com"}}|};
  let t = Contact.Online_service.jsont in
  roundtrip "online service keeps unknown members" t
    {|{"user":"alice","example.com:foo":"bar"}|}
    {|{"user":"alice","example.com:foo":"bar"}|};
  (* Section 2.3.2 states no mandatory member, so an empty object decodes and
     validate is what rejects it. *)
  roundtrip "an empty online service decodes" t {|{}|} {|{}|};
  dec_fails "service is not a number" t {|{"service":1,"user":"alice"}|}

let test_online_service_validate () =
  let open Contact.Online_service in
  valid_ok "uri only" (validate (make ~uri:"xmpp:alice@example.com" ()));
  valid_ok "user only" (validate (make ~service:"Mastodon" ~user:"@alice" ()));
  valid_fails "neither uri nor user" (validate (make ~service:"GitHub" ()));
  valid_fails "uri without a scheme"
    (validate (make ~uri:"example.com/alice" ()));
  valid_fails "pref out of range" (validate (make ~user:"alice" ~pref:0 ()));
  valid_fails "delivery context"
    (validate (make ~user:"alice" ~contexts:[ `Delivery ] ()))

(* Section 2.3.3 *)

let test_phones () =
  let t = Json.Map.of_id Contact.Phone.jsont in
  (* Figure 27 *)
  roundtrip "phones" t
    {|{"tel0":{"contexts":{"private":true},"features":{"voice":true},"number":"tel:+1-555-555-5555;ext=5555","pref":1},"tel3":{"contexts":{"work":true},"number":"tel:+1-201-555-0123"}}|}
    {|{"tel0":{"number":"tel:+1-555-555-5555;ext=5555","features":{"voice":true},"contexts":{"private":true},"pref":1},"tel3":{"number":"tel:+1-201-555-0123","contexts":{"work":true}}}|};
  let t = Contact.Phone.jsont in
  roundtrip "the main-number feature" t
    {|{"number":"tel:+1-555-555-0000","features":{"voice":true,"main-number":true}}|}
    {|{"number":"tel:+1-555-555-0000","features":{"main-number":true,"voice":true}}|};
  roundtrip "phone keeps unknown members" t
    {|{"number":"+1 555 555 0000","example.com:foo":[1,2]}|}
    {|{"number":"+1 555 555 0000","example.com:foo":[1,2]}|};
  dec_fails "phone without a number" t {|{"features":{"voice":true}}|};
  dec_fails "a feature mapped to false" t
    {|{"number":"tel:+1-555-555-0000","features":{"voice":false}}|}

let test_phone_features () =
  let open Contact.Phone.Feature in
  Alcotest.(check string) "main-number" "main-number" (to_string `Main_number);
  Alcotest.(check bool) "of_string" true (equal (of_string "pager") `Pager);
  Alcotest.(check bool) "vendor" true (is_vendor (of_string "example.com:foo"));
  valid_ok "vendor feature" (validate (of_string "example.com:foo"));
  valid_fails "unregistered feature" (validate (of_string "bogus"))

let test_phone_validate () =
  let open Contact.Phone in
  valid_ok "phone"
    (validate
       (make ~features:[ `Voice; `Main_number ] ~contexts:[ `Work ] ~pref:1
          "tel:+1-201-555-0123"));
  valid_fails "unregistered feature"
    (validate (make ~features:[ `Vendor "bogus" ] "tel:+1-201-555-0123"));
  valid_fails "billing context"
    (validate (make ~contexts:[ `Billing ] "tel:+1-201-555-0123"));
  valid_fails "pref out of range"
    (validate (make ~pref:101 "tel:+1-201-555-0123"))

(* Section 2.3.4 *)

let test_preferred_languages () =
  let t = Json.Map.of_id Contact.Language_pref.jsont in
  (* Figure 28 *)
  roundtrip "preferredLanguages" t
    {|{"l1":{"language":"en","contexts":{"work":true},"pref":1},"l2":{"language":"fr","contexts":{"work":true},"pref":2},"l3":{"language":"fr","contexts":{"private":true}}}|}
    {|{"l1":{"language":"en","contexts":{"work":true},"pref":1},"l2":{"language":"fr","contexts":{"work":true},"pref":2},"l3":{"language":"fr","contexts":{"private":true}}}|};
  let t = Contact.Language_pref.jsont in
  roundtrip "language preference keeps unknown members" t
    {|{"language":"de-CH","example.com:foo":null}|}
    {|{"language":"de-CH","example.com:foo":null}|};
  dec_fails "language preference without a language" t {|{"pref":1}|}

let test_language_pref_validate () =
  let open Contact.Language_pref in
  valid_ok "language preference"
    (validate (make ~contexts:[ `Private ] ~pref:1 "zh-Hant-TW"));
  valid_fails "underscore in the tag" (validate (make "en_US"));
  valid_fails "an over long subtag" (validate (make "abcdefghi"));
  valid_fails "delivery context" (validate (make ~contexts:[ `Delivery ] "en"));
  valid_fails "pref out of range" (validate (make ~pref:0 "en"))

let () =
  Alcotest.run "jscontact"
    [
      ( "contact",
        [
          Alcotest.test_case "emails" `Quick test_emails;
          Alcotest.test_case "EmailAddress validate" `Quick test_email_validate;
          Alcotest.test_case "onlineServices" `Quick test_online_services;
          Alcotest.test_case "OnlineService validate" `Quick
            test_online_service_validate;
          Alcotest.test_case "phones" `Quick test_phones;
          Alcotest.test_case "Phone features" `Quick test_phone_features;
          Alcotest.test_case "Phone validate" `Quick test_phone_validate;
          Alcotest.test_case "preferredLanguages" `Quick
            test_preferred_languages;
          Alcotest.test_case "LanguagePref validate" `Quick
            test_language_pref_validate;
        ] );
    ]

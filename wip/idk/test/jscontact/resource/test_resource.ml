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
  | Ok _ -> Alcotest.failf "%s: this should not decode: %s" name s
  | Error _ -> ()

let roundtrip name t s expected =
  let v = dec_ok name t s in
  let s1 = enc_ok name t v in
  Alcotest.(check string) name expected s1;
  let v2 = dec_ok name t s1 in
  Alcotest.(check string) (name ^ " (stable)") s1 (enc_ok name t v2)

(* RFC 9553 Section 1.4.4 makes uri mandatory, but RFC 9610 Section 3 has a
   JMAP server omit it on a Media that carries a blobId instead, so the codec
   accepts its absence and validate is what rejects it. *)
let decodes_but_invalid name t s validate =
  match Jsont_bytesrw.decode_string' t s with
  | Error e ->
      Alcotest.failf "%s: should decode: %s" name (Jsont.Error.to_string e)
  | Ok v -> (
      match validate v with
      | Ok _ -> Alcotest.failf "%s: should not validate" name
      | Error _ -> ())

let invalid name = function
  | Ok _ -> Alcotest.failf "%s: this should not validate" name
  | Error _ -> ()

let valid name = function
  | Ok _ -> ()
  | Error msg -> Alcotest.failf "%s: valid value rejected: %s" name msg

(* Section 2.6.1 *)

let crypto_keys = Json.Map.of_id Resource.Crypto_key.jsont

let test_crypto_keys () =
  (* Figure 34 *)
  roundtrip "cryptoKeys with external data" crypto_keys
    {|{"mykey1":{"uri":"https://www.example.com/keys/jdoe.cer"}}|}
    {|{"mykey1":{"uri":"https://www.example.com/keys/jdoe.cer"}}|};
  (* Figure 35, whose uri is depicted over several lines in the RFC *)
  let embedded =
    {|{"mykey2":{"uri":"data:application/pgp-keys;base64,LS0tLS1CRUdJTiBSU0EgUFVCTElDIEtFWS0tLS0tCk1JSUJDZ0tDQVFFQSt4R1ovd2N6OXVnRnBQMDdOc3BvNlUxN2wwWWhGaUZweHhVNHBUazNMaWZ6OVIzenNJc3UKRVJ3dGE3K2ZXSWZ4T28yMDhldHQvamhza2lWb2RTRXQzUUJHaDRYQmlweVdvcEt3WjkzSEhhRFZaQUFMaS8yQQoreFRCdFdkRW83WEdVdWpLRHZDMi9hWkt1a2ZqcE9pVUk4QWhMQWZqbWxjRC9VWjFRUGgwbUhzZ2xSTkNtcEN3Cm13U1hBOVZObWh6K1BpQitEbWw0V1duS1cvVkhvMnVqVFh4cTcrZWZNVTRIMmZueTNTZTNLWU9zRlBGR1oxVE4KUVNZbEZ1U2hXckhQdGlMbVVkUG9QNkNWMm1NTDF0aytsN0RJSXFYclFoTFVLREFDZU01cm9NeDBrTGhVV0I4UAorMHVqMUNObE5ONEpSWmxDN3hGZnFpTWJGUlU5WjRONll3SURBUUFCCi0tLS0tRU5EIFJTQSBQVUJMSUMgS0VZLS0tLS0K"}}|}
  in
  roundtrip "cryptoKeys with embedded data" crypto_keys embedded embedded;
  let t = Resource.Crypto_key.jsont in
  roundtrip "an unknown member is kept" t
    {|{"uri":"https://example.com/k","example.com:foo":{"bar":true}}|}
    {|{"uri":"https://example.com/k","example.com:foo":{"bar":true}}|};
  roundtrip "@type is accepted and implied" t
    {|{"@type":"CryptoKey","uri":"https://example.com/k","kind":"pgp"}|}
    {|{"kind":"pgp","uri":"https://example.com/k"}|};
  decodes_but_invalid "no uri" t {|{"kind":"pgp"}|} Resource.Crypto_key.validate;
  dec_fails "another @type" t {|{"@type":"Link","uri":"https://example.com/k"}|};
  valid "a plain key"
    (Resource.Crypto_key.validate
       (Resource.Crypto_key.make ~uri:"https://example.com/k" ()));
  invalid "an Address context"
    (Resource.Crypto_key.validate
       (Resource.Crypto_key.make ~contexts:[ `Billing ]
          ~uri:"https://example.com/k" ()));
  invalid "an unknown member that no name may spell"
    (Resource.Crypto_key.validate
       (dec_ok "bad member" t {|{"uri":"https://example.com/k","a/b":1}|}))

(* Section 2.6.2 *)

let directories = Json.Map.of_id Resource.Directory.jsont

let test_directories () =
  (* Figure 36, whose final closing brace is missing in the RFC *)
  let fig36 =
    {|{"dir1":{"kind":"entry",|}
    ^ {|"uri":"https://dir.example.com/addrbook/jdoe/Jean%20Dupont.vcf"},|}
    ^ {|"dir2":{"kind":"directory",|}
    ^ {|"uri":"ldap://ldap.example/o=Example%20Tech,ou=Engineering",|}
    ^ {|"pref":1}}|}
  in
  roundtrip "directories" directories fig36 fig36;
  let t = Resource.Directory.jsont in
  roundtrip "listAs follows label" t
    {|{"kind":"entry","uri":"https://example.com/d","label":"work","listAs":2}|}
    {|{"kind":"entry","uri":"https://example.com/d","label":"work","listAs":2}|};
  roundtrip "an unknown member is kept" t
    {|{"kind":"entry","uri":"https://example.com/d","example.com:x":[1]}|}
    {|{"kind":"entry","uri":"https://example.com/d","example.com:x":[1]}|};
  dec_fails "no kind" t {|{"uri":"https://example.com/d"}|};
  decodes_but_invalid "no uri" t {|{"kind":"entry"}|}
    Resource.Directory.validate;
  valid "a listed directory"
    (Resource.Directory.validate
       (Resource.Directory.make ~list_as:1 ~uri:"https://example.com/d" `Entry));
  invalid "a listAs of zero"
    (Resource.Directory.validate
       (Resource.Directory.make ~list_as:0 ~uri:"https://example.com/d" `Entry));
  invalid "a pref outside 1 to 100"
    (Resource.Directory.validate
       (Resource.Directory.make ~pref:101 ~uri:"https://example.com/d"
          `Directory));
  invalid "a vendor kind that is not a v-extension"
    (Resource.Directory.validate
       (Resource.Directory.make ~uri:"https://example.com/d" (`Vendor "bogus")))

(* Section 2.6.3 *)

let links = Json.Map.of_id Resource.Link.jsont

let test_links () =
  (* Figure 37 *)
  let fig37 =
    {|{"link3":{"kind":"contact","uri":"mailto:contact@example.com","pref":1}}|}
  in
  roundtrip "links" links fig37 fig37;
  let t = Resource.Link.jsont in
  roundtrip "the kind is optional" t
    {|{"uri":"https://example.com/l","mediaType":"text/html"}|}
    {|{"uri":"https://example.com/l","mediaType":"text/html"}|};
  roundtrip "an unknown member is kept" t
    {|{"uri":"https://example.com/l","example.com:x":"y"}|}
    {|{"uri":"https://example.com/l","example.com:x":"y"}|};
  roundtrip "contexts are a set" t
    {|{"uri":"https://example.com/l","contexts":{"work":true,"private":true}}|}
    {|{"uri":"https://example.com/l","contexts":{"private":true,"work":true}}|};
  decodes_but_invalid "no uri" t {|{"kind":"contact"}|} Resource.Link.validate;
  dec_fails "a context mapped to false" t
    {|{"uri":"https://example.com/l","contexts":{"work":false}}|};
  valid "a preferred link"
    (Resource.Link.validate
       (Resource.Link.make ~kind:`Contact ~pref:1 ~uri:"mailto:a@example.com" ()));
  invalid "a pref of zero"
    (Resource.Link.validate
       (Resource.Link.make ~pref:0 ~uri:"mailto:a@example.com" ()));
  invalid "an Address context"
    (Resource.Link.validate
       (Resource.Link.make ~contexts:[ `Delivery ] ~uri:"mailto:a@example.com"
          ()))

(* Section 2.6.4 *)

let media = Json.Map.of_id Resource.Media.jsont

let test_media () =
  (* Figure 38 *)
  let fig38 =
    {|{"res45":{"kind":"sound",|}
    ^ {|"uri":"CID:JOHNQ.part8.19960229T080000.xyzMail@example.com"},|}
    ^ {|"res47":{"kind":"logo",|}
    ^ {|"uri":"https://www.example.com/pub/logos/abccorp.jpg"},|}
    ^ {|"res1":{"kind":"photo",|}
    ^ {|"uri":"data:image/jpeg;base64,/9j/4AAQSkZJRgABAQAASABIAAD/4..."}}|}
  in
  (* Decoding orders the map by key, so res1 comes first on the way out. *)
  let sorted =
    {|{"res1":{"kind":"photo",|}
    ^ {|"uri":"data:image/jpeg;base64,/9j/4AAQSkZJRgABAQAASABIAAD/4..."},|}
    ^ {|"res45":{"kind":"sound",|}
    ^ {|"uri":"CID:JOHNQ.part8.19960229T080000.xyzMail@example.com"},|}
    ^ {|"res47":{"kind":"logo",|}
    ^ {|"uri":"https://www.example.com/pub/logos/abccorp.jpg"}}|}
  in
  roundtrip "media" media fig38 sorted;
  let t = Resource.Media.jsont in
  roundtrip "an unknown member is kept" t
    {|{"kind":"photo","uri":"https://example.com/m","example.com:x":null}|}
    {|{"kind":"photo","uri":"https://example.com/m","example.com:x":null}|};
  dec_fails "no kind" t {|{"uri":"https://example.com/m"}|};
  decodes_but_invalid "no uri" t {|{"kind":"photo"}|} Resource.Media.validate;
  (* RFC 9610 Section 3: a JMAP server returns a blobId and omits the uri on a
     Media whose value would be a data: URI. Section 7.5 reserves blobId in a
     Media, so such a Media decodes and keeps the member but does not validate
     as a JSContact resource. *)
  let blob = {|{"kind":"photo","blobId":"G123","mediaType":"image/jpeg"}|} in
  roundtrip "a JMAP Media carries a blobId instead of a uri" t blob
    {|{"kind":"photo","mediaType":"image/jpeg","blobId":"G123"}|};
  decodes_but_invalid "a blobId is reserved in a Media" t blob
    Resource.Media.validate;
  Alcotest.(check string)
    "a vendor kind" "example.com:thumb"
    (Resource.Media.Kind.to_string
       (Resource.Media.Kind.of_string "example.com:thumb"));
  valid "a vendor kind"
    (Resource.Media.validate
       (Resource.Media.make ~uri:"https://example.com/m"
          (`Vendor "example.com:thumb")));
  invalid "a vendor kind that is not a v-extension"
    (Resource.Media.validate
       (Resource.Media.make ~uri:"https://example.com/m" (`Vendor "bogus")));
  (* An encoder reports a bad value rather than raising Invalid_argument. *)
  match
    encode t
      (Resource.Media.make ~uri:"https://example.com/m" (`Vendor "bogus"))
  with
  | Ok s -> Alcotest.failf "a bad vendor kind should not encode: %s" s
  | Error _ -> ()

(* Section 2.4.1 *)

let calendars = Json.Map.of_id Calendar.jsont

let test_calendars () =
  (* Figure 29 *)
  let fig29 =
    {|{"calA":{"kind":"calendar",|}
    ^ {|"uri":"webcal://calendar.example.com/calA.ics"},|}
    ^ {|"project-a":{"kind":"freeBusy",|}
    ^ {|"uri":"https://calendar.example.com/busy/project-a"}}|}
  in
  roundtrip "calendars" calendars fig29 fig29;
  let t = Calendar.jsont in
  roundtrip "an unknown member is kept" t
    {|{"kind":"calendar","uri":"webcal://example.com/c","example.com:x":1}|}
    {|{"kind":"calendar","uri":"webcal://example.com/c","example.com:x":1}|};
  dec_fails "no kind" t {|{"uri":"webcal://example.com/c"}|};
  decodes_but_invalid "no uri" t {|{"kind":"calendar"}|} Calendar.validate;
  Alcotest.(check bool)
    "freeBusy" true
    (Calendar.Kind.equal (Calendar.Kind.of_string "freeBusy") `Free_busy);
  Alcotest.(check string)
    "freeBusy spelling" "freeBusy"
    (Calendar.Kind.to_string `Free_busy);
  valid "a free-busy calendar"
    (Calendar.validate
       (Calendar.make ~pref:1 ~uri:"https://example.com/busy" `Free_busy));
  invalid "a pref outside 1 to 100"
    (Calendar.validate
       (Calendar.make ~pref:0 ~uri:"webcal://example.com/c" `Calendar));
  invalid "an Address context"
    (Calendar.validate
       (Calendar.make ~contexts:[ `Billing ] ~uri:"webcal://example.com/c"
          `Calendar));
  invalid "a vendor kind that is not a v-extension"
    (Calendar.validate
       (Calendar.make ~uri:"webcal://example.com/c" (`Vendor "bogus")))

(* Section 2.4.2 *)

let scheduling_addresses = Json.Map.of_id Calendar.Scheduling_address.jsont

let test_scheduling_addresses () =
  (* Figure 30 *)
  let fig30 = {|{"sched1":{"uri":"mailto:janedoe@example.com"}}|} in
  roundtrip "schedulingAddresses" scheduling_addresses fig30 fig30;
  let t = Calendar.Scheduling_address.jsont in
  roundtrip "an unknown member is kept" t
    {|{"uri":"mailto:a@example.com","example.com:x":{}}|}
    {|{"uri":"mailto:a@example.com","example.com:x":{}}|};
  roundtrip "@type is accepted and implied" t
    {|{"@type":"SchedulingAddress","uri":"mailto:a@example.com","pref":2}|}
    {|{"uri":"mailto:a@example.com","pref":2}|};
  dec_fails "no uri" t {|{"pref":1}|};
  (* Section 2.4.2 gives a SchedulingAddress no mediaType, so one is an
     unknown member that Section 1.7.4 requires to be preserved. *)
  roundtrip "no mediaType property" t
    {|{"uri":"mailto:a@example.com","mediaType":"text/plain"}|}
    {|{"uri":"mailto:a@example.com","mediaType":"text/plain"}|};
  valid "a labelled address"
    (Calendar.Scheduling_address.validate
       (Calendar.Scheduling_address.make ~label:"work" ~pref:100
          "mailto:a@example.com"));
  invalid "a pref outside 1 to 100"
    (Calendar.Scheduling_address.validate
       (Calendar.Scheduling_address.make ~pref:101 "mailto:a@example.com"));
  invalid "an Address context"
    (Calendar.Scheduling_address.validate
       (Calendar.Scheduling_address.make ~contexts:[ `Billing ]
          "mailto:a@example.com"))

let () =
  Alcotest.run "jscontact"
    [
      ( "resource",
        [
          Alcotest.test_case "cryptoKeys" `Quick test_crypto_keys;
          Alcotest.test_case "directories" `Quick test_directories;
          Alcotest.test_case "links" `Quick test_links;
          Alcotest.test_case "media" `Quick test_media;
          Alcotest.test_case "calendars" `Quick test_calendars;
          Alcotest.test_case "schedulingAddresses" `Quick
            test_scheduling_addresses;
        ] );
    ]

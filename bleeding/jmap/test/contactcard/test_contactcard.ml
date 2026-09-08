(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** RFC 9610 Section 3 ContactCard codec tests, focused on the two JMAP
    properties a JSContact Card does not define and on the partial objects a
    [ContactCard/get] with a [properties] argument returns. The JSON here is
    what a Cyrus oracle answered. *)

open Jmap.Proto

(* [Jmap.Proto.Contact_card] is the canonical path, but lib/core/jmap.mli does
   not yet list the contacts aliases its lib/core/jmap.ml defines, so the
   library module is named directly here. Delete this line once it does. *)

let decode jsont json_str = Jsont_bytesrw.decode_string' jsont json_str
let encode jsont value = Jsont_bytesrw.encode_string' jsont value

let decode_string name jsont json_str =
  match decode jsont json_str with
  | Ok v -> v
  | Error e ->
      Alcotest.failf "%s: decode failed: %s" name (Jsont.Error.to_string e)

let encode_string name jsont v =
  match encode jsont v with
  | Ok s -> s
  | Error e ->
      Alcotest.failf "%s: encode failed: %s" name (Jsont.Error.to_string e)

(* Round trip through a decode / encode / decode cycle and check that the
   re-encoded JSON is byte-identical to the first encoding. *)
let roundtrip name jsont json_str =
  let v = decode_string name jsont json_str in
  let s1 = encode_string name jsont v in
  let v' = decode_string (name ^ " re-decode") jsont s1 in
  let s2 = encode_string (name ^ " re-encode") jsont v' in
  Alcotest.(check string) (name ^ ": stable re-encode") s1 s2;
  (s1, v')

let contains ~needle s =
  let n = String.length needle and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = needle || go (i + 1)) in
  n = 0 || go 0

let check_member name member json =
  Alcotest.(check bool)
    (Printf.sprintf "%s: %S present" name member)
    true
    (contains ~needle:(Printf.sprintf "%S" member) json)

let check_omitted name member json =
  Alcotest.(check bool)
    (Printf.sprintf "%s: %S omitted" name member)
    false
    (contains ~needle:(Printf.sprintf "%S" member) json)

let some name = function
  | Some v -> v
  | None -> Alcotest.failf "%s: expected Some" name

let uid = "urn:uuid:3f1a0d2e-0000-4000-8000-000000000001"

(* RFC 8620 Section 1.2 draws an Id from [A-Za-z0-9_-], so a ContactCard id is
   not the urn a uid is, even where a server happens to spell the two alike. *)
let id = "K71"

(* ContactCard (RFC 9610 Section 3) *)
module Card_tests = struct
  (* A whole card, as a [ContactCard/get] with no [properties] returns it. *)
  let whole =
    {|{"@type":"Card","version":"1.0","kind":"individual",
       "created":"2026-09-07T16:09:26Z",
       "organizations":{"o1":{"name":"Analytical Engines"}},
       "emails":{"e1":{"address":"ada@example.com"}},
       "notes":{"n1":{"note":"a note"}},
       "prodId":"-//CyrusIMAP.org//Cyrus//EN",
       "uid":"urn:uuid:3f1a0d2e-0000-4000-8000-000000000001",
       "name":{"components":[{"kind":"surname","value":"Lovelace"},
                             {"kind":"given","value":"Ada"}]},
       "id":"K71",
       "addressBookIds":{"Default":true}}|}

  (* The same card fetched with properties:["name"]. The server returns the
     asked-for property plus [id] and [addressBookIds], and none of the three
     RFC 9553 Section 2.1 makes mandatory. *)
  let partial =
    {|{"name":{"components":[{"kind":"surname","value":"Lovelace"},
                             {"kind":"given","value":"Ada"}]},
       "id":"K71",
       "addressBookIds":{"Default":true}}|}

  (* A card as sent in a [ContactCard/set] create: no [id], the server
     assigning one. *)
  let create =
    {|{"@type":"Card","version":"1.0",
       "uid":"urn:uuid:3f1a0d2e-0000-4000-8000-000000000001",
       "addressBookIds":{"Default":true},
       "kind":"individual",
       "name":{"components":[{"kind":"given","value":"Ada"}]}}|}

  let check_lifted name (c : Contact_card.t) =
    Alcotest.(check string)
      (name ^ ": id") id
      (Id.to_string (some (name ^ ": id") c.Contact_card.id));
    let books =
      some (name ^ ": addressBookIds") c.Contact_card.address_book_ids
    in
    Alcotest.(check (list (pair string bool)))
      (name ^ ": addressBookIds")
      [ ("Default", true) ]
      (List.map (fun (id, b) -> (Id.to_string id, b)) books);
    (* RFC 9553 Section 1.7.4 has the Card codec keep the members it does not
       define; the two JMAP properties must be lifted out of them rather than
       left there to be encoded twice. *)
    let unknown = c.Contact_card.card.Jscontact.Card.unknown in
    Alcotest.(check bool)
      (name ^ ": id not in card.unknown")
      true
      (Jscontact.Unknown.find unknown "id" = None);
    Alcotest.(check bool)
      (name ^ ": addressBookIds not in card.unknown")
      true
      (Jscontact.Unknown.find unknown "addressBookIds" = None)

  let test_whole () =
    let json, c = roundtrip "whole card" Contact_card.jsont whole in
    check_lifted "whole card" c;
    Alcotest.(check string) "uid" uid c.Contact_card.card.Jscontact.Card.uid;
    Alcotest.(check bool)
      "kind is individual" true
      (c.Contact_card.card.Jscontact.Card.kind = `Individual);
    (* Both JMAP properties go back on the wire, and exactly once. *)
    check_member "whole card" "id" json;
    check_member "whole card" "addressBookIds" json;
    check_member "whole card" "@type" json;
    check_member "whole card" "uid" json

  let test_partial () =
    let json, c = roundtrip "partial card" Contact_card.jsont partial in
    check_lifted "partial card" c;
    let card = c.Contact_card.card in
    (* RFC 8620 Section 5.1 lets the client name the properties it wants, so
       none of @type, version and uid need be present. *)
    Alcotest.(check string) "uid is empty" "" card.Jscontact.Card.uid;
    Alcotest.(check string)
      "version defaults" Jscontact.Card.version_1_0 card.Jscontact.Card.version;
    Alcotest.(check bool) "name decoded" true (card.Jscontact.Card.name <> None);
    (* Encoding always writes @type and version, and never an empty uid. *)
    check_member "partial card" "@type" json;
    check_member "partial card" "version" json;
    check_omitted "partial card" "uid" json

  let test_create () =
    let json, c = roundtrip "create card" Contact_card.jsont create in
    Alcotest.(check bool) "no id" true (c.Contact_card.id = None);
    Alcotest.(check bool)
      "addressBookIds set" true
      (c.Contact_card.address_book_ids <> None);
    check_omitted "create card" "id" json;
    check_member "create card" "addressBookIds" json

  let test_make () =
    let card = Jscontact.Card.make uid in
    let c = Contact_card.make card in
    Alcotest.(check bool) "make: no id" true (c.Contact_card.id = None);
    Alcotest.(check bool)
      "make: no addressBookIds" true
      (c.Contact_card.address_book_ids = None);
    let json = encode_string "make" Contact_card.jsont c in
    check_omitted "make" "id" json;
    check_omitted "make" "addressBookIds" json;
    let books = [ (Id.of_string_exn "Default", true) ] in
    let c = Contact_card.make ~address_book_ids:books card in
    let json = encode_string "make books" Contact_card.jsont c in
    check_member "make books" "addressBookIds" json

  (* The Cyrus oracle answers a ContactCard/get with the uid as the id, so the
     id it sends is a urn and holds octets outside the RFC 8620 Section 1.2 Id
     alphabet. Proto_id decodes what a server sent rather than refusing a
     record it can otherwise read, so such a card decodes and keeps the id
     verbatim; Proto_id.of_string is where the alphabet is enforced. *)
  let test_urn_id_decodes () =
    let json =
      {|{"@type":"Card","version":"1.0","uid":"x","id":"urn:uuid:1",
         "addressBookIds":{"Default":true}}|}
    in
    match decode Contact_card.jsont json with
    | Error e ->
        Alcotest.failf "a urn id should decode: %s" (Jsont.Error.to_string e)
    | Ok c -> (
        Alcotest.(check (option string))
          "id kept verbatim" (Some "urn:uuid:1")
          (Option.map Id.to_string c.Contact_card.id);
        match Id.of_string "urn:uuid:1" with
        | Ok _ -> Alcotest.fail "of_string should enforce the alphabet"
        | Error _ -> ())

  (* RFC 9610 Section 3 adds a [blobId] to the Media objects of a card. It is
     no member of RFC 9553 Section 2.6.4, so it survives in the unknown members
     the Media codec keeps rather than in a field of its own. *)
  let test_media_blob_id () =
    let json =
      {|{"@type":"Card","version":"1.0","uid":"x","id":"K71",
         "addressBookIds":{"Default":true},
         "media":{"m1":{"@type":"Media","kind":"photo","uri":"data:,x",
                        "blobId":"B1"}}}|}
    in
    let out, c = roundtrip "media blobId" Contact_card.jsont json in
    let media = some "media" c.Contact_card.card.Jscontact.Card.media in
    let _, m = List.hd media in
    Alcotest.(check bool)
      "blobId kept" true
      (Jscontact.Unknown.find m.Jscontact.Resource.unknown "blobId" <> None);
    check_member "media blobId" "blobId" out

  let test_creation () =
    let c = Contact_card.creation "c1" in
    Alcotest.(check string) "creation id" "c1" (Id.to_string (Id.creation_id c));
    Alcotest.(check string)
      "creation ref" "#c1"
      (Id.to_string (Id.creation_ref c))

  let tests =
    [
      ("whole card round trip", `Quick, test_whole);
      ("partial card round trip", `Quick, test_partial);
      ("create-shaped card round trip", `Quick, test_create);
      ("make", `Quick, test_make);
      ("Media blobId", `Quick, test_media_blob_id);
      ("a urn id decodes", `Quick, test_urn_id_decodes);
      ("creation id", `Quick, test_creation);
    ]
end

(* FilterCondition and sorting (RFC 9610 Sections 3.3.1 and 3.3.2) *)
module Query_tests = struct
  let date s =
    match Date.of_utc_string s with
    | Ok t -> t
    | Error e -> Alcotest.failf "date %S: %s" s e

  let full =
    {|{"inAddressBook":"Default",
       "uid":"urn:uuid:3f1a0d2e-0000-4000-8000-000000000001",
       "hasMember":"urn:uuid:0000",
       "kind":"individual",
       "createdBefore":"2100-01-01T00:00:00Z",
       "createdAfter":"2000-01-01T00:00:00Z",
       "updatedBefore":"2100-01-01T00:00:00Z",
       "updatedAfter":"2000-01-01T00:00:00Z",
       "text":"Ada","name":"Ada",
       "name/given":"Ada","name/surname":"Lovelace","name/surname2":"King",
       "nickname":"Countess","organization":"Analytical Engines",
       "email":"ada@example.com","phone":"+1","onlineService":"@ada",
       "address":"London","note":"a note"}|}

  let test_empty () =
    let json =
      encode_string "empty" Contact_card.Filter_condition.jsont
        Contact_card.Filter_condition.empty
    in
    Alcotest.(check string) "empty condition" "{}" json;
    let json =
      encode_string "filter ()" Contact_card.filter_jsont
        (Contact_card.filter ())
    in
    Alcotest.(check string) "filter ()" "{}" json

  let test_full () =
    let json, f =
      roundtrip "full condition" Contact_card.Filter_condition.jsont full
    in
    let open Contact_card.Filter_condition in
    Alcotest.(check string)
      "inAddressBook" "Default"
      (Id.to_string (some "inAddressBook" f.in_address_book));
    Alcotest.(check string) "uid" uid (some "uid" f.uid);
    Alcotest.(check string)
      "hasMember" "urn:uuid:0000"
      (some "hasMember" f.has_member);
    Alcotest.(check string) "kind" "individual" (some "kind" f.kind);
    Alcotest.(check bool)
      "createdBefore" true
      (Ptime.equal
         (date "2100-01-01T00:00:00Z")
         (some "createdBefore" f.created_before));
    Alcotest.(check bool)
      "createdAfter" true
      (Ptime.equal
         (date "2000-01-01T00:00:00Z")
         (some "createdAfter" f.created_after));
    Alcotest.(check bool)
      "updatedBefore" true
      (Ptime.equal
         (date "2100-01-01T00:00:00Z")
         (some "updatedBefore" f.updated_before));
    Alcotest.(check bool)
      "updatedAfter" true
      (Ptime.equal
         (date "2000-01-01T00:00:00Z")
         (some "updatedAfter" f.updated_after));
    Alcotest.(check string) "text" "Ada" (some "text" f.text);
    Alcotest.(check string) "name" "Ada" (some "name" f.name);
    (* The solidus of these three is part of the member name, not a nesting. *)
    Alcotest.(check string) "name/given" "Ada" (some "name/given" f.name_given);
    Alcotest.(check string)
      "name/surname" "Lovelace"
      (some "name/surname" f.name_surname);
    Alcotest.(check string)
      "name/surname2" "King"
      (some "name/surname2" f.name_surname2);
    Alcotest.(check string) "nickname" "Countess" (some "nickname" f.nickname);
    Alcotest.(check string)
      "organization" "Analytical Engines"
      (some "organization" f.organization);
    Alcotest.(check string) "email" "ada@example.com" (some "email" f.email);
    Alcotest.(check string) "phone" "+1" (some "phone" f.phone);
    Alcotest.(check string)
      "onlineService" "@ada"
      (some "onlineService" f.online_service);
    Alcotest.(check string) "address" "London" (some "address" f.address);
    Alcotest.(check string) "note" "a note" (some "note" f.note);
    check_member "full condition" "name/given" json;
    check_member "full condition" "name/surname" json;
    check_member "full condition" "name/surname2" json

  let test_filter () =
    let json =
      encode_string "filter" Contact_card.filter_jsont
        (Contact_card.filter
           ~in_address_book:(Id.of_string_exn "Default")
           ~name_given:"Ada" ~name_surname:"Lovelace" ~name_surname2:"King"
           ~kind:"individual" ())
    in
    Alcotest.(check string)
      "filter"
      {|{"inAddressBook":"Default","kind":"individual","name/given":"Ada","name/surname":"Lovelace","name/surname2":"King"}|}
      json

  let test_operator () =
    let f =
      Filter.and_
        [
          Contact_card.filter ~kind:"group" ();
          Contact_card.filter ~text:"Ada" ();
        ]
    in
    let json = encode_string "operator" Contact_card.filter_jsont f in
    Alcotest.(check string)
      "AND filter"
      {|{"operator":"AND","conditions":[{"kind":"group"},{"text":"Ada"}]}|} json;
    ignore (roundtrip "operator" Contact_card.filter_jsont json)

  let sorts =
    [
      (`Created, "created");
      (`Updated, "updated");
      (`Name_given, "name/given");
      (`Name_surname, "name/surname");
      (`Name_surname2, "name/surname2");
    ]

  let test_sort_strings () =
    List.iter
      (fun (p, s) ->
        Alcotest.(check string) "to_string" s (Contact_card.Sort.to_string p);
        Alcotest.(check bool)
          (Printf.sprintf "of_string %S" s)
          true
          (Contact_card.Sort.of_string s = Some p))
      sorts;
    Alcotest.(check bool)
      "unknown sort" true
      (Contact_card.Sort.of_string "name" = None)

  let test_sort_jsont () =
    List.iter
      (fun (p, s) ->
        let json = encode_string "sort" Contact_card.Sort.jsont p in
        Alcotest.(check string) "encoded" (Printf.sprintf "%S" s) json;
        Alcotest.(check bool)
          "decoded" true
          (decode_string "sort" Contact_card.Sort.jsont json = p))
      sorts;
    match decode Contact_card.Sort.jsont {|"name"|} with
    | Ok _ -> Alcotest.fail "expected an unknown sort property to fail"
    | Error _ -> ()

  let test_comparator () =
    (* Section 3.3.2 gives the Comparator property as a string, so
       Sort.to_string is what a caller passes Filter.comparator. *)
    let c =
      Filter.comparator ~is_ascending:false
        (Contact_card.Sort.to_string `Name_surname)
    in
    let json = encode_string "comparator" Filter.comparator_jsont c in
    Alcotest.(check string)
      "comparator" {|{"property":"name/surname","isAscending":false}|} json

  let tests =
    [
      ("empty condition encodes to {}", `Quick, test_empty);
      ("full condition round trip", `Quick, test_full);
      ("filter constructor", `Quick, test_filter);
      ("filter operator", `Quick, test_operator);
      ("sort property strings", `Quick, test_sort_strings);
      ("sort property codec", `Quick, test_sort_jsont);
      ("sort comparator", `Quick, test_comparator);
    ]
end

let () =
  Alcotest.run "contactcard"
    [ ("ContactCard", Card_tests.tests); ("query", Query_tests.tests) ]

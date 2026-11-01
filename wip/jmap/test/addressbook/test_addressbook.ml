(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** RFC 9610 Section 2 AddressBook codec tests, driven by the Section 4.1
    example and by the constraints Section 2 puts on a client side create. *)

open Jmap.Proto

let decode jsont json_str = Jsont_bytesrw.decode_string' jsont json_str
let encode jsont value = Jsont_bytesrw.encode_string' jsont value

let decode_ok name jsont json_str =
  match decode jsont json_str with
  | Ok v -> v
  | Error e ->
      Alcotest.failf "%s: decode failed: %s" name (Jsont.Error.to_string e)

let encode_ok name jsont v =
  match encode jsont v with
  | Ok s -> s
  | Error e ->
      Alcotest.failf "%s: encode failed: %s" name (Jsont.Error.to_string e)

(* Round trip through a decode / encode / decode cycle and check that the
   re-encoded JSON is byte-identical to the first encoding. *)
let roundtrip name jsont json_str =
  let v = decode_ok name jsont json_str in
  let s1 = encode_ok name jsont v in
  let v' = decode_ok (name ^ ": re-decode") jsont s1 in
  let s2 = encode_ok (name ^ ": re-encode") jsont v' in
  Alcotest.(check string) (name ^ ": stable re-encode") s1 s2;
  (s1, v')

let contains ~needle s =
  let n = String.length needle and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = needle || go (i + 1)) in
  n = 0 || go 0

let check_member name member json =
  Alcotest.(check bool)
    (Printf.sprintf "%s: %s present" name member)
    true
    (contains ~needle:member json)

let check_omitted name member json =
  Alcotest.(check bool)
    (Printf.sprintf "%s: %S omitted" name member)
    false
    (contains ~needle:(Printf.sprintf "%S" member) json)

let some name = function
  | Some v -> v
  | None -> Alcotest.failf "%s: expected Some" name

let id_v s = Id.of_string_exn s

(* The two AddressBook objects of the "AddressBook/get" response in RFC 9610
   Section 4.1, verbatim. *)
let rfc_personal =
  {|{
    "id": "062adcfa-105d-455c-bc60-6db68b69c3f3",
    "name": "Personal",
    "description": null,
    "sortOrder": 0,
    "isDefault": true,
    "isSubscribed": true,
    "shareWith": {
      "3f1502e0-63fe-4335-9ff3-e739c188f5dd": {
        "mayRead": true,
        "mayWrite": false,
        "mayShare": false,
        "mayDelete": false
      }
    },
    "myRights": {
      "mayRead": true,
      "mayWrite": true,
      "mayShare": true,
      "mayDelete": false
    }
  }|}

let rfc_autosaved =
  {|{
    "id": "cd40089d-35f9-4fd7-980b-ba3a9f1d74fe",
    "name": "Autosaved",
    "description": null,
    "sortOrder": 1,
    "isDefault": false,
    "isSubscribed": true,
    "shareWith": null,
    "myRights": {
      "mayRead": true,
      "mayWrite": true,
      "mayShare": true,
      "mayDelete": false
    }
  }|}

(* AddressBook (RFC 9610 Section 2) *)
module Address_book_tests = struct
  let test_rfc_example () =
    let _, a = roundtrip "personal" Address_book.jsont rfc_personal in
    Alcotest.(check string)
      "id" "062adcfa-105d-455c-bc60-6db68b69c3f3"
      (Id.to_string (some "id" a.Address_book.id));
    Alcotest.(check (option string))
      "name" (Some "Personal") a.Address_book.name;
    Alcotest.(check bool)
      "description is None" true
      (a.Address_book.description = None);
    Alcotest.(check bool) "sortOrder" true (a.Address_book.sort_order = Some 0L);
    Alcotest.(check bool)
      "isDefault" true
      (a.Address_book.is_default = Some true);
    Alcotest.(check bool)
      "isSubscribed" true
      (a.Address_book.is_subscribed = Some true);
    (match some "shareWith" a.Address_book.share_with with
    | [ (principal, rights) ] ->
        Alcotest.(check string)
          "principal id" "3f1502e0-63fe-4335-9ff3-e739c188f5dd"
          (Id.to_string principal);
        Alcotest.(check bool)
          "shared rights" true
          (rights
          = {
              Address_book.Rights.may_read = true;
              may_write = false;
              may_share = false;
              may_delete = false;
            })
    | l -> Alcotest.failf "shareWith has %d entries" (List.length l));
    Alcotest.(check bool)
      "myRights" true
      (some "myRights" a.Address_book.my_rights
      = {
          Address_book.Rights.may_read = true;
          may_write = true;
          may_share = true;
          may_delete = false;
        })

  let test_rfc_example_unshared () =
    let json, a = roundtrip "autosaved" Address_book.jsont rfc_autosaved in
    Alcotest.(check (option string))
      "name" (Some "Autosaved") a.Address_book.name;
    Alcotest.(check bool)
      "isDefault" true
      (a.Address_book.is_default = Some false);
    Alcotest.(check bool) "sortOrder" true (a.Address_book.sort_order = Some 1L);
    Alcotest.(check bool)
      "shareWith is None" true
      (a.Address_book.share_with = None);
    check_member "autosaved" {|"shareWith":null|} json

  (* RFC 8620 Section 5.1 lets a "/get" name the properties it wants, and the
     server then returns those alone, so a property that was not asked for is
     absent rather than null. *)
  let test_partial_object () =
    let a =
      decode_ok "partial" Address_book.jsont
        {|{"id":"Default","name":"Personal"}|}
    in
    Alcotest.(check (option string))
      "name" (Some "Personal") a.Address_book.name;
    List.iter
      (fun (label, absent) ->
        Alcotest.(check bool) (label ^ " unset") true absent)
      [
        ("description", a.Address_book.description = None);
        ("sortOrder", a.Address_book.sort_order = None);
        ("isDefault", a.Address_book.is_default = None);
        ("isSubscribed", a.Address_book.is_subscribed = None);
        ("shareWith", a.Address_book.share_with = None);
        ("myRights", a.Address_book.my_rights = None);
      ];
    let json = encode_ok "partial" Address_book.jsont a in
    check_omitted "partial" "sortOrder" json;
    check_omitted "partial" "myRights" json

  (* Section 2 types description String|null and shareWith
     Id[AddressBookRights]|null, so a re-encode must state the null rather
     than drop the member and turn "no description" into "not asked for". *)
  let test_nulls_survive_a_reencode () =
    let json, a =
      roundtrip "explicit nulls" Address_book.jsont
        {|{"id":"Default","description":null,"shareWith":null}|}
    in
    Alcotest.(check bool)
      "description is None" true
      (a.Address_book.description = None);
    Alcotest.(check bool)
      "shareWith is None" true
      (a.Address_book.share_with = None);
    check_member "explicit nulls" {|"description":null|} json;
    check_member "explicit nulls" {|"shareWith":null|} json

  let test_rights () =
    let json =
      {|{"mayRead":true,"mayWrite":false,"mayShare":true,"mayDelete":false}|}
    in
    let r = decode_ok "rights" Address_book.Rights.jsont json in
    Alcotest.(check bool)
      "rights" true
      (r
      = {
          Address_book.Rights.may_read = true;
          may_write = false;
          may_share = true;
          may_delete = false;
        });
    Alcotest.(check string)
      "encode" json
      (encode_ok "rights" Address_book.Rights.jsont r);
    (* Every member of an AddressBookRights is mandatory. *)
    Alcotest.(check bool)
      "a missing member is rejected" true
      (Result.is_error (decode Address_book.Rights.jsont {|{"mayRead":true}|}))

  let tests =
    [
      ("RFC 9610 Section 4.1 example", `Quick, test_rfc_example);
      ("RFC 9610 Section 4.1 unshared", `Quick, test_rfc_example_unshared);
      ("a partial /get result", `Quick, test_partial_object);
      ("null description and shareWith", `Quick, test_nulls_survive_a_reencode);
      ("AddressBookRights", `Quick, test_rights);
    ]
end

(* AddressBook creation constraints (RFC 9610 Section 2) *)
module Create_tests = struct
  let ok name f =
    match f () with
    | Ok v -> v
    | Error msg -> Alcotest.failf "%s: rejected: %s" name msg

  let bad name f =
    match f () with
    | Error _ -> ()
    | Ok _ -> Alcotest.failf "%s: expected a rejection" name

  let test_create () =
    let a =
      ok "work" (fun () ->
          Address_book.create ~name:"Work" ~description:"Colleagues"
            ~sort_order:10L ~is_subscribed:true ())
    in
    Alcotest.(check (option string)) "name" (Some "Work") a.Address_book.name;
    Alcotest.(check (option string))
      "description" (Some "Colleagues") a.Address_book.description;
    Alcotest.(check bool) "sortOrder" true (a.Address_book.sort_order = Some 10L);
    (* The server-set properties are left unset, so the create object omits
       them (RFC 9610 Section 2). *)
    Alcotest.(check bool) "id unset" true (a.Address_book.id = None);
    Alcotest.(check bool)
      "isDefault unset" true
      (a.Address_book.is_default = None);
    Alcotest.(check bool) "myRights unset" true (a.Address_book.my_rights = None);
    let json = encode_ok "create" Address_book.jsont a in
    check_omitted "create" "id" json;
    check_omitted "create" "isDefault" json;
    check_omitted "create" "myRights" json

  (* "This MUST NOT be the empty string and MUST NOT be greater than 255
     octets in size when encoded as UTF-8." *)
  let test_create_checks_the_name () =
    bad "empty name" (fun () -> Address_book.create ~name:"" ());
    Alcotest.check_raises "create_exn raises on an empty name"
      (Invalid_argument
         ("Contacts_addressbook.create_exn: "
         ^ Result.get_error (Address_book.create ~name:"" ())))
      (fun () -> ignore (Address_book.create_exn ~name:"" ()));
    ignore
      (ok "255 octets" (fun () ->
           Address_book.create ~name:(String.make 255 'a') ()));
    bad "256 octets" (fun () ->
        Address_book.create ~name:(String.make 256 'a') ());
    (* The cap is on octets, not characters: 128 copies of "é" are 128
       characters but 256 octets when encoded as UTF-8. *)
    ignore
      (ok "127 two octet characters" (fun () ->
           Address_book.create
             ~name:(String.concat "" (List.init 127 (fun _ -> "\xc3\xa9")))
             ()));
    bad "128 two octet characters" (fun () ->
        Address_book.create
          ~name:(String.concat "" (List.init 128 (fun _ -> "\xc3\xa9")))
          ());
    (* RFC 8620 Section 1.5 makes every string I-JSON, so a byte sequence
       that is not UTF-8 is rejected rather than sent on. *)
    bad "invalid UTF-8" (fun () -> Address_book.create ~name:"\xff\xfe" ());
    ignore (ok "UTF-8" (fun () -> Address_book.create ~name:"Amis" ()))

  (* "The number MUST be an integer in the range 0 <= sortOrder < 2^31." *)
  let test_create_checks_the_sort_order () =
    bad "negative sortOrder" (fun () ->
        Address_book.create ~name:"Work" ~sort_order:(-1L) ());
    bad "sortOrder = 2^31" (fun () ->
        Address_book.create ~name:"Work" ~sort_order:2147483648L ());
    ignore
      (ok "sortOrder = 2^31 - 1" (fun () ->
           Address_book.create ~name:"Work" ~sort_order:2147483647L ()));
    ignore
      (ok "sortOrder = 0" (fun () ->
           Address_book.create ~name:"Work" ~sort_order:0L ()))

  (* Section 2 lets shareWith be set only by a user with the mayShare right,
     which is server state, so the client side create takes the map as is. *)
  let test_create_share_with () =
    let rights =
      {
        Address_book.Rights.may_read = true;
        may_write = false;
        may_share = false;
        may_delete = false;
      }
    in
    let a =
      ok "shared" (fun () ->
          Address_book.create ~name:"Team"
            ~share_with:[ (id_v "P1", rights) ]
            ())
    in
    let json = encode_ok "shared" Address_book.jsont a in
    check_member "shared" {|"shareWith":{"P1":|} json

  (* A creation id is typed by the record it names, so an AddressBook token
     needs no annotation where a /set expects one. *)
  let test_creation () =
    let c = Address_book.creation "k1" in
    Alcotest.(check string) "creation id" "k1" (Id.to_string (Id.creation_id c));
    Alcotest.(check string)
      "creation reference" "#k1"
      (Id.to_string (Id.creation_ref c))

  let tests =
    [
      ("create keeps the client settable properties", `Quick, test_create);
      ("create checks the name", `Quick, test_create_checks_the_name);
      ("create checks the sortOrder", `Quick, test_create_checks_the_sort_order);
      ("create takes a shareWith map", `Quick, test_create_share_with);
      ("creation is typed", `Quick, test_creation);
    ]
end

(* Property names (RFC 9610 Section 2) *)
module Property_tests = struct
  let all : Address_book.property list =
    [
      `Id;
      `Name;
      `Description;
      `Sort_order;
      `Is_default;
      `Is_subscribed;
      `Share_with;
      `My_rights;
    ]

  let test_roundtrip () =
    List.iter
      (fun p ->
        let s = Address_book.property_to_string p in
        Alcotest.(check bool)
          (Printf.sprintf "%s round trips" s)
          true
          (Address_book.property_of_string s = Some p))
      all;
    Alcotest.(check int) "every property is covered" 8 (List.length all);
    Alcotest.(check bool)
      "an unknown name is None" true
      (Address_book.property_of_string "sortorder" = None)

  let tests = [ ("property names round trip", `Quick, test_roundtrip) ]
end

let () =
  Alcotest.run "jmap-addressbook"
    [
      ("addressbook", Address_book_tests.tests);
      ("addressbook-create", Create_tests.tests);
      ("addressbook-property", Property_tests.tests);
    ]

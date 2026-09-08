(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* RFC 9610 AddressBook and ContactCard against the Cyrus oracle. *)

open Jmap.Proto
module H = Oracle_harness
module Chain = Jmap.Chain
module Card = Jscontact.Card
module Patch = Jmap.Proto.Patch

let caps = H.contacts_capabilities

let some name = function
  | Some v -> v
  | None -> Alcotest.failf "%s: expected a value" name

let books t =
  H.call ~capabilities:caps t
    (Chain.address_book_get ~account_id:t.H.account_id ())

(* RFC 9610 Section 1.4.1 puts two members in the account capability. *)
let capability t =
  let session = Jmap_eio.Client.session t.H.client in
  Alcotest.(check bool)
    "the session advertises contacts" true
    (List.mem_assoc Capability.contacts session.capabilities);
  let account = List.assoc t.H.account_id session.accounts in
  match Session.contacts_capability account with
  | Some c ->
      Alcotest.(check bool)
        "mayCreateAddressBook is stated" true
        (c.Capability.Contacts.may_create_address_book <> None)
  | None -> Alcotest.fail "the account has no contacts capability object"

(* Section 2.1: ids may be null to fetch every AddressBook at once. *)
let address_book_get t =
  let r = books t in
  Alcotest.(check bool) "at least one AddressBook" true (r.Method.list <> []);
  let default =
    List.find_opt (fun (b : Address_book.t) -> b.is_default = Some true) r.list
  in
  let default = some "a default AddressBook" default in
  Alcotest.(check bool) "the default book has a name" true (default.name <> None);
  let rights = some "myRights" default.my_rights in
  Alcotest.(check bool) "may read the default book" true rights.may_read

(* A /get naming properties returns those alone, plus the id. *)
let address_book_partial t =
  let r =
    H.call ~capabilities:caps t
      (Chain.address_book_get ~account_id:t.H.account_id ~properties:[ `Name ]
         ())
  in
  let b = List.hd r.Method.list in
  Alcotest.(check bool) "id is always returned" true (b.Address_book.id <> None);
  Alcotest.(check bool) "name was asked for" true (b.name <> None);
  Alcotest.(check bool) "myRights was not" true (b.my_rights = None)

let address_book_state t =
  let s =
    H.call ~capabilities:caps t
      (Chain.address_book_state ~account_id:t.H.account_id)
  in
  Alcotest.(check bool) "a state string" true (String.length s > 0);
  let c =
    H.call ~capabilities:caps t
      (Chain.address_book_changes ~account_id:t.H.account_id ~since_state:s ())
  in
  Alcotest.(check bool)
    "no change since that state" true
    (c.Method.created = [] && c.updated = [] && c.destroyed = [])

let default_book t =
  let r = books t in
  let b =
    some "a default AddressBook"
      (List.find_opt
         (fun (b : Address_book.t) -> b.is_default = Some true)
         r.Method.list)
  in
  some "the default book id" b.Address_book.id

(* A card the oracle will accept: Section 3 requires at least one AddressBook,
   and RFC 9553 Section 2.1 the @type, version and uid. *)
let a_card t ~uid ~full =
  let card =
    Card.make
      ~name:(Jscontact.Name.make ~full ())
      ~emails:
        [
          ( Jscontact.Id.v "e1",
            Jscontact.Contact.Email_address.make "oracle@example.com" );
        ]
      uid
  in
  Contact_card.make ~address_book_ids:[ (default_book t, true) ] card

let destroy t ids =
  ignore
    (H.call ~capabilities:caps t
       (Chain.contact_card_set ~account_id:t.H.account_id
          ~destroy:(Chain.ids ids) ()))

(* Create, read back whole, read back partial, then destroy. *)
let contact_card_roundtrip t =
  let uid = "urn:uuid:" ^ H.unique "card" in
  let cid = Contact_card.creation "c1" in
  let r =
    H.call ~capabilities:caps t
      (Chain.contact_card_set ~account_id:t.H.account_id
         ~create:[ (cid, a_card t ~uid ~full:"Oracle Person") ]
         ())
  in
  Alcotest.(check bool)
    "nothing failed to create" true
    (Method.set_failures r = []);
  let created = some "the created card" (Method.created r cid) in
  let id = some "the server set id" created.Contact_card.id in
  let fin () = destroy t [ id ] in
  Fun.protect ~finally:fin @@ fun () ->
  let got =
    H.call ~capabilities:caps t
      (Chain.contact_card_get ~account_id:t.H.account_id ~ids:(Chain.ids [ id ])
         ())
  in
  let c = List.hd got.Method.list in
  Alcotest.(check (option string))
    "the uid came back" (Some uid) (Some c.Contact_card.card.Card.uid);
  Alcotest.(check (option string))
    "the name came back" (Some "Oracle Person")
    (Option.bind c.card.Card.name (fun n -> n.Jscontact.Name.full));
  Alcotest.(check bool)
    "it is in an address book" true
    (c.address_book_ids <> None);
  (* Section 5.1 of RFC 8620: a properties argument truncates the object, so
     the mandatory @type, version and uid of RFC 9553 all go missing. *)
  let partial =
    H.call ~capabilities:caps t
      (Chain.contact_card_get ~account_id:t.H.account_id ~ids:(Chain.ids [ id ])
         ~properties:[ "name" ] ())
  in
  let p = List.hd partial.Method.list in
  Alcotest.(check string)
    "a truncated card has no uid" "" p.Contact_card.card.Card.uid;
  Alcotest.(check bool) "but it has the name" true (p.card.Card.name <> None)

(* Section 3.3: query by text, sorted by created. *)
let contact_card_query t =
  let uid = "urn:uuid:" ^ H.unique "query" in
  let full = "Query " ^ H.unique "person" in
  let cid = Contact_card.creation "q1" in
  let r =
    H.call ~capabilities:caps t
      (Chain.contact_card_set ~account_id:t.H.account_id
         ~create:[ (cid, a_card t ~uid ~full) ]
         ())
  in
  let created = some "the created card" (Method.created r cid) in
  let id = some "the server set id" created.Contact_card.id in
  Fun.protect ~finally:(fun () -> destroy t [ id ]) @@ fun () ->
  let q =
    H.call ~capabilities:caps t
      (Chain.contact_card_query ~account_id:t.H.account_id
         ~filter:(Contact_card.filter ~text:full ())
         ~sort:
           [
             Filter.comparator ~is_ascending:false
               (Contact_card.Sort.to_string `Created);
           ]
         ())
  in
  Alcotest.(check bool)
    "the new card is found" true
    (List.exists (Id.equal id) q.Method.ids);
  (* Section 3.3.1: filtering on the uid is exact. *)
  let by_uid =
    H.call ~capabilities:caps t
      (Chain.contact_card_query ~account_id:t.H.account_id
         ~filter:(Contact_card.filter ~uid ())
         ())
  in
  Alcotest.(check int)
    "exactly one card has that uid" 1
    (List.length by_uid.Method.ids)

(* RFC 8620 Section 5.4 needs both accounts of a /copy in one session. RFC 9610
   Section 2 shares an AddressBook through its shareWith property, and a JMAP
   server then shows the sharer's account to the user it was shared with. *)
let share t rights =
  let books = books t in
  let default =
    some "a default AddressBook"
      (List.find_opt
         (fun (b : Address_book.t) -> b.is_default = Some true)
         books.Method.list)
  in
  let id = some "the default book id" default.Address_book.id in
  let patch =
    match rights with
    | Some r ->
        Patch.v
          [
            Patch.set_field "shareWith"
              (match
                 Jsont.Json.encode
                   (Json_map.of_id Address_book.Rights.jsont)
                   [ (Id.of_string_exn (H.other_user ()), r) ]
               with
              | Ok j -> j
              | Error e -> Alcotest.failf "shareWith: %s" e);
          ]
    | None -> Patch.v [ Patch.set_field "shareWith" (Jsont.Json.null ()) ]
  in
  let r =
    H.call ~capabilities:caps t
      (Chain.address_book_set ~account_id:t.H.account_id
         ~update:[ (id, patch) ]
         ())
  in
  Alcotest.(check bool)
    "the share was accepted" true
    (Method.set_failures r = []);
  id

let book_ids ids =
  match
    Jsont.Json.encode Json_map.id_to_bool (List.map (fun i -> (i, true)) ids)
  with
  | Ok j -> j
  | Error e -> Alcotest.failf "addressBookIds: %s" e

let contact_card_copy t =
  let rights =
    {
      Address_book.Rights.may_read = true;
      may_write = true;
      may_share = false;
      may_delete = false;
    }
  in
  let _book = share t (Some rights) in
  Fun.protect ~finally:(fun () -> ignore (share t None)) @@ fun () ->
  let uid = "urn:uuid:" ^ H.unique "copy" in
  let cid = Contact_card.creation "src" in
  let r =
    H.call ~capabilities:caps t
      (Chain.contact_card_set ~account_id:t.H.account_id
         ~create:[ (cid, a_card t ~uid ~full:"Copy Source") ]
         ())
  in
  let created = some "the created card" (Method.created r cid) in
  let source = some "the server set id" created.Contact_card.id in
  Fun.protect ~finally:(fun () -> destroy t [ source ]) @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client, other_account = H.connect_other ~sw t in
  (* The destination account's own AddressBook, since the source account's book
     ids mean nothing there. *)
  let their_books =
    H.call ~client ~capabilities:caps t
      (Chain.address_book_get ~account_id:other_account ())
  in
  let their_book =
    some "the other account's default book"
      (List.find_opt
         (fun (b : Address_book.t) -> b.is_default = Some true)
         their_books.Method.list)
  in
  let their_book_id = some "its id" their_book.Address_book.id in
  let ccid = Contact_card.creation "copy" in
  let copied =
    H.call ~client ~capabilities:caps t
      (Chain.contact_card_copy ~from_account_id:t.H.account_id
         ~account_id:other_account
         ~create:
           [
             (ccid, source, [ ("addressBookIds", book_ids [ their_book_id ]) ]);
           ]
         ())
  in
  Alcotest.(check bool)
    "nothing failed to copy" true
    (copied.Method.not_created = None || copied.not_created = Some []);
  let made =
    some "the copy"
      (List.assoc_opt (Id.creation_id ccid)
         (Option.value copied.Method.created ~default:[]))
  in
  let copy_id = some "the copy id" made.Contact_card.id in
  Fun.protect ~finally:(fun () ->
      ignore
        (H.call ~client ~capabilities:caps t
           (Chain.contact_card_set ~account_id:other_account
              ~destroy:(Chain.ids [ copy_id ]) ())))
  @@ fun () ->
  let got =
    H.call ~client ~capabilities:caps t
      (Chain.contact_card_get ~account_id:other_account
         ~ids:(Chain.ids [ copy_id ]) ())
  in
  let c = List.hd got.Method.list in
  Alcotest.(check string)
    "the copy keeps the uid" uid c.Contact_card.card.Card.uid;
  Alcotest.(check (option string))
    "and the name" (Some "Copy Source")
    (Option.bind c.card.Card.name (fun n -> n.Jscontact.Name.full))

let () =
  H.run "oracle-contacts"
    [
      ("capability", [ H.test_case "the contacts capability" capability ]);
      ( "AddressBook",
        [
          H.test_case "AddressBook/get" address_book_get;
          H.test_case "a properties argument" address_book_partial;
          H.test_case "state and changes" address_book_state;
        ] );
      ( "ContactCard",
        [
          H.test_case "create, read, destroy" contact_card_roundtrip;
          H.test_case "query" contact_card_query;
          H.test_case "copy between accounts" contact_card_copy;
        ] );
    ]

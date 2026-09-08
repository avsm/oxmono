(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The CardDAV client against a Radicale server, RFC 6352 end to end. *)

open Oracle_harness
module Client = Carddav_eio.Client
module Data = Carddav.Data

let join collection name =
  if String.ends_with ~suffix:"/" collection then collection ^ name
  else collection ^ "/" ^ name

let fn v = Option.map Vcard.Property.text (Vcard.find v "FN")

let hrefs entries =
  List.sort compare
    (List.map (fun (e : _ Client.entry) -> Httpz_dav.href_path e.href) entries)

let member_hrefs ms =
  List.sort compare
    (List.map (fun (m : Client.member) -> Httpz_dav.href_path m.href) ms)

let test_discovery t =
  let principal = Client.principal t.client in
  Alcotest.(check bool)
    "principal names the user" true
    (String.ends_with ~suffix:("/" ^ t.user ^ "/") principal);
  Alcotest.(check bool) "a home set" true (Client.home_sets t.client <> [])

let test_addressbooks t =
  let url = fresh_addressbook t in
  let books = ok "addressbooks" (Client.addressbooks t.client) in
  let book =
    match
      List.find_opt
        (fun (b : Carddav.Addressbook.t) -> Httpz_dav.same_href b.href url)
        books
    with
    | Some b -> b
    | None -> Alcotest.fail "the new address book is not listed"
  in
  Alcotest.(check (option string))
    "display name" (Some "Oracle") book.display_name;
  Alcotest.(check bool)
    "advertises addressbook-query" true
    (Carddav.Addressbook.supports Carddav.Property.addressbook_query book);
  Alcotest.(check bool)
    "advertises sync-collection" true
    (Carddav.Addressbook.supports (Httpz_dav.dav "sync-collection") book);
  let one = ok "addressbook" (Client.addressbook t.client url) in
  Alcotest.(check (option string))
    "same book" book.display_name one.display_name;
  ok "proppatch"
    (Client.set_props t.client url
       [ Httpz_dav.Set [ Httpz_dav.leaf Httpz_dav.Prop.displayname "Renamed" ] ]);
  let renamed = ok "addressbook" (Client.addressbook t.client url) in
  Alcotest.(check (option string))
    "renamed" (Some "Renamed") renamed.display_name;
  ok "delete address book" (Client.delete_addressbook t.client url);
  match Client.addressbook t.client url with
  | Error (Client.Not_found _) -> ()
  | Error e -> Alcotest.failf "unexpected error: %s" (Client.error_to_string e)
  | Ok _ -> Alcotest.fail "the address book survived its deletion"

let test_objects t =
  let url = fresh_addressbook t in
  let alice = vcard ~email:"alice@example.com" "Alice Example" in
  let { Client.href; etag; _ } =
    ok "add" (Client.add Data.vcard t.client url alice)
  in
  Alcotest.(check bool) "an etag" true (etag <> None);
  let got = ok "get" (Client.get Data.vcard t.client href) in
  Alcotest.(check (option string)) "FN" (Some "Alice Example") (fn got.value);
  Alcotest.(check (option string)) "etag agrees" etag got.etag;
  let listed = ok "list" (Client.list t.client url) in
  Alcotest.(check (list string))
    "listed"
    [ Httpz_dav.href_path href ]
    (member_hrefs listed);
  (match Client.put Data.vcard t.client ~create:true href alice with
  | Error (Client.Precondition_failed _) -> ()
  | Error e -> Alcotest.failf "unexpected error: %s" (Client.error_to_string e)
  | Ok _ -> Alcotest.fail "If-None-Match: * did not refuse an existing resource");
  (match Client.put Data.vcard t.client ~etag:"\"stale\"" href alice with
  | Error (Client.Precondition_failed _) -> ()
  | Error e -> Alcotest.failf "unexpected error: %s" (Client.error_to_string e)
  | Ok _ -> Alcotest.fail "If-Match did not refuse a stale etag");
  let updated =
    Vcard.v
      (List.map
         (fun p ->
           if Vcard.Property.name p = "FN" then
             Vcard.Property.of_text "FN" "Alice B. Example"
           else p)
         (Vcard.properties alice))
  in
  let etag2 = ok "update" (Client.put Data.vcard t.client ?etag href updated) in
  Alcotest.(check bool) "etag changed" true (etag2 <> etag);
  let got = ok "get" (Client.get Data.vcard t.client href) in
  Alcotest.(check (option string))
    "updated" (Some "Alice B. Example") (fn got.value);
  ok "delete" (Client.delete t.client ?etag:etag2 href);
  match Client.get Data.vcard t.client href with
  | Error (Client.Not_found _) -> ()
  | Error e -> Alcotest.failf "unexpected error: %s" (Client.error_to_string e)
  | Ok _ -> Alcotest.fail "the object survived its deletion"

let test_reports t =
  let url = fresh_addressbook t in
  let { Client.href = a; _ } =
    ok "add"
      (Client.add Data.vcard t.client url (vcard ~email:"a@example.com" "Ann"))
  in
  let { Client.href = b; _ } =
    ok "add"
      (Client.add Data.vcard t.client url (vcard ~email:"b@example.org" "Bob"))
  in
  let _ = ok "add" (Client.add Data.vcard t.client url (vcard "Cid")) in
  let filter =
    Carddav.Filter.v
      [
        Carddav.Filter.prop "EMAIL"
          [ Carddav.Filter.text_match ~match_type:`Ends_with "example.com" ];
      ]
  in
  let page = ok "query" (Client.query Data.vcard t.client url filter) in
  Alcotest.(check bool) "not truncated" false page.truncated;
  Alcotest.(check (list string))
    "query"
    [ Httpz_dav.href_path a ]
    (hrefs page.entries);
  let found =
    (ok "query all" (Client.query Data.vcard t.client url Carddav.Filter.all))
      .entries
  in
  Alcotest.(check int) "three" 3 (List.length found);
  List.iter
    (fun (e : _ Client.entry) ->
      Alcotest.(check bool)
        "filter agrees" true
        (Carddav.Filter.matches filter e.value
        = List.mem (Httpz_dav.href_path e.href) [ Httpz_dav.href_path a ]))
    found;
  let got =
    ok "multiget"
      (Client.multiget Data.vcard t.client url [ a; b; join url "missing.vcf" ])
  in
  Alcotest.(check (list string))
    "multiget"
    (List.sort compare [ Httpz_dav.href_path a; Httpz_dav.href_path b ])
    (hrefs got);
  let raw = ok "raw" (Client.get Data.raw t.client a) in
  Alcotest.(check bool)
    "raw text" true
    (String.starts_with ~prefix:"BEGIN:VCARD" raw.value)

let test_sync t =
  let url = fresh_addressbook t in
  let first = ok "initial sync" (Client.sync Data.vcard t.client url) in
  Alcotest.(check bool) "a token" true (first.token <> None);
  Alcotest.(check int) "empty" 0 (List.length first.changes);
  let { Client.href = a; _ } =
    ok "add" (Client.add Data.vcard t.client url (vcard "Ann"))
  in
  let { Client.href = b; _ } =
    ok "add" (Client.add Data.vcard t.client url (vcard "Bob"))
  in
  let second =
    ok "sync" (Client.sync Data.vcard t.client ?token:first.token url)
  in
  let changed =
    List.filter_map
      (function
        | Client.Changed e -> Some (Httpz_dav.href_path e.href)
        | Client.Removed _ -> None)
      second.changes
  in
  Alcotest.(check (list string))
    "two added"
    (List.sort compare [ Httpz_dav.href_path a; Httpz_dav.href_path b ])
    (List.sort compare changed);
  ok "delete" (Client.delete t.client a);
  let third =
    ok "sync" (Client.sync Data.vcard t.client ?token:second.token url)
  in
  let removed =
    List.filter_map
      (function
        | Client.Removed h -> Some (Httpz_dav.href_path h)
        | Client.Changed _ -> None)
      third.changes
  in
  Alcotest.(check (list string)) "one removed" [ Httpz_dav.href_path a ] removed;
  let token = ok "sync token" (Client.sync_token t.client url) in
  Alcotest.(check bool) "token property" true (token <> None)

let test_jscontact t =
  let url = fresh_addressbook t in
  let card =
    Jscontact.Card.make ~kind:`Individual
      ~name:(Jscontact.Name.make ~full:"Carol Example" ())
      ~emails:
        [
          ( Jscontact.Id.v "e1",
            Jscontact.Contact.Email_address.make ~contexts:[ `Work ] ~pref:1
              "carol@example.com" );
        ]
      "carol-uid-1"
  in
  let { Client.href; _ } =
    ok "add card" (Client.add Carddav_jscontact.card t.client url card)
  in
  Alcotest.(check bool)
    "named by uid" true
    (String.ends_with ~suffix:"carol-uid-1.vcf" href);
  let got = ok "get card" (Client.get Carddav_jscontact.card t.client href) in
  Alcotest.(check string) "uid" "carol-uid-1" got.value.uid;
  Alcotest.(check (option string))
    "full name" (Some "Carol Example")
    (Option.bind got.value.name (fun (n : Jscontact.Name.t) -> n.full));
  (* A server that serves vCard 3.0 folds PREF into TYPE and KIND into an X
     property; the vcard3 quirk maps them back, so both survive. *)
  Alcotest.(check string)
    "kind survives" "individual"
    (Jscontact.Card.Kind.to_string got.value.kind);
  Alcotest.(check (option int))
    "pref survives" (Some 1)
    (match got.value.emails with
    | Some ((_, e) :: _) -> e.Jscontact.Contact.Email_address.pref
    | _ -> None);
  let raw = ok "raw" (Client.get Data.raw t.client href) in
  (* RFC 6352 Section 5.1 lets a server store any version it supports.
     Fastmail serves vCard 3.0, which is noted. *)
  if
    try
      ignore (Str.search_forward (Str.regexp_string "VERSION:4.0") raw.value 0);
      false
    with Not_found -> true
  then Printf.printf "  [note] the server serves the card as vCard 3.0\n";
  let found =
    (ok "query cards"
       (Client.query Carddav_jscontact.card t.client url
          (Carddav.Filter.v
             [
               Carddav.Filter.prop "EMAIL" [ Carddav.Filter.text_match "carol" ];
             ])))
      .entries
  in
  Alcotest.(check int) "one card" 1 (List.length found)

let () =
  Alcotest.run "carddav-oracle"
    [
      ( "radicale",
        [
          test_case "discovery" test_discovery;
          test_case "address books" test_addressbooks;
          test_case "address objects" test_objects;
          test_case "reports" test_reports;
          test_case "sync" test_sync;
          test_case "jscontact" test_jscontact;
        ] );
    ]

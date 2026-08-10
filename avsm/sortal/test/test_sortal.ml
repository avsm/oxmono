(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Tests for the Sortal library *)

open Eio.Std
module Account = Sortal.Contact.Account
module Platform = Sortal.Contact.Platform

let test_contact_creation () =
  let c = Sortal.Contact.make
    ~handle:"test"
    ~names:["Test User"; "T. User"]
    ~emails:["test@example.com"]
    ~accounts:[Account.Simple (Github, "testuser")]
    () in
  assert (Sortal.Contact.handle c = "test");
  assert (Sortal.Contact.name c = "Test User");
  assert (List.length (Sortal.Contact.names c) = 2);
  assert (Sortal.Contact.emails c = ["test@example.com"]);
  assert (List.length (Sortal.Contact.accounts c) = 1);
  assert (Sortal.Contact.handle_on c (Simple Github) = Some "testuser");
  traceln "✓ Contact creation works"

let test_best_url () =
  let c1 = Sortal.Contact.make
    ~handle:"test1"
    ~names:["Test 1"]
    ~links:[{ Sortal.Contact.url = "https://example.com"; label = None }]
    ~accounts:[Account.Simple (Github, "test1")]
    () in
  assert (Sortal.Contact.best_url c1 = Some "https://example.com");

  let c2 = Sortal.Contact.make
    ~handle:"test2"
    ~names:["Test 2"]
    ~accounts:[Account.Simple (Github, "test2")]
    () in
  assert (Sortal.Contact.best_url c2 = Some "https://github.com/test2");

  (* An email alone gives no URL: V2's [best_url] has no mailto fallback. *)
  let c3 = Sortal.Contact.make
    ~handle:"test3"
    ~names:["Test 3"]
    ~emails:["test3@example.com"]
    () in
  assert (Sortal.Contact.best_url c3 = None);

  let c4 = Sortal.Contact.make
    ~handle:"test4"
    ~names:["Test 4"]
    () in
  assert (Sortal.Contact.best_url c4 = None);

  traceln "✓ Best URL selection works"

let test_json_encoding () =
  let c = Sortal.Contact.make
    ~handle:"json_test"
    ~names:["JSON Test"]
    ~emails:["json@example.com"]
    ~accounts:[
      Account.Simple (Github, "jsontest");
      Account.Simple (Orcid, "0000-0001-2345-6789");
    ]
    () in

  match Jsont_bytesrw.encode_string Sortal.Contact.json_t c with
  | Ok json_str ->
      (match Jsont_bytesrw.decode_string Sortal.Contact.json_t json_str with
       | Ok decoded ->
           assert (Sortal.Contact.handle decoded = "json_test");
           assert (Sortal.Contact.emails decoded = ["json@example.com"]);
           assert (Sortal.Contact.handle_on decoded (Simple Github) = Some "jsontest");
           assert (Sortal.Contact.handle_on decoded (Simple Orcid) = Some "0000-0001-2345-6789");
           traceln "✓ JSON encoding/decoding works"
       | Error err ->
           failwith ("JSON decode failed: " ^ err))
  | Error err ->
      failwith ("JSON encode failed: " ^ err)

let test_handle_generation () =
  assert (Sortal.handle_of_name "John Smith" = "jssmith");
  assert (Sortal.handle_of_name "Alice Barbara Cooper" = "abccooper");
  assert (Sortal.handle_of_name "Bob" = "bbob");
  traceln "✓ Handle generation works"

let test_store_operations () =
  Eio_main.run @@ fun env ->

  (* Create a store with a test app name *)
  let store = Sortal.create env#fs "sortal-test" in

  (* Create test contacts *)
  let c1 = Sortal.Contact.make
    ~handle:"alice"
    ~names:["Alice Anderson"]
    ~emails:["alice@example.com"]
    () in

  let c2 = Sortal.Contact.make
    ~handle:"bob"
    ~names:["Bob Brown"; "Robert Brown"]
    ~accounts:[Account.Simple (Github, "bobbrown")]
    () in

  (* Test save *)
  Sortal.save store c1;
  Sortal.save store c2;
  traceln "✓ Saving contacts works";

  (* Test lookup *)
  (match Sortal.lookup store "alice" with
   | Some c ->
       assert (Sortal.Contact.name c = "Alice Anderson");
       traceln "✓ Lookup works"
   | None -> failwith "Lookup failed to find saved contact");

  (* Test lookup of non-existent contact *)
  (match Sortal.lookup store "nonexistent" with
   | None -> traceln "✓ Lookup correctly returns None for missing contact"
   | Some _ -> failwith "Lookup should return None for non-existent contact");

  (* Test list *)
  let all = Sortal.list store in
  assert (List.length all >= 2);
  traceln "✓ List returns saved contacts (%d total)" (List.length all);

  (* Test find_by_name *)
  let found = Sortal.find_by_name store "Bob Brown" in
  assert (Sortal.Contact.handle found = "bob");
  traceln "✓ Find by name works";

  (* Test find_by_name_opt *)
  (match Sortal.find_by_name_opt store "Alice Anderson" with
   | Some c ->
       assert (Sortal.Contact.handle c = "alice");
       traceln "✓ Find by name (optional) works"
   | None -> failwith "find_by_name_opt failed");

  (match Sortal.find_by_name_opt store "Nobody" with
   | None -> traceln "✓ Find by name (optional) returns None for missing"
   | Some _ -> failwith "find_by_name_opt should return None");

  (* Test delete *)
  Sortal.delete store "alice";
  (match Sortal.lookup store "alice" with
   | None -> traceln "✓ Delete works"
   | Some _ -> failwith "Contact should have been deleted");

  (* Clean up remaining test contact *)
  Sortal.delete store "bob";
  traceln "✓ Test cleanup complete"

let test_feed_pause_resume () =
  Eio_main.run @@ fun env ->
  let store = Sortal.create env#fs "sortal-test-pause" in
  let url = "https://example.com/feed.atom" in
  let feed = Sortal.Feed.make ~feed_type:Atom ~url () in
  let c = Sortal.Contact.make ~handle:"paused-test" ~names:["Pause Test"]
    ~feeds:[feed] () in
  Sortal.save store c;

  (* Unknown handle and unknown URL are both reported, not silently
     ignored. *)
  (match Sortal.Store.set_feed_paused store "no-such-handle" url true with
   | Error _ -> traceln "✓ pause reports an unknown handle"
   | Ok () -> failwith "pause should fail for an unknown handle");
  (match Sortal.Store.set_feed_paused store "paused-test" "https://no.such/url" true with
   | Error _ -> traceln "✓ pause reports an unknown feed URL"
   | Ok () -> failwith "pause should fail for an unknown feed URL");

  (match Sortal.Store.set_feed_paused store "paused-test" url true with
   | Error e -> failwith ("pause failed: " ^ e)
   | Ok () -> ());
  (match Sortal.lookup store "paused-test" with
   | None -> failwith "contact vanished after pause"
   | Some c ->
       (match Sortal.Contact.feeds c with
        | [ f ] -> assert (Sortal.Feed.paused f)
        | _ -> failwith "expected exactly one feed"));
  traceln "✓ pause persists the paused flag";

  (match Sortal.Store.set_feed_paused store "paused-test" url false with
   | Error e -> failwith ("resume failed: " ^ e)
   | Ok () -> ());
  (match Sortal.lookup store "paused-test" with
   | None -> failwith "contact vanished after resume"
   | Some c ->
       (match Sortal.Contact.feeds c with
        | [ f ] -> assert (not (Sortal.Feed.paused f))
        | _ -> failwith "expected exactly one feed"));
  traceln "✓ resume clears the paused flag";

  Sortal.delete store "paused-test"

let test_contact_compare () =
  let c1 = Sortal.Contact.make ~handle:"alice" ~names:["Alice"] () in
  let c2 = Sortal.Contact.make ~handle:"bob" ~names:["Bob"] () in
  let c3 = Sortal.Contact.make ~handle:"alice" ~names:["Alice2"] () in

  assert (Sortal.Contact.compare c1 c2 < 0);
  assert (Sortal.Contact.compare c2 c1 > 0);
  assert (Sortal.Contact.compare c1 c3 = 0);
  traceln "✓ Contact comparison works"

let test_links () =
  (* Test with only one link set *)
  let c1 = Sortal.Contact.make
    ~handle:"test1"
    ~names:["Test 1"]
    ~links:[{ Sortal.Contact.url = "https://example.com"; label = None }]
    () in
  assert (Sortal.Contact.best_url c1 = Some "https://example.com");
  assert (List.length (Sortal.Contact.links c1) = 1);

  (* Test with multiple links: the first is preferred *)
  let c2 = Sortal.Contact.make
    ~handle:"test2"
    ~names:["Test 2"]
    ~links:[
      { Sortal.Contact.url = "https://one.com"; label = None };
      { Sortal.Contact.url = "https://two.com"; label = None };
    ]
    () in
  assert (Sortal.Contact.best_url c2 = Some "https://one.com");
  assert (List.length (Sortal.Contact.links c2) = 2);

  (* Test with no links *)
  let c3 = Sortal.Contact.make
    ~handle:"test3"
    ~names:["Test 3"]
    () in
  assert (Sortal.Contact.best_url c3 = None);
  assert (Sortal.Contact.links c3 = []);

  traceln "✓ Links field works correctly"

let () =
  traceln "\n=== Running Sortal Tests ===\n";

  test_contact_creation ();
  test_best_url ();
  test_json_encoding ();
  test_handle_generation ();
  test_contact_compare ();
  test_links ();
  test_store_operations ();
  test_feed_pause_resume ();

  traceln "\n=== All Tests Passed ===\n"

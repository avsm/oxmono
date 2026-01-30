(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Tests for the Sortal library *)

open Eio.Std

let test_contact_creation () =
  let c = Sortal.Contact.make
    ~handle:"test"
    ~names:["Test User"; "T. User"]
    ~emails:[Sortal.Contact.email_of_string "test@example.com"]
    ~services:[Sortal.Contact.make_service ~kind:Git ~handle:"testuser" "https://github.com/testuser"]
    () in
  assert (Sortal.Contact.handle c = "test");
  assert (Sortal.Contact.name c = "Test User");
  assert (List.length (Sortal.Contact.names c) = 2);
  assert (Sortal.Contact.current_email c = Some "test@example.com");
  assert (List.length (Sortal.Contact.services c) = 1);
  assert (List.length (Sortal.Contact.services_of_kind c Git) = 1);
  traceln "✓ Contact creation works"

let test_best_url () =
  let c1 = Sortal.Contact.make
    ~handle:"test1"
    ~names:["Test 1"]
    ~urls:[Sortal.Contact.url_of_string "https://example.com"]
    ~services:[Sortal.Contact.service_of_url "https://github.com/test1"]
    () in
  assert (Sortal.Contact.best_url c1 = Some "https://example.com");

  let c2 = Sortal.Contact.make
    ~handle:"test2"
    ~names:["Test 2"]
    ~services:[Sortal.Contact.service_of_url "https://github.com/test2"]
    () in
  assert (Sortal.Contact.best_url c2 = Some "https://github.com/test2");

  let c3 = Sortal.Contact.make
    ~handle:"test3"
    ~names:["Test 3"]
    ~emails:[Sortal.Contact.email_of_string "test3@example.com"]
    () in
  assert (Sortal.Contact.best_url c3 = Some "mailto:test3@example.com");

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
    ~emails:[Sortal.Contact.email_of_string "json@example.com"]
    ~services:[Sortal.Contact.make_service ~kind:Git ~handle:"jsontest" "https://github.com/jsontest"]
    ~orcid:"0000-0001-2345-6789"
    () in

  match Jsont_bytesrw.encode_string Sortal.Contact.json_t c with
  | Ok json_str ->
      (match Jsont_bytesrw.decode_string Sortal.Contact.json_t json_str with
       | Ok decoded ->
           assert (Sortal.Contact.handle decoded = "json_test");
           assert (Sortal.Contact.current_email decoded = Some "json@example.com");
           assert (List.length (Sortal.Contact.services_of_kind decoded Git) = 1);
           assert (Sortal.Contact.orcid decoded = Some "0000-0001-2345-6789");
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
    ~emails:[Sortal.Contact.email_of_string "alice@example.com"]
    () in

  let c2 = Sortal.Contact.make
    ~handle:"bob"
    ~names:["Bob Brown"; "Robert Brown"]
    ~services:[Sortal.Contact.service_of_url "https://github.com/bobbrown"]
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

let test_contact_compare () =
  let c1 = Sortal.Contact.make ~handle:"alice" ~names:["Alice"] () in
  let c2 = Sortal.Contact.make ~handle:"bob" ~names:["Bob"] () in
  let c3 = Sortal.Contact.make ~handle:"alice" ~names:["Alice2"] () in

  assert (Sortal.Contact.compare c1 c2 < 0);
  assert (Sortal.Contact.compare c2 c1 > 0);
  assert (Sortal.Contact.compare c1 c3 = 0);
  traceln "✓ Contact comparison works"

let test_urls () =
  (* Test with only url set *)
  let c1 = Sortal.Contact.make
    ~handle:"test1"
    ~names:["Test 1"]
    ~urls:[Sortal.Contact.url_of_string "https://example.com"]
    () in
  assert (Sortal.Contact.current_url c1 = Some "https://example.com");
  assert (List.length (Sortal.Contact.urls c1) = 1);

  (* Test with multiple urls *)
  let c2 = Sortal.Contact.make
    ~handle:"test2"
    ~names:["Test 2"]
    ~urls:[
      Sortal.Contact.url_of_string "https://one.com";
      Sortal.Contact.url_of_string "https://two.com"
    ]
    () in
  assert (Sortal.Contact.current_url c2 = Some "https://one.com");
  assert (List.length (Sortal.Contact.urls c2) = 2);

  (* Test with no urls *)
  let c3 = Sortal.Contact.make
    ~handle:"test3"
    ~names:["Test 3"]
    () in
  assert (Sortal.Contact.current_url c3 = None);
  assert (Sortal.Contact.urls c3 = []);

  traceln "✓ URLs field works correctly"

let () =
  traceln "\n=== Running Sortal Tests ===\n";

  test_contact_creation ();
  test_best_url ();
  test_json_encoding ();
  test_handle_generation ();
  test_contact_compare ();
  test_urls ();
  test_store_operations ();

  traceln "\n=== All Tests Passed ===\n"

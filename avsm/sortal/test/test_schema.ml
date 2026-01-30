(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Schema-only tests (no I/O dependencies) *)

let test_temporal () =
  (* Parse dates from strings *)
  let from_date = Sortal_schema.Temporal.parse_date_string "2020-01" |> Option.get in
  let until_date = Sortal_schema.Temporal.parse_date_string "2023-12" |> Option.get in
  let test_date_1 = Sortal_schema.Temporal.parse_date_string "2021-06" |> Option.get in
  let test_date_2 = Sortal_schema.Temporal.parse_date_string "2024-01" |> Option.get in

  let r = Sortal_schema.Temporal.make ~from:from_date ~until:until_date () in
  assert (Sortal_schema.Temporal.valid_at (Some r) ~date:test_date_1);
  assert (not (Sortal_schema.Temporal.valid_at (Some r) ~date:test_date_2));
  print_endline "✓ Temporal ranges work"

let test_feed_types () =
  let feed = Sortal_schema.Feed.make ~feed_type:Atom ~url:"https://example.com/feed" () in
  assert (Sortal_schema.Feed.url feed = "https://example.com/feed");
  print_endline "✓ Feed types work"

let test_contact_construction () =
  let c = Sortal_schema.Contact.make
    ~handle:"test"
    ~names:["Test User"]
    ~emails:[Sortal_schema.Contact.email_of_string "test@example.com"]
    () in
  assert (Sortal_schema.Contact.handle c = "test");
  assert (Sortal_schema.Contact.name c = "Test User");
  print_endline "✓ Contact construction works"

let test_json_roundtrip () =
  let c = Sortal_schema.Contact.make ~handle:"json" ~names:["JSON Test"] () in
  match Jsont_bytesrw.encode_string Sortal_schema.Contact.json_t c with
  | Ok json ->
      (match Jsont_bytesrw.decode_string Sortal_schema.Contact.json_t json with
       | Ok decoded ->
           assert (Sortal_schema.Contact.handle decoded = "json");
           assert (Sortal_schema.Contact.name decoded = "JSON Test");
           print_endline "✓ JSON roundtrip works"
       | Error e -> failwith ("Decode failed: " ^ e))
  | Error e -> failwith ("Encode failed: " ^ e)

let () =
  print_endline "\n=== Schema Tests ===\n";
  test_temporal ();
  test_feed_types ();
  test_contact_construction ();
  test_json_roundtrip ();
  print_endline "\n=== All Schema Tests Passed ===\n"

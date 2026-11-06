(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let test_basic () =
  let content = {|---
title: Hello World
tags:
  - ocaml
  - yaml
---
# Markdown Body

This is the body content.
|} in
  match Frontmatter.of_string content with
  | Error e ->
    Printf.printf "ERROR: %s\n" e;
    false
  | Ok t ->
    let title = Option.value ~default:"(none)" (Frontmatter.find_string "title" t) in
    let tags = String.concat ", " (Frontmatter.find_strings "tags" t) in
    let body = Frontmatter.body t in
    Printf.printf "Title: %s\n" title;
    Printf.printf "Tags: %s\n" tags;
    Printf.printf "Body:\n%s\n" body;
    title = "Hello World" &&
    tags = "ocaml, yaml" &&
    String.length body > 0

let test_no_frontmatter () =
  let content = "No frontmatter here" in
  match Frontmatter.of_string content with
  | Error _ ->
    Printf.printf "Correctly rejected content without frontmatter\n";
    true
  | Ok _ ->
    Printf.printf "ERROR: Should have rejected content without frontmatter\n";
    false

let test_with_dash_in_body () =
  let content = {|---
title: Test
---
Body with --- in it
And more content
|} in
  match Frontmatter.of_string content with
  | Error e ->
    Printf.printf "ERROR: %s\n" e;
    false
  | Ok t ->
    let body = Frontmatter.body t in
    Printf.printf "Body with dashes: %s\n" body;
    String.sub body 0 4 = "Body"

let test_explicit_doc_end () =
  let content = {|---
title: With explicit end
...
Body after explicit document end marker
|} in
  match Frontmatter.of_string content with
  | Error e ->
    Printf.printf "ERROR: %s\n" e;
    false
  | Ok t ->
    let body = Frontmatter.body t in
    Printf.printf "Body after ...: %s\n" body;
    String.sub body 0 4 = "Body"

let test_delimiter_allocations () =
  let source = "---\ntitle: Test\n---\n" ^ String.make 100000 'x' in
  let measure source =
    let parse () =
      match Frontmatter.of_string source with
      | Ok t -> ignore (Sys.opaque_identity t)
      | Error e -> failwith e
    in
    parse ();
    Gc.full_major ();
    let before = Gc.allocated_bytes () in
    for _ = 1 to 10 do parse () done;
    (Gc.allocated_bytes () -. before) /. 80.
  in
  let plain = measure source and trailing = measure (source ^ "\n") in
  if trailing > plain +. 128. then
    failwith "trailing newline copied the entire document";
  List.iter (fun source ->
    match Frontmatter.of_string source with
    | Error _ -> () | Ok _ -> failwith "accepted missing delimiter")
    [""; " \t\n"; "-"; "--"; " \012 -- "; "x---"]

let test_round_trip_and_fields () =
  let check message condition = if not condition then failwith message in
  let t =
    Frontmatter.of_string_exn "---\ntitle: Old\ncount: 3\ndraft: yes\n---\nBody\n"
  in
  check "typed lookups"
    (Frontmatter.find_int "count" t = Some 3
    && Frontmatter.find_bool "draft" t = Some true
    && Frontmatter.find_string "missing" t = None);
  let t = Frontmatter.set_field "title" (`String "New") t in
  let t = Frontmatter.set_field "extra" (`Float 1.5) t in
  let t' = Frontmatter.of_string_exn (Frontmatter.to_string t) in
  check "replaced field survives serialisation"
    (Frontmatter.find_string "title" t' = Some "New");
  check "added field survives serialisation"
    (Frontmatter.find_float "extra" t' = Some 1.5);
  check "body survives serialisation" (Frontmatter.body t' = Frontmatter.body t);
  let crlf = Frontmatter.of_string "---\r\ntitle: Win\r\n---\r\nBody\r\n" in
  check "CRLF delimiters accepted"
    (match crlf with
    | Ok t -> Frontmatter.find_string "title" t = Some "Win"
    | Error _ -> false);
  check "unterminated frontmatter rejected"
    (Result.is_error (Frontmatter.of_string "---\ntitle: x\nbody\n"));
  let date y m d = Ptime.of_date (y, m, d) in
  check "dated slug"
    (Frontmatter.slug_of_fname "2025-01-15-my-post.md"
    = Ok ("my-post", date 2025 1 15));
  check "undated slug"
    (Frontmatter.slug_of_fname "my-post.md" = Ok ("my-post", None))

let () =
  test_delimiter_allocations ();
  test_round_trip_and_fields ();
  Printf.printf "=== Testing basic frontmatter ===\n";
  let r1 = test_basic () in
  Printf.printf "\n=== Testing no frontmatter ===\n";
  let r2 = test_no_frontmatter () in
  Printf.printf "\n=== Testing dash in body ===\n";
  let r3 = test_with_dash_in_body () in
  Printf.printf "\n=== Testing explicit doc end ===\n";
  let r4 = test_explicit_doc_end () in
  if r1 && r2 && r3 && r4 then (
    Printf.printf "\nAll tests passed!\n";
    exit 0
  ) else (
    Printf.printf "\nSome tests failed!\n";
    exit 1
  )

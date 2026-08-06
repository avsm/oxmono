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

let () =
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

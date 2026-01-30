(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Fuzz tests for Encoding module *)

open Crowbar

(** Test that encoding detection never crashes on arbitrary input *)
let () =
  add_test ~name:"encoding: detect crash safety" [ bytes ] @@ fun buf ->
  let _ = Yamlrw.Encoding.detect buf in
  check true

(** Test that to_string never crashes for any detected encoding *)
let () =
  add_test ~name:"encoding: to_string after detect" [ bytes ] @@ fun buf ->
  let enc, _ = Yamlrw.Encoding.detect buf in
  let _ = Yamlrw.Encoding.to_string enc in
  check true

(** Test that pp never crashes *)
let () =
  add_test ~name:"encoding: pp after detect" [ bytes ] @@ fun buf ->
  let enc, _ = Yamlrw.Encoding.detect buf in
  let _ = Format.asprintf "%a" Yamlrw.Encoding.pp enc in
  check true

(** Test encoding equality is reflexive *)
let () =
  add_test ~name:"encoding: equal reflexive" [ bytes ] @@ fun buf ->
  let enc, _ = Yamlrw.Encoding.detect buf in
  if not (Yamlrw.Encoding.equal enc enc) then fail "encoding not equal to itself"
  else check true

(** Test that BOM length is always non-negative and reasonable *)
let () =
  add_test ~name:"encoding: bom_length non-negative" [ bytes ] @@ fun buf ->
  let _, bom_len = Yamlrw.Encoding.detect buf in
  if bom_len < 0 then fail "negative BOM length"
  else if bom_len > 4 then fail "BOM length too large (max 4 for UTF-32)"
  else check true

(** Test specific BOM patterns *)
let () =
  add_test ~name:"encoding: UTF-8 BOM" [ const () ] @@ fun () ->
  let utf8_bom = "\xEF\xBB\xBF" in
  let enc, len = Yamlrw.Encoding.detect utf8_bom in
  if enc <> `Utf8 then fail "expected UTF-8"
  else if len <> 3 then fail "expected BOM length 3"
  else check true

let () =
  add_test ~name:"encoding: UTF-16 BE BOM" [ const () ] @@ fun () ->
  let utf16be_bom = "\xFE\xFF" in
  let enc, len = Yamlrw.Encoding.detect utf16be_bom in
  if enc <> `Utf16be then fail "expected UTF-16 BE"
  else if len <> 2 then fail "expected BOM length 2"
  else check true

let () =
  add_test ~name:"encoding: UTF-16 LE BOM" [ const () ] @@ fun () ->
  (* Use BOM followed by non-null bytes to avoid ambiguity with UTF-32 LE *)
  let utf16le_bom = "\xFF\xFEab" in
  let enc, len = Yamlrw.Encoding.detect utf16le_bom in
  if enc <> `Utf16le then fail "expected UTF-16 LE"
  else if len <> 2 then fail "expected BOM length 2"
  else check true

let () =
  add_test ~name:"encoding: empty string defaults to UTF-8" [ const () ]
  @@ fun () ->
  let enc, len = Yamlrw.Encoding.detect "" in
  if enc <> `Utf8 then fail "expected UTF-8 for empty string"
  else if len <> 0 then fail "expected BOM length 0 for empty string"
  else check true

let run () = ()

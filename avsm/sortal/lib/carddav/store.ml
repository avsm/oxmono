(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Common

let cards_dir root =
  let marker = Filename.concat root "store.json" in
  if exists marker then (
    let store = load_json marker in
    if number (get "version" store) <> 1 then fail "unsupported vCard store";
    safe_path root "cards")
  else root

(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Common

let identity root =
  let marker = load_json (Filename.concat root "store.json") in
  if number (get "version" marker) <> 1 then fail "unsupported native store";
  let id = field "store_id" marker in
  ignore (uuid_bytes id);
  id

let cards_dir root =
  let marker = Filename.concat root "store.json" in
  if exists marker then (
    let store = load_json marker in
    if number (get "version" store) <> 1 then fail "unsupported vCard store";
    safe_path root "cards")
  else root

(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t =
  | Atom
  | Rss
  | Json
  | Html
  | Unknown of string

let is_ws = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let skip_ws s i =
  let n = String.length s in
  let rec go i = if i < n && is_ws s.[i] then go (i + 1) else i in
  go i

(* Skip a leading "<?xml ... ?>" declaration, if present. Anything else
   ahead of the root element, such as an HTML-escaped declaration, is not
   skipped: the owner declined to tolerate leading junk, so a feed like
   that falls through to [Unknown]. *)
let skip_xml_decl s i =
  let n = String.length s in
  if i + 5 <= n && String.sub s i 5 = "<?xml" then
    match String.index_from_opt s i '>' with
    | Some j -> j + 1
    | None -> i
  else i

let starts_with_ci prefix s i =
  let pn = String.length prefix in
  i + pn <= String.length s
  && String.lowercase_ascii (String.sub s i pn) = String.lowercase_ascii prefix

let excerpt s i =
  let n = String.length s in
  let len = min (n - i) 80 in
  let e = String.sub s i len in
  if i + len < n then e ^ "..." else e

let detect body =
  let i = skip_ws body 0 in
  let i = skip_xml_decl body i in
  let i = skip_ws body i in
  if i < String.length body && body.[i] = '{' then Json
  else if starts_with_ci "<feed" body i then Atom
  else if starts_with_ci "<rss" body i then Rss
  else if starts_with_ci "<rdf:rdf" body i then Rss
  else if starts_with_ci "<html" body i then Html
  else if starts_with_ci "<!doctype html" body i then Html
  else Unknown (excerpt body i)

(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)

type t = { prefix : string; suffix : string }

let is_digits s =
  s <> "" && String.for_all (function '0' .. '9' -> true | _ -> false) s

(* ISO 26324 4.3.1: a DOI name is Graphic code points only. Bytes are
   not classified beyond ASCII, so the check is on the control range. *)
let has_control s =
  String.exists (fun c -> c < ' ' || c = '\x7F') s

(* 4.4: the doi: label and the proxy URLs are presentation forms, not
   part of the name, and both are percent-encoded forms. *)
let presentation_prefixes =
  [ "https://doi.org/"; "http://doi.org/";
    "https://dx.doi.org/"; "http://dx.doi.org/"; "doi:" ]

let strip_presentation s =
  let starts p =
    String.length s >= String.length p
    && String.(equal (lowercase_ascii (sub s 0 (length p))) p)
  in
  match List.find_opt starts presentation_prefixes with
  | None -> s
  | Some p ->
    match Uriz.pct_decode
      (String.sub s (String.length p) (String.length s - String.length p)) with
    | This s -> s | Null -> "\000"

let of_string s =
  let name = String.lowercase_ascii (strip_presentation (String.trim s)) in
  if has_control name then
    Error "a DOI name cannot contain control characters"
  else
    match String.index_opt name '/' with
    | None -> Error "a DOI name is <prefix>/<suffix>, with no \"/\" present"
    | Some i ->
      let prefix = String.sub name 0 i in
      let suffix = String.sub name (i + 1) (String.length name - i - 1) in
      if suffix = "" then Error "the DOI suffix is empty"
      else if not (List.for_all is_digits (String.split_on_char '.' prefix))
      then
        Error
          (Printf.sprintf
             "the DOI prefix %S is not digit components separated by \".\""
             prefix)
      else if String.equal prefix "10" then
        Error
          "directory indicator \"10\" requires a registrant code (\"10.NNNN\")"
      else Ok { prefix; suffix }

let to_string t = t.prefix ^ "/" ^ t.suffix

let equal a b = String.equal a.prefix b.prefix && String.equal a.suffix b.suffix

let prefix t = t.prefix

let registrant t =
  match String.index_opt t.prefix '.' with
  | None -> None
  | Some i ->
    Some (String.sub t.prefix (i + 1) (String.length t.prefix - i - 1))

let suffix t = t.suffix

(* The percent-encoding of 4.7, whose kept set is RFC 3986 unreserved
   plus the sub-delims, less ",". "/" is not kept either, so that an
   encoded name is a single path segment. *)
let keep = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9'
  | '-' | '.' | '_' | '~'
  | '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ';' | '=' | ':' | '@' ->
    true
  | _ -> false

let encode s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
       if keep c then Buffer.add_char b c
       else Buffer.add_string b (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents b

let to_url t = "https://doi.org/" ^ encode t.prefix ^ "/" ^ encode t.suffix

let path t =
  String.concat "/"
    (List.map encode (String.split_on_char '/' (to_string t)))

let pp f t = Fmt.string f (to_string t)

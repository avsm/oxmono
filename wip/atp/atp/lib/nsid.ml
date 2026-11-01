(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = { raw : string; segments : string list }
type error = [ `Invalid_nsid of string ]

let pp_error ppf = function `Invalid_nsid s -> Fmt.pf ppf "invalid NSID: %S" s

(* Authority segment: alphanumeric + hyphen, can start with digit *)
let is_valid_authority_char c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || c = '-'

(* Name segment (last): alphanumeric only *)
let is_valid_name_char c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

(* First char of name must be a letter *)
let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

(* Validate an authority segment (not the first one) *)
let is_valid_authority_segment s =
  let len = String.length s in
  len >= 1 && len <= 63
  && String.for_all is_valid_authority_char s
  (* Cannot start with hyphen *)
  && s.[0] <> '-'
  &&
  (* Cannot end with hyphen *)
  s.[len - 1] <> '-'

(* Validate the first authority segment - stricter rules *)
let is_valid_first_segment s =
  let len = String.length s in
  len >= 1 && len <= 63
  && String.for_all is_valid_authority_char s
  (* Cannot start with hyphen or digit *)
  && s.[0] <> '-'
  && (not (s.[0] >= '0' && s.[0] <= '9'))
  &&
  (* Cannot end with hyphen *)
  s.[len - 1] <> '-'

(* Validate the name segment (last segment) *)
let is_valid_name_segment s =
  let len = String.length s in
  len >= 1 && len <= 63
  && String.for_all is_valid_name_char s
  && is_letter s.[0]

let is_valid s =
  let len = String.length s in
  (* Length check *)
  if len < 1 || len > 317 then false (* No leading/trailing dots or spaces *)
  else if s.[0] = '.' || s.[len - 1] = '.' then false
  else if s.[0] = ' ' || s.[len - 1] = ' ' then false
  else if String.exists (fun c -> c = ' ') s then false
  else
    let segments = String.split_on_char '.' s in
    let num_segments = List.length segments in
    (* Must have at least 3 segments *)
    if num_segments < 3 then false (* Check for empty segments (double dots) *)
    else if List.exists (fun seg -> String.length seg = 0) segments then false
    else
      match List.rev segments with
      | [] -> false
      | name :: authority_rev -> (
          (* Last segment must be a valid name *)
          is_valid_name_segment name
          &&
          (* Validate authority segments: first segment has stricter rules *)
          match List.rev authority_rev with
          | [] -> false (* Need at least one authority segment *)
          | first :: rest ->
              is_valid_first_segment first
              && List.for_all is_valid_authority_segment rest)

let of_string s =
  if is_valid s then
    let segments = String.split_on_char '.' s in
    Ok { raw = s; segments }
  else Error (`Invalid_nsid s)

let of_string_exn s =
  match of_string s with
  | Ok t -> t
  | Error e -> invalid_arg (Format.asprintf "%a" pp_error e)

let to_string t = t.raw

let authority t =
  match List.rev t.segments with
  | _ :: authority_rev -> String.concat "." (List.rev authority_rev)
  | [] -> ""

let name t = match List.rev t.segments with name :: _ -> name | [] -> ""
let segments t = t.segments
let equal a b = String.equal a.raw b.raw
let compare a b = String.compare a.raw b.raw
let pp ppf t = Fmt.string ppf t.raw

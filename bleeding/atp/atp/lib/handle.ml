(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = string
type error = [ `Invalid_handle of string ]

let pp_error ppf = function
  | `Invalid_handle s -> Fmt.pf ppf "invalid handle: %S" s

(* Check if a character is valid in a handle segment *)
let is_valid_char c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || c = '-'

(* Check if a string starts with a digit *)
let starts_with_digit s = String.length s > 0 && s.[0] >= '0' && s.[0] <= '9'

(* Validate a single segment *)
let is_valid_segment s =
  let len = String.length s in
  len >= 1 && len <= 63
  && String.for_all is_valid_char s
  && s.[0] <> '-'
  && s.[len - 1] <> '-'

let is_valid s =
  let len = String.length s in
  (* Length check *)
  if len < 1 || len > 253 then false (* No leading/trailing dots or spaces *)
  else if s.[0] = '.' || s.[len - 1] = '.' then false
  else if s.[0] = ' ' || s.[len - 1] = ' ' then false
  else
    let segments = String.split_on_char '.' s in
    (* Must have at least 2 segments *)
    if List.length segments < 2 then false
      (* Check for empty segments (double dots) *)
    else if List.exists (fun seg -> String.length seg = 0) segments then false
      (* All segments must be valid *)
    else if not (List.for_all is_valid_segment segments) then false
    (* TLD (last segment) cannot start with a digit *)
      else
      match List.rev segments with
      | [] -> false
      | tld :: _ -> not (starts_with_digit tld)

let of_string s = if is_valid s then Ok s else Error (`Invalid_handle s)

let of_string_exn s =
  match of_string s with
  | Ok t -> t
  | Error e -> invalid_arg (Format.asprintf "%a" pp_error e)

let to_string t = t
let equal = String.equal

(* Case-insensitive comparison for handles *)
let compare a b =
  String.compare (String.lowercase_ascii a) (String.lowercase_ascii b)

let pp ppf t = Fmt.string ppf t

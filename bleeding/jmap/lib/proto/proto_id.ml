(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = string

let is_valid_char c =
  (c >= 'A' && c <= 'Z')
  || (c >= 'a' && c <= 'z')
  || (c >= '0' && c <= '9')
  || c = '_' || c = '-'

let check ~offset s =
  let len = String.length s in
  if len = 0 then Error "Id cannot be empty"
  else if len > 255 then Error "Id cannot exceed 255 octets"
  else
    let rec loop i =
      if i >= len then Ok ()
      else if is_valid_char s.[i] then loop (i + 1)
      else
        Error
          (Printf.sprintf "Invalid character %C in Id at position %d" s.[i]
             (offset + i))
    in
    loop 0

let of_string s = Result.map (fun () -> s) (check ~offset:0 s)

(* Section 1.2 restricts an Id to the base64url alphabet, but that is a rule
   for the server that assigns the id, and a server that breaks it is not
   thereby unreadable. Cyrus sets the id of a ContactCard to the card's uid,
   which is a URN and holds colons, so a client that refuses such an id cannot
   read a ContactCard/get response at all. Decoding therefore takes any string
   of 1 to 255 octets and rejects only a leading "#", which Section 5.3 gives
   to creation references. Constructing an id stays strict: of_string is what a
   caller reaches for and it enforces the alphabet. *)
let of_string_received s =
  let len = String.length s in
  if len = 0 then Error "Id cannot be empty"
  else if len > 255 then Error "Id cannot exceed 255 octets"
  else if s.[0] = '#' then
    Error "Id cannot start with '#', which marks a creation reference"
  else Ok s

let of_string_exn s =
  match of_string s with Ok id -> id | Error msg -> invalid_arg msg

let creation_id_error msg = Printf.sprintf "Invalid creation id: %s" msg

let of_creation_id s =
  match of_string s with
  | Ok id -> Ok ("#" ^ id)
  | Error msg -> Error (creation_id_error msg)

let of_creation_id_exn s =
  match of_creation_id s with Ok id -> id | Error msg -> invalid_arg msg

(* RFC 8620 Section 5.3: a client argument that names a record may name one
   being created in the same request, as "#" followed by its creation id. *)
let of_string_or_creation s =
  if String.equal s "#" then
    Error "a creation reference must be \"#\" followed by a creation id"
  else if String.length s > 1 && s.[0] = '#' then
    match check ~offset:1 (String.sub s 1 (String.length s - 1)) with
    | Ok () -> Ok s
    | Error msg -> Error (creation_id_error msg)
  else of_string s

let of_string_or_creation_received s =
  if String.length s > 0 && s.[0] = '#' then of_string_or_creation s
  else of_string_received s

let is_creation_ref t = String.length t > 0 && t.[0] = '#'

let to_creation_id t =
  if is_creation_ref t then Some (String.sub t 1 (String.length t - 1))
  else None

type 'a creation = t

let creation cid =
  match check ~offset:0 cid with
  | Ok () -> cid
  | Error msg -> invalid_arg (creation_id_error msg)

let creation_id c = c
let creation_ref c = "#" ^ c
let to_string t = t
let equal = String.equal
let compare = String.compare
let pp ppf t = Format.pp_print_string ppf t
let pp_creation ppf c = pp ppf (creation_id c)
let jsont = Jsont.of_of_string ~kind:"Id" ~enc:to_string of_string_received

let jsont_or_creation =
  Jsont.of_of_string ~kind:"Id or creation reference" ~enc:to_string
    of_string_or_creation_received

(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = Ptime.date

let is_valid (y, m, d) =
  m >= 1 && m <= 12 && d >= 1
  && Ptime.of_date (y, m, d) <> None

(* ISO 8601 fields are fixed-width decimal digits: a bare [int_of_string_opt]
   would also accept hex, octal, binary, sign prefixes and [_] separators. *)
let int_of_digits ~width s =
  let is_digit c = c >= '0' && c <= '9' in
  if String.length s = width && String.for_all is_digit s then
    int_of_string_opt s
  else None

let parse s =
  let candidate =
    match String.split_on_char '-' s with
    | [ y ] ->
        (match int_of_digits ~width:4 y with
         | Some y -> Some (y, 1, 1)
         | None -> None)
    | [ y; m ] ->
        (match (int_of_digits ~width:4 y, int_of_digits ~width:2 m) with
         | Some y, Some m -> Some (y, m, 1)
         | _ -> None)
    | [ y; m; d ] ->
        (match
           (int_of_digits ~width:4 y, int_of_digits ~width:2 m,
            int_of_digits ~width:2 d)
         with
         | Some y, Some m, Some d -> Some (y, m, d)
         | _ -> None)
    | _ -> None
  in
  match candidate with
  | Some date when is_valid date -> Some date
  | _ -> None

let to_string (y, m, d) = Printf.sprintf "%04d-%02d-%02d" y m d

let compare = Stdlib.compare

let json_t =
  let dec meta s =
    match parse s with
    | Some d -> d
    | None -> Jsont.Error.msgf meta "Date: not an ISO 8601 date: %S" s
  in
  Jsont.Base.string (Jsont.Base.map ~kind:"Date" ~dec ~enc:to_string ())

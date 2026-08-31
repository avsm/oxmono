(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t =
  [ `Slug of string
  | `Contact of string
  | `Set of string
  | `Text of string
  | `Year of int
  ]

let of_string s : t =
  if String.length s < 2 then invalid_arg ("Tag.of_string: " ^ s);
  match s.[0] with
  | ':' ->
    let slug = String.sub s 1 (String.length s - 1) in
    `Slug slug
  | '@' ->
    let handle = String.sub s 1 (String.length s - 1) in
    `Contact handle
  | '#' ->
    let cl = String.sub s 1 (String.length s - 1) in
    `Set cl
  | _ ->
    (try
       let x = int_of_string s in
       if x > 1900 && x < 2100 then `Year x else `Text s
     with _ -> `Text s)

let of_string_list l = List.map of_string l

let to_string = function
  | `Slug t -> ":" ^ t
  | `Contact c -> "@" ^ c
  | `Set s -> "#" ^ s
  | `Text t -> t
  | `Year y -> string_of_int y

let to_raw_string = function
  | `Slug t -> t
  | `Contact c -> c
  | `Set s -> s
  | `Text t -> t
  | `Year y -> string_of_int y

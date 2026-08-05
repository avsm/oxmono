(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Tag system for Bushel entries *)

type t =
  [ `Slug of string    (** :foo points to the specific slug foo *)
  | `Contact of string (** \@foo points to contact foo *)
  | `Set of string     (** #papers points to all Paper entries *)
  | `Text of string    (** foo points to a free text "foo" *)
  | `Year of int       (** a number between 1900--2100 is interpreted as a year *)
  ]

(** {1 Predicates} *)

let is_text = function `Text _ -> true | _ -> false
let is_slug = function `Slug _ -> true | _ -> false
let is_contact = function `Contact _ -> true | _ -> false
let is_set = function `Set _ -> true | _ -> false
let is_year = function `Year _ -> true | _ -> false

(** {1 Parsing} *)

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

(** {1 Serialization} *)

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

(** {1 Pretty Printing} *)

let pp ppf t = Fmt.string ppf (to_string t)

(** {1 Tag Filtering} *)

let mentions tags =
  List.filter (function
    | `Contact _ | `Slug _ -> true
    | _ -> false
  ) tags

(** {1 Tag Counting} *)

let count_tags ?h fn vs =
  let h = match h with
    | Some h -> h
    | None -> Hashtbl.create 42
  in
  List.iter (fun ent ->
    List.iter (fun tag ->
      match Hashtbl.find_opt h tag with
      | Some num -> Hashtbl.replace h tag (num + 1)
      | None -> Hashtbl.add h tag 1
    ) (fn ent)
  ) vs;
  h

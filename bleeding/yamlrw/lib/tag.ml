(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** YAML tags for type information *)

type t = {
  handle : string;  (** e.g., "!" or "!!" or "!foo!" *)
  suffix : string;  (** e.g., "str", "int", "custom/type" *)
}

let make ~handle ~suffix = { handle; suffix }

let of_string s =
  let len = String.length s in
  match len with
  | 0 -> None
  | _ when s.[0] <> '!' -> None
  | 1 -> Some { handle = "!"; suffix = "" }
  | _ -> (
      match s.[1] with
      | '!' ->
          (* !! handle *)
          Some { handle = "!!"; suffix = String.sub s 2 (len - 2) }
      | '<' ->
          (* Verbatim tag !<...> *)
          if len > 2 && s.[len - 1] = '>' then
            Some { handle = "!"; suffix = String.sub s 2 (len - 3) }
          else None
      | _ ->
          (* Primary handle or local tag *)
          Some { handle = "!"; suffix = String.sub s 1 (len - 1) })

let to_string t =
  if t.handle = "!" && t.suffix = "" then "!" else t.handle ^ t.suffix

let to_uri t =
  match t.handle with
  | "!!" -> "tag:yaml.org,2002:" ^ t.suffix
  | "!" -> "!" ^ t.suffix
  | h -> h ^ t.suffix

let pp fmt t = Format.pp_print_string fmt (to_string t)
let equal a b = String.equal a.handle b.handle && String.equal a.suffix b.suffix

let compare a b =
  let c = String.compare a.handle b.handle in
  if c <> 0 then c else String.compare a.suffix b.suffix

(** Standard tags *)

let null = { handle = "!!"; suffix = "null" }
let bool = { handle = "!!"; suffix = "bool" }
let int = { handle = "!!"; suffix = "int" }
let float = { handle = "!!"; suffix = "float" }
let str = { handle = "!!"; suffix = "str" }
let seq = { handle = "!!"; suffix = "seq" }
let map = { handle = "!!"; suffix = "map" }
let binary = { handle = "!!"; suffix = "binary" }
let timestamp = { handle = "!!"; suffix = "timestamp" }

(** Check if tag matches a standard type *)

let is_null t = equal t null || (t.handle = "!" && t.suffix = "")
let is_bool t = equal t bool
let is_int t = equal t int
let is_float t = equal t float
let is_str t = equal t str
let is_seq t = equal t seq
let is_map t = equal t map

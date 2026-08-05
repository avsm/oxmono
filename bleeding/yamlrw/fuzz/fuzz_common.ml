(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Common utilities for fuzz tests. *)

open Crowbar

let to_bytes buf =
  let len = String.length buf in
  let b = Bytes.create len in
  Bytes.blit_string buf 0 b 0 len;
  b

(** Generator for printable ASCII strings (useful for YAML content) *)
let printable_char = map [ range 95 ] (fun n -> Char.chr (n + 32))

let printable_string =
  map [ list printable_char ] (fun chars ->
      String.init (List.length chars) (List.nth chars))

(** Generator for valid YAML scalar content (excludes problematic chars) *)
let yaml_safe_char =
  map [ range 94 ] (fun n ->
      let c = n + 32 in
      (* Skip colon, hash, and other YAML special chars at start *)
      if c = 58 (* : *) || c = 35 (* # *) then Char.chr 97 (* 'a' *)
      else Char.chr c)

let yaml_safe_string =
  map [ list yaml_safe_char ] (fun chars ->
      String.init (List.length chars) (List.nth chars))

(** Generator for identifier-like strings *)
let ident_char =
  map [ range 62 ] (fun n ->
      if n < 26 then Char.chr (n + 97) (* a-z *)
      else if n < 52 then Char.chr (n - 26 + 65) (* A-Z *)
      else if n < 62 then Char.chr (n - 52 + 48) (* 0-9 *)
      else '_')

let ident_string =
  map [ list1 ident_char ] (fun chars ->
      String.init (List.length chars) (List.nth chars))

(** Catch exceptions and pass the test if expected exception occurs *)
let catch_invalid_arg f =
  try f () with Invalid_argument _ -> check true

let catch_yamlrw_error f =
  try f ()
  with Yamlrw.Yamlrw_error _ -> check true

let run () = ()

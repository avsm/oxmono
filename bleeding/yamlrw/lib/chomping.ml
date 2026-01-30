(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Block scalar chomping indicators *)

type t =
  | Strip  (** Remove final line break and trailing empty lines *)
  | Clip  (** Keep final line break, remove trailing empty lines (default) *)
  | Keep  (** Keep final line break and trailing empty lines *)

let to_string = function Strip -> "strip" | Clip -> "clip" | Keep -> "keep"
let pp fmt t = Format.pp_print_string fmt (to_string t)
let of_char = function '-' -> Some Strip | '+' -> Some Keep | _ -> None
let to_char = function Strip -> Some '-' | Clip -> None | Keep -> Some '+'
let equal a b = a = b

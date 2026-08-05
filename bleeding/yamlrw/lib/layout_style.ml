(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Collection layout styles *)

type t =
  [ `Any  (** Let emitter choose *)
  | `Block  (** Indentation-based *)
  | `Flow  (** Inline with brackets *) ]

let to_string = function `Any -> "any" | `Block -> "block" | `Flow -> "flow"
let pp fmt t = Format.pp_print_string fmt (to_string t)
let equal a b = a = b

let compare a b =
  let to_int = function `Any -> 0 | `Block -> 1 | `Flow -> 2 in
  Int.compare (to_int a) (to_int b)

(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Scalar formatting styles *)

type t =
  [ `Any  (** Let emitter choose *)
  | `Plain  (** Unquoted: foo *)
  | `Single_quoted  (** 'foo' *)
  | `Double_quoted  (** "foo" *)
  | `Literal  (** | block *)
  | `Folded  (** > block *) ]

let to_string = function
  | `Any -> "any"
  | `Plain -> "plain"
  | `Single_quoted -> "single-quoted"
  | `Double_quoted -> "double-quoted"
  | `Literal -> "literal"
  | `Folded -> "folded"

let pp fmt t = Format.pp_print_string fmt (to_string t)
let equal a b = a = b

let compare a b =
  let to_int = function
    | `Any -> 0
    | `Plain -> 1
    | `Single_quoted -> 2
    | `Double_quoted -> 3
    | `Literal -> 4
    | `Folded -> 5
  in
  Int.compare (to_int a) (to_int b)

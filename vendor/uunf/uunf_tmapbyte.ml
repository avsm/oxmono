(*---------------------------------------------------------------------------
   Copyright (c) 2012 The uunf programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Trie character byte maps *)

type t =
  { default : int;                                     (* default value. *)
    l0 : string iarray iarray }          (* 0x1FFFFF as 0x1FF - 0xF - 0xFF *)

let nil = [::]
let snil = ""
let l0_shift = 12
let l0_size = 272 (* 0x10F + 1 *)
let l1_shift = 8
let l1_mask = 0xF
let l1_size = 16 (* 0xF + 1 *)
let l2_mask = 0xFF
let l2_size = 256 (* 0xFF + 1 *)
let get m u =
  let l1 = Stdlib_stable.Iarray.get m.l0 (u lsr l0_shift) in
  if Stdlib_stable.Iarray.length l1 = 0 then m.default else
  let l2 = Stdlib_stable.Iarray.unsafe_get l1 (u lsr l1_shift land l1_mask) in
  if l2 == snil then m.default else
  Char.code (String.unsafe_get l2 (u land l2_mask))


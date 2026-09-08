(*---------------------------------------------------------------------------
   Copyright (c) 2012 The uunf programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Trie character maps *)

type 'a t =
  { default : 'a;                                      (* default value. *)
    l0 : 'a iarray iarray iarray }       (* 0x1FFFFF as 0x1FF - 0xFF - 0xF. *)

let nil = [::]
let l0_shift = 12
let l0_size = 272 (* 0x10F + 1 *)
let l1_shift = 4
let l1_mask = 0xFF
let l1_size = 256 (* 0xFF + 1 *)
let l2_mask = 0xF
let l2_size = 16  (* 0xF + 1 *)
let get m u =
  let l1 = Stdlib_stable.Iarray.unsafe_get m.l0 (u lsr l0_shift) in
  if Stdlib_stable.Iarray.length l1 = 0 then m.default else
  let l2 = Stdlib_stable.Iarray.unsafe_get l1 (u lsr l1_shift land l1_mask) in
  if Stdlib_stable.Iarray.length l2 = 0 then m.default else
  Stdlib_stable.Iarray.unsafe_get l2 (u land l2_mask)


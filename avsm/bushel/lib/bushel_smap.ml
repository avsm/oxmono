(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Build-once lookup tables keyed by string *)

(* The tree is declared here rather than reached for in the stdlib because a
   type has to carry a kind before a portable closure can read a captured
   value of it, and no stdlib container declares one. *)
type 'a t = Empty | Node of 'a t * string * 'a * 'a t

let empty = Empty

(* Split the sorted array at its midpoint, so the tree is as shallow as the
   number of bindings allows and no rebalancing code is needed. *)
let rec of_sorted_sub a lo hi =
  if lo > hi then Empty
  else
    let mid = lo + ((hi - lo) / 2) in
    let k, v = a.(mid) in
    Node (of_sorted_sub a lo (mid - 1), k, v, of_sorted_sub a (mid + 1) hi)

(* Collapse each run of equal keys to its last element. The sort is stable, so
   that is the binding that came last in the input list. *)
let last_of_each_run a =
  let n = Array.length a in
  if n = 0 then a
  else begin
    let out = Array.make n a.(0) in
    let j = ref 0 in
    for i = 1 to n - 1 do
      let k, _ = a.(i) in
      let k', _ = out.(!j) in
      if String.equal k k' then out.(!j) <- a.(i)
      else begin
        incr j;
        out.(!j) <- a.(i)
      end
    done;
    Array.sub out 0 (!j + 1)
  end

let of_list l =
  let a = Array.of_list l in
  Array.stable_sort (fun (k1, _) (k2, _) -> String.compare k1 k2) a;
  let a = last_of_each_run a in
  of_sorted_sub a 0 (Array.length a - 1)

let rec find_opt k = function
  | Empty -> None
  | Node (l, k', v, r) ->
    let c = String.compare k k' in
    if c = 0 then Some v else if c < 0 then find_opt k l else find_opt k r

let rec find k = function
  | Empty -> raise Not_found
  | Node (l, k', v, r) ->
    let c = String.compare k k' in
    if c = 0 then v else if c < 0 then find k l else find k r

let bindings t =
  let rec go t acc =
    match t with
    | Empty -> acc
    | Node (l, k, v, r) -> go l ((k, v) :: go r acc)
  in
  go t []

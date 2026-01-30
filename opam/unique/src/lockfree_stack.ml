open Base
open Portable_kernel
module Backoff = Basement.Stdlib_shim.Backoff

(* SAFETY: This data structure must not expose operations that would duplicate elems. *)

type 'a t = 'a list Atomic.t

let create ?padded () = Atomic.make ?padded []

let rec push t x backoff =
  let xs = Atomic.get t in
  let xxs = x :: xs in
  match Atomic.compare_and_set t ~if_phys_equal_to:xs ~replace_with:xxs with
  | Set_here -> ()
  | Compare_failed -> push t x (Backoff.once backoff)
;;

let rec pop_or_null t backoff =
  let xxs = Atomic.get t in
  match xxs with
  | [] -> Null
  | x :: xs ->
    (match Atomic.compare_and_set t ~if_phys_equal_to:xxs ~replace_with:xs with
     | Set_here -> This x
     | Compare_failed -> pop_or_null t (Backoff.once backoff))
;;

let[@inline] exchange t xs = Atomic.exchange t xs

let[@inline] push t x =
  push t ((Obj.magic_many [@mode contended portable]) x) Backoff.default
;;

let[@inline] pop_or_null t =
  (Obj.magic_unique [@mode contended portable]) (pop_or_null t Backoff.default)
;;

let[@inline] is_empty t = phys_equal [] (Atomic.get t)

let[@inline] exchange t xs =
  (Obj.magic_unique [@mode contended portable])
    (exchange t ((Obj.magic_many [@mode contended portable]) xs))
;;

let[@inline] pop_all t = exchange t []

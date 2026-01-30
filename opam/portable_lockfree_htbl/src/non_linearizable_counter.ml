open Base
open Portable_kernel

(* [add] and [remove] operations on a concurrent hash table can be fast enough that using
   a single atomic location for keeping track of the length of the hash table quickly
   becomes a bottleneck.

   This is a simple scalable accumulator that maintains an atomic location per domain.
   Sharing of atomic locations is used to allow widening the accumulator. *)

(* NOTE: we intentionally don't use [Atomic_array] here, as we want each atomic integer to
   be alone on a cache line. *)
type t = int Atomic.t Iarray.t

let zero_of ~width = Iarray.init width ~f:(fun _ -> Atomic.make ~padded:true 0)

let[@inline] zero ~width_of:t =
  let width = Iarray.length t in
  zero_of ~width
;;

let create value =
  let width =
    Bits.ceil_pow_2_minus_1 (Multicore.current_domain () lor 1)
    (* This potentially adjusts the counter width for the number of domains. *)
  in
  let t = zero_of ~width in
  Atomic.add (Iarray.unsafe_get t 0) value;
  t
;;

let[@inline] get t =
  let accum = ref 0 in
  for i = Iarray.length t - 1 downto 0 do
    (* [fenceless_get] is fine here, because we are not even trying to get a precise
       linearizable count, i.e. it is fine to read past values. *)
    accum := !accum + Atomic.Expert.fenceless_get (Iarray.unsafe_get t i)
  done;
  !accum
;;

let[@inline] add_or_resize t delta =
  let i = Multicore.current_domain () in
  let n = Iarray.length t in
  if i < n
  then (
    Atomic.add (Iarray.unsafe_get t i) delta;
    Null)
  else (
    let new_cs =
      (* We use [n + n + 1] as it keeps the length of the array as a power of 2 minus 1
         and so the size of the array/block including header word will be a power of 2. *)
      (Iarray.init (n + n + 1)) ~f:(fun i ->
        if i < n then Iarray.unsafe_get t i else Atomic.make ~padded:true 0)
    in
    This new_cs)
;;

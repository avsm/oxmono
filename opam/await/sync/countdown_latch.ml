open Base
open Basement
open Await_kernel
open Await_sync_intf

module State : sig
  type t : immediate

  val max_count : int

  (* *)

  val from_count : int -> t

  (* *)

  val count : t -> int
  val is_poisoned : t -> bool
  val is_0_or_less : t -> bool
  val is_1_or_less : t -> bool

  (* *)

  val and_poisoned : t -> t
  val and_incr : t -> t

  (* *)

  val fetch_and_add : t Awaitable.t @ local -> int -> t
end = struct
  type t = int

  let max_count = 1 lsl (Int.num_bits - 2)
  let poisoned_bit = 1
  let count_shift = 1
  let from_count n = n lsl count_shift
  let count t = if t < 0 then 0 else t lsr count_shift
  let is_poisoned t = t land poisoned_bit <> 0
  let and_poisoned t = t lor poisoned_bit
  let is_0_or_less t = t <= (0 lsl count_shift) lor poisoned_bit
  let is_1_or_less t = t <= (1 lsl count_shift) lor poisoned_bit
  let and_incr t = t + (1 lsl count_shift)
  let fetch_and_add t d = Awaitable.fetch_and_add t (d lsl count_shift)
end

type t = State.t Awaitable.t

let sexp_of_t t =
  let s = Awaitable.get t in
  let c = State.count s in
  [%sexp
    (if State.is_poisoned s then `poisoned c else `count c
     : [ `poisoned of int | `count of int ])]
;;

let max_count = State.max_count

let create ?padded count =
  if count < 0 || max_count < count
  then invalid_arg "Countdown_latch.create: invalid initial count"
  else Awaitable.make ?padded (State.from_count count)
;;

let rec poison t =
  let before = Awaitable.get t in
  if not (State.is_poisoned before)
  then (
    let after = State.and_poisoned before in
    match Awaitable.compare_and_set t ~if_phys_equal_to:before ~replace_with:after with
    | Set_here -> Awaitable.broadcast t
    | Compare_failed -> poison t)
;;

let is_poisoned t = State.is_poisoned (Awaitable.get t)

let[@inline never] already_reached_0 t d =
  let _ : _ = State.fetch_and_add t d in
  invalid_arg "Countdown_latch.decr: already reached zero"
;;

let incr t =
  let[@inline] rec loop t backoff =
    let before = Awaitable.get t in
    if State.is_poisoned before
    then raise Poisoned
    else if State.is_0_or_less before
    then already_reached_0 t 0
    else (
      let after = State.and_incr before in
      match Awaitable.compare_and_set t ~if_phys_equal_to:before ~replace_with:after with
      | Set_here -> ()
      | Compare_failed -> loop t (Backoff.once backoff))
  in
  loop t Backoff.default
;;

let decr t =
  let before_decr = State.fetch_and_add t (-1) in
  if State.is_1_or_less before_decr
  then
    if State.is_0_or_less before_decr
    then already_reached_0 t 1
    else Awaitable.broadcast t
;;

let rec await w t =
  let state = Awaitable.get t in
  if State.is_poisoned state
  then raise Poisoned
  else if not (State.is_0_or_less state)
  then (
    match Awaitable.await w t ~until_phys_unequal_to:state with
    | Signaled -> await w t
    | Terminated -> raise Await.Terminated)
;;

let rec await_or_cancel w c t : _ Or_canceled.t =
  let state = Awaitable.get t in
  if State.is_poisoned state
  then raise Poisoned
  else if State.is_0_or_less state
  then Completed ()
  else (
    match Awaitable.await_or_cancel w c t ~until_phys_unequal_to:state with
    | Signaled -> await_or_cancel w c t
    | Terminated -> raise Await.Terminated
    | Canceled -> Canceled)
;;

let count t = State.count (Awaitable.get t)

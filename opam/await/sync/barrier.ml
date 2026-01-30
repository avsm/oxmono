open Base
open Await_kernel
open Await_sync_intf

module State : sig @@ portable
  type t : immediate

  val max_parties : int

  (* *)

  val from_parties : int -> t

  (* *)

  val decr_awaiting : t Awaitable.t @ local -> t

  (* *)

  val is_awaiting_one_or_more : t -> bool
  val is_awaiting_none : t -> bool
  val is_poisoned : t -> bool
  val parties : t -> int

  (* *)

  module Sense : sig
    type t : immediate

    val equal : t -> t -> bool
  end

  (** [sense_of t] masks out all the bits of [t] except the sense bit. The sense bit is a
      flip-flop changed whenever threads awaiting on the barrier need to wake up. *)
  val sense_of : t -> Sense.t

  (* *)

  val and_decr_awaiting : t -> t
  val and_incr_awaiting : t -> t
  val and_poison : t -> t
  val and_reset : t -> t
end = struct
  type t = int

  (* The state looks like this:

     Bit: [   0   | 1 to n/2-1 | n/2 to n-2 |    n-1   ] Use:
     [ sense |  parties   |  awaiting  | poisoned ]

     [awaiting] is the number of [parties] still being awaited for by or to arrive at the
     barrier. It starts equal to [parties], then counts down to 0.

     [sense] is a flip-flop whose state is changed whenever threads awaiting on the
     barrier need to wake up. *)

  let sense_bit = 1
  let parties_shift = 1
  let parties_bits = (Int.num_bits - 2) / 2
  let max_parties = (1 lsl parties_bits) - 1
  let parties_mask = max_parties lsl parties_shift
  let awaiting_shift = parties_shift + parties_bits
  let awaiting_one = 1 lsl awaiting_shift
  let poisoned_bit = Int.min_value

  (* *)

  let[@inline] is_poisoned state = state <= 0

  (* Are we waiting for at least one more party to arrive? *)
  let[@inline] is_awaiting_one_or_more state =
    (* This will correctly return [false] if the barrier has been poisoned. *)
    awaiting_one < state
  ;;

  let[@inline] is_awaiting_none state =
    (* This will incorrectly return [true] if the barrier has been poisoned and should not
       be used unless it is known that the barrier has not been poisoned. *)
    state < awaiting_one
  ;;

  module Sense = struct
    type t = int

    let equal = ( = )
  end

  let[@inline] sense_of state = state land sense_bit
  let[@inline] parties state = (state land parties_mask) lsr parties_shift

  (* *)

  let[@inline] from_parties parties =
    (parties lsl parties_shift)
    lor ((* Start at awaiting = parties, then count down *)
         parties lsl awaiting_shift)
  ;;

  (* *)

  let[@inline] decr_awaiting t = Awaitable.fetch_and_add t (-awaiting_one)

  (* *)

  let[@inline] and_decr_awaiting state = state - awaiting_one
  let[@inline] and_incr_awaiting state = state + awaiting_one

  let[@inline] and_poison state =
    let after_sense = sense_of (lnot state) in
    let parties_shifted = state land parties_mask in
    let awaiting_shifted =
      (* We use [max_parties] to make it practically impossible to break the poisoning by
         using lots of concurrent/parallel threads to await causing an underflow due to
         the optimistic [fetch_and_add] at the start of await. *)
      max_parties lsl awaiting_shift
    in
    after_sense lor parties_shifted lor awaiting_shifted lor poisoned_bit
  ;;

  let[@inline] and_reset state =
    let after_sense = sense_of (lnot state) in
    let parties_shifted = state land parties_mask in
    let awaiting_shifted = parties_shifted lsl (awaiting_shift - parties_shift) in
    after_sense lor parties_shifted lor awaiting_shifted
  ;;
end

type t = State.t Awaitable.t

let max_parties = State.max_parties

let create ?padded parties =
  if 0 < parties && parties <= max_parties
  then Awaitable.make ?padded (State.from_parties parties)
  else invalid_arg "Barrier.create: invalid number of parties"
;;

let[@inline never] rec poison t =
  let before = Awaitable.get t in
  let after = before |> State.and_poison in
  if not (phys_equal before after)
  then (
    match Awaitable.compare_and_set t ~if_phys_equal_to:before ~replace_with:after with
    | Set_here -> Awaitable.broadcast t
    | Compare_failed -> poison t)
;;

type ('a, _) result =
  | Value : ('a, 'a) result
  | Canceled : ('a, 'a Or_canceled.t) result

let await_as (type r) ~awt ~ct t (r : (unit, r) result) : r =
  let[@inline] completed () : r =
    match r with
    | Value -> ()
    | Canceled -> Completed ()
  in
  (* The main idea of this barrier algorithm is to optimistically use [fetch_and_add].

     All threads except the last one to arrive perform only a single update of the atomic
     barrier state. This should minimize contention. (A tree based barrier would have even
     less contention, but would take more memory.)

     The last thread to arrive does another update to reset the barrier state, which also
     reverse the "sense" of the barrier, and signals, or broadcasts, to wake up all the
     awaiting threads. *)
  let before_decr = State.decr_awaiting t in
  if State.is_awaiting_one_or_more before_decr
  then (
    let after_decr = before_decr |> State.and_decr_awaiting in
    if State.is_awaiting_none after_decr
    then (
      (* We were the last to arrive and must wake up others. *)
      let after_decr_and_reset = after_decr |> State.and_reset in
      match
        Awaitable.compare_and_set
          t
          ~if_phys_equal_to:after_decr
          ~replace_with:after_decr_and_reset
      with
      | Set_here ->
        Awaitable.broadcast t;
        completed ()
      | Compare_failed ->
        (* We allow cancellation to happen only before everyone has arrived and the last
           to arrive, which is us right here, is responsible for waking up others.
           [Compare_failed] indicates that either some external party poisoned the barrier
           or that the barrier is being misused such as having more threads await on the
           barrier than the configured number of [parties]. *)
        poison t;
        raise Poisoned)
    else (
      (* We need to await for others to arrive. *)
      let sense_before = State.sense_of after_decr in
      let rec await_others_to_arrive ~awt ~ct t state_before : r =
        if State.Sense.equal (State.sense_of state_before) sense_before
        then (
          match r with
          | Value ->
            (match Awaitable.await awt t ~until_phys_unequal_to:state_before with
             | Signaled -> await_others_to_arrive ~awt ~ct t (Awaitable.get t)
             | Terminated ->
               poison t;
               raise Await.Terminated)
          | Canceled ->
            (match
               Awaitable.await_or_cancel awt ct t ~until_phys_unequal_to:state_before
             with
             | Signaled -> await_others_to_arrive ~awt ~ct t (Awaitable.get t)
             | Canceled ->
               let rec try_to_cancel t =
                 let before = Awaitable.get t in
                 if State.Sense.equal (State.sense_of before) sense_before
                    && State.is_awaiting_one_or_more before
                 then (
                   (* The barrier has neither switched sense nor have all parties arrived,
                      which means we may be able to cancel the await. *)
                   let after = before |> State.and_incr_awaiting in
                   match
                     Awaitable.compare_and_set
                       t
                       ~if_phys_equal_to:before
                       ~replace_with:after
                   with
                   | Set_here -> (Canceled : r)
                   | Compare_failed -> try_to_cancel t)
                 else if not (State.is_poisoned before)
                 then Completed ()
                 else raise Poisoned
               in
               try_to_cancel t
             | Terminated ->
               poison t;
               raise Await.Terminated))
        else if not (State.is_poisoned state_before)
        then completed ()
        else raise Poisoned
      in
      await_others_to_arrive ~awt ~ct t after_decr))
  else (
    (* The barrier is poisoned or is being misused. *)
    poison t;
    raise Poisoned)
;;

let await awt t = await_as ~awt ~ct:Cancellation.never t Value
let await_or_cancel awt ct t = await_as ~awt ~ct t Canceled
let parties t = State.parties (Awaitable.get t)

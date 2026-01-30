open Base
open Basement
open Await_kernel
open Await_sync_intf

module Acquired_or_would_block = struct
  type t =
    | Acquired
    | Would_block
  [@@deriving sexp_of]
end

module State : sig @@ portable
  (** Implements primitive semaphore state manipulation logic.

      The state has three main features:

      - a signed [value] that is normally normalized to be non-negative and also has a
        (very large) [max_value],

      - a (very small) negative threshold value that indicates that the semaphore has been
        poisoned,

      - a bit that indicates whether or not there are potentially threads awaiting to
        acquire the semaphore.

      State values are allowed to be momentarily imprecise:

      - The value is allowed to be negative, to be (relatively speaking) slightly greater
        than the (very large) [max_value], and also to be (relatively speaking) slightly
        less than or greater than the (very small) negative threshold for a poisoned
        semaphore.

      - The bit that indicates whether or not there are potentially awaiters is allowed to
        momentarily say that there are no awaiters when, in fact, there are awaiters and
        also the opposite that there are awaiters when, in fact, there is none.

      This can be used to optimize both [acquire] and [release] operations:

      - Both [acquire] and [release] optimistically use [fetch_and_add]. This means that
        in the common uncontended case they only perform a single atomic update of the
        state without needing to first read the state.

      - Although awaiters form a FIFO, active [acquire] callers are allowed to bypass the
        FIFO. This avoids the lock convoy performance problem under heavy contention.

      To make this possible, state changes are expressed as incremental updates to current
      state that retain information from the previous state and actively contending
      operation work cooperatively on the state:

      - Both [acquire] and [release] may need to undo their decrement or increment,
        respectively, of the [value].

      - Awoken awaiters must actively re-establish that some thread might be awaiting. *)

  type t : immediate

  val max_value : int

  (* *)

  (** [from_value ~n] is a state value whose [value] is [n] and that indicates that there
      are no uncontended awaiters. *)
  val from_value : n:int -> t

  (** [poisoned] is a state value constant indicating that the semaphore has been poisoned
      and that there are no uncontended awaiters. *)
  val poisoned : t

  (* *)

  val value : t -> int

  (* *)

  val incr_value : t Awaitable.t @ local -> t
  val decr_value : t Awaitable.t @ local -> t

  (* *)

  val is_poisoned : t -> bool
  val has_value_zero_or_higher : t -> bool
  val has_value_zero_and_uncontended_awaiters : t -> bool
  val has_value_one_or_higher : t -> bool
  val has_max_value_or_higher : t -> bool
  val has_uncontended_awaiters : t -> bool

  (* *)

  val and_has_uncontended_awaiters : t -> t
  val and_has_no_uncontended_awaiters : t -> t
  val and_incr_value : t -> t
  val and_decr_value : t -> t
end = struct
  (* The state looks like this:

     Bit: [      0      |   1  to  n-1   ] Use: [ no_awaiters | (signed) value ]

     The value is treated as signed, but operations will normalize it to be non-negative
     unless the value is less than [-poisoned_abs / 2], which is taken to mean that the
     semaphore is poisoned. *)

  type t = int [@@deriving sexp_of]

  let no_need_to_signal = 1 lsl 0
  let one_shift = 1
  let one = 1 lsl one_shift
  let poisoned_abs = (Int.max_value / 2) + 1
  let max_value = ((Int.max_value / 2) + 1) asr one_shift

  (* *)

  let[@inline] incr_value t = Awaitable.fetch_and_add t one
  let[@inline] decr_value t = Awaitable.fetch_and_add t (-one)

  (* *)

  let[@inline] is_poisoned state = state < -poisoned_abs / 2
  let[@inline] has_value_zero_or_higher state = -one + no_need_to_signal < state
  let[@inline] has_value_zero_and_uncontended_awaiters state = 0 <= state
  let[@inline] has_value_one_or_higher state = one <= state
  let[@inline] has_max_value_or_higher state = max_value * one <= state
  let[@inline] has_uncontended_awaiters state = state land no_need_to_signal = 0
  let[@inline] value state = state asr one_shift

  (* *)

  let[@inline] and_has_uncontended_awaiters state = state land lnot no_need_to_signal
  let[@inline] and_has_no_uncontended_awaiters state = state lor no_need_to_signal
  let[@inline] and_incr_value state = state + one
  let[@inline] and_decr_value state = state - one

  (* *)

  let[@inline] from_value ~n = n * one |> and_has_no_uncontended_awaiters
  let poisoned = -poisoned_abs lor no_need_to_signal
end

type t = State.t Awaitable.t

let max_value = State.max_value

let create ?padded n =
  if 0 <= n && n <= max_value
  then Awaitable.make ?padded (State.from_value ~n)
  else invalid_arg "Semaphore.create: invalid initial count"
;;

let release t =
  let prior = State.incr_value t in
  if not (State.is_poisoned prior || State.has_max_value_or_higher prior)
  then (
    if State.has_uncontended_awaiters prior
    then (
      let signal_awaiter t before =
        let _ : State.t =
          Awaitable.compare_exchange
            t
            ~if_phys_equal_to:before
            ~replace_with:(before |> State.and_has_no_uncontended_awaiters)
        in
        Awaitable.signal t
      in
      signal_awaiter t (prior |> State.and_incr_value)))
  else (
    (* In case of poisoning, the value should be sufficiently negative (small) that it is
       impossible to make it greater than the threshold used to determine that the
       semaphore is poisoned before we decrement the value back.

       In case of overflow, the count should be sufficiently large that it is impossible
       for an acquirer to take the extra resource before we decrement the value back. *)
    let undo_release t =
      let prior = State.decr_value t in
      if State.has_max_value_or_higher prior
      then raise (Sys_error "Semaphore.release: overflow")
      else ( (* Semaphore has been poisoned -- nop *) )
    in
    undo_release t)
;;

type ('a, _) result =
  | Value : ('a, 'a) result
  | Or_canceled : ('a, 'a Or_canceled.t) result

let acquire_as (type r) w c t (r : (unit, r) result) : r =
  let[@inline] completed () : r =
    match r with
    | Value -> ()
    | Or_canceled -> Completed ()
  in
  let prior = State.decr_value t in
  if State.has_value_one_or_higher prior
  then completed ()
  else (
    let rec acquire_awaiting w c t backoff before =
      if State.has_value_one_or_higher before
      then (
        let after =
          before |> State.and_decr_value |> State.and_has_uncontended_awaiters
        in
        match
          Awaitable.compare_and_set t ~if_phys_equal_to:before ~replace_with:after
        with
        | Set_here ->
          if State.has_value_one_or_higher after then Awaitable.signal t;
          completed ()
        | Compare_failed ->
          let backoff = Backoff.once backoff in
          acquire_awaiting w c t backoff (Awaitable.get t))
      else if State.is_poisoned before
      then raise Poisoned
      else (
        let after = before |> State.and_has_uncontended_awaiters in
        if phys_equal before after
           ||
           match
             Awaitable.compare_and_set t ~if_phys_equal_to:before ~replace_with:after
           with
           | Set_here -> true
           | Compare_failed -> false
        then (
          match r with
          | Value ->
            (match Awaitable.await w t ~until_phys_unequal_to:after with
             | Signaled -> acquire_awaiting w c t Backoff.default (Awaitable.get t)
             | Terminated -> raise Await.Terminated)
          | Or_canceled ->
            (match Awaitable.await_or_cancel w c t ~until_phys_unequal_to:after with
             | Signaled -> acquire_awaiting w c t Backoff.default (Awaitable.get t)
             | Terminated -> raise Await.Terminated
             | Canceled -> Or_canceled.Canceled))
        else (
          let backoff = Backoff.once backoff in
          acquire_awaiting w c t backoff (Awaitable.get t)))
    in
    let rec acquire_contended w c t backoff before =
      if State.has_value_zero_or_higher before
      then completed ()
      else (
        let after =
          before |> State.and_incr_value |> State.and_has_uncontended_awaiters
        in
        match
          Awaitable.compare_and_set t ~if_phys_equal_to:before ~replace_with:after
        with
        | Set_here -> acquire_awaiting w c t Backoff.default after
        | Compare_failed ->
          let backoff = Backoff.once backoff in
          acquire_contended w c t backoff (Awaitable.get t))
    in
    acquire_contended w c t Backoff.default (prior |> State.and_decr_value))
;;

let acquire w t = acquire_as w Cancellation.never t Value
let acquire_or_cancel w c t = acquire_as w c t Or_canceled

let try_acquire t =
  let prior = State.decr_value t in
  if State.has_value_one_or_higher prior
  then Acquired_or_would_block.Acquired
  else (
    let undo_acquire t =
      let prior = State.incr_value t in
      if State.is_poisoned prior
      then raise Poisoned
      else (
        if State.has_value_zero_and_uncontended_awaiters prior then Awaitable.signal t;
        Acquired_or_would_block.Would_block)
    in
    undo_acquire t)
;;

let rec poison t =
  let before = Awaitable.get t in
  if not (State.is_poisoned before)
  then (
    let after = State.poisoned in
    match Awaitable.compare_and_set t ~if_phys_equal_to:before ~replace_with:after with
    | Set_here -> Awaitable.broadcast t
    | Compare_failed ->
      (* Backoff is probably not worth the trouble. *)
      poison t)
;;

let get_value t =
  let n = State.value (Awaitable.get t) in
  (* We must clamp the value [n] as we are using [fetch_and_add], which can temporarily
     make the value be out of bounds. Poisoning also makes the value negative. *)
  if 0 <= n then if n < max_value then n else max_value else 0
;;

let%template sexp_of_t t = [%sexp { value = (get_value t : int) }]
[@@alloc a = (heap, stack)]
;;

let is_poisoned t = State.is_poisoned (Awaitable.get t)

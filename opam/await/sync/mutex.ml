open Base
open Basement
open Portable_kernel
open Await_kernel
open Await_sync_intf
module Capsule = Capsule.Expert

module Prim = struct
  module State : sig @@ portable
    (** Implements primitive mutex state manipulation logic.

        The state has three main features:

        - a [locked] count,

        - a (very large) threshold [locked] count that indicates that the mutex is locked
          permanently, i.e. poisoned, and

        - a bit that indicates whether a [release] operation should signal on the
          awaitable to wake up awaiters.

        State values are allowed to be momentarily imprecise:

        - The [locked] count can be momentarily higher than [1] or (relatively speaking)
          slightly higher than the (very large) threshold value of a permanently locked
          mutex.

        - The [locked] count of a permanently locked mutex may momentarily be (relatively
          speaking) slightly less than the (very large) threshold value of a permanently
          locked mutex.

        This can be and is used to optimize both the [acquire] and [release] operations:

        - Both [acquire] and [release] optimistically use [fetch_and_add]. This means that
          in the common uncontended case they only perform a single atomic update of the
          state without needing to first read the state.

        - Although awaiters form a FIFO, active [acquire] callers are allowed to bypass
          the FIFO. This avoids the lock convoy performance problem under heavy
          contention.

        - [release] does not wake up awaiters in case of contention as the state, i.e. the
          [locked] count being more than one, allows it to detect when there is active
          contention on the lock.

        To make this possible, state changes are expressed as incremental updates to
        current state that retain information from the previous state and actively
        contending operations work cooperatively on the state:

        - Both [acquire] and [release] may need to undo their increment or decrement,
          respectively, of the [locked] count.

        - The responsibility to signal awaiters, and update the state to indicate that
          there are no awaiters, is transferred to whichever thread is the last to release
          the mutex while there potentially are awaiters.

        - Awoken awaiters must actively re-establish that there might be further awaiters. *)

    type t : immediate

    (** [unlocked_and_no_need_to_signal] is a state value constant indicating that there
        are no awaiters on the mutex and that that the mutex is not locked. *)
    val unlocked_and_no_need_to_signal : t

    (** [unlocked_and_need_to_signal] is a state value constant indicating that there are
        awaiters on the mutex and that the mutex is not locked. *)
    val unlocked_and_need_to_signal : t

    (* *)

    val incr_locked : t Awaitable.t @ local -> t
    val decr_locked : t Awaitable.t @ local -> t

    (* *)

    val is_locked : t -> bool
    val is_locked_permanently : t -> bool
    val is_locked_at_most_once : t -> bool
    val has_uncontended_awaiters : t -> bool

    (* *)

    val and_locked_permanently : t -> t
    val and_need_to_signal : t -> t
    val and_decr_locked : t -> t
    val and_incr_locked : t -> t
  end = struct
    type t = int

    (* The state looks like this:

       Bit: [      0            |  1  to  n-4  | n-3 |        n-2         | n-1 ] Use:
       [ no_need_to_signal | locked count |     | locked permanently |  0  ]

       Above, [n] is the number of bits in an [int] and bit at [n-1] is the sign bit,
       which should normally always be [0].

       The [n-3] bit allows the [locked_permanently] bit to be temporarily [0] due to an
       optimistic decrement of the [locked] count by [release]. *)

    (* [no_need_to_signal] indicates whether or not there are potential awaiters. *)
    let no_need_to_signal = 1 lsl 0
    let unlocked_and_no_need_to_signal = no_need_to_signal
    let unlocked_and_need_to_signal = 0

    (* [locked] is the value [1] of counter bits. The counter is manipulated using
       [fetch_and_add] and may temporarily be greater than [locked].

       There is no fear of overflow, because the number of extra increments is bounded by
       the number of contending threads and there should be far more than enough bits in
       an int to not overflow. *)
    let locked = 1 lsl 1

    (* [locked_permanently] is a threshold value for a poisoned mutex.

       When it comes to the threshold, we allow both lower and higher values so that both
       [acquire] and [release] can use [fetch_and_add] and then check and potentially
       revert the state later. *)
    let locked_permanently = 1 lsl (Int.num_bits - 2)
    let[@inline] incr_locked t = Awaitable.fetch_and_add t locked
    let[@inline] decr_locked t = Awaitable.fetch_and_add t (-locked)

    (* *)

    let[@inline] is_locked state = locked <= state
    let[@inline] is_locked_at_most_once state = state < locked * 2

    let[@inline] has_uncontended_awaiters state =
      (* This is [true] when the [no_need_to_signal] bit has been cleared, meaning that
         there are awaiters, and no other thread has temporarily incremented the [locked]
         count. This is the case where we need to signal an awaiter. In other cases we
         don't need to signal, because either there are no awaiters or there are threads
         contending for the mutex and one of them will notice the lock has been released. *)
      state = locked
    ;;

    let[@inline] is_locked_permanently state =
      (* This allows lower values so that [release] may optimistically use [fetch_and_add]
         and revert the counter decrement later in case the mutex was locked permanently. *)
      locked_permanently / 2 <= state
    ;;

    let[@inline] and_locked_permanently state = state + locked_permanently
    let[@inline] and_need_to_signal state = state land lnot no_need_to_signal
    let[@inline] and_decr_locked state = state - locked
    let[@inline] and_incr_locked state = state + locked
  end

  type t = State.t Awaitable.t

  let[@inline never] rec poison t =
    let before = Awaitable.get t in
    if not (State.is_locked_permanently before)
    then (
      match
        Awaitable.compare_and_set
          t
          ~if_phys_equal_to:before
          ~replace_with:(before |> State.and_locked_permanently)
      with
      | Set_here -> Awaitable.broadcast t
      | Compare_failed -> poison t)
  ;;

  type ('a, _) result =
    | Value : ('a, 'a) result
    | Or_canceled : ('a, 'a Or_canceled.t) result

  let[@inline never] acquire_as (type r) w c t (r : (unit, r) result) : r =
    let[@inline] completed () : r =
      match r with
      | Value -> ()
      | Or_canceled -> Completed ()
    in
    let prior = State.incr_locked t in
    if not (State.is_locked prior)
    then completed ()
    else (
      let[@inline] rec acquire_awaiting w c t backoff before =
        if not (State.is_locked before)
        then (
          (* We know [before] is [no_need_to_signal] or [awaiters]. *)
          let after = State.unlocked_and_need_to_signal |> State.and_incr_locked in
          match
            Awaitable.compare_and_set t ~if_phys_equal_to:before ~replace_with:after
          with
          | Set_here -> completed ()
          | Compare_failed ->
            let backoff = Backoff.once backoff in
            acquire_awaiting w c t backoff (Awaitable.get t))
        else if State.is_locked_permanently before
        then raise Poisoned
        else (
          let after = before |> State.and_need_to_signal in
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
      let[@inline] rec acquire_contended w c t backoff before =
        (* At this point we have incremented the [locked] count.

           We now check whether we actually managed to obtain the mutex exclusively or
           whether we should try to decrement the count back and await. *)
        if State.is_locked_at_most_once before
        then completed ()
        else (
          let after = before |> State.and_decr_locked |> State.and_need_to_signal in
          match
            Awaitable.compare_and_set t ~if_phys_equal_to:before ~replace_with:after
          with
          | Set_here -> acquire_awaiting w c t Backoff.default after
          | Compare_failed ->
            let backoff = Backoff.once backoff in
            acquire_contended w c t backoff (Awaitable.get t))
      in
      acquire_contended w c t Backoff.default (prior |> State.and_incr_locked))
  ;;

  let acquire_or_cancel w c t = acquire_as w c t Or_canceled
  let acquire w t = acquire_as w Cancellation.never t Value

  let[@inline never] release t =
    let[@inline] signal_awaiter t =
      match
        Awaitable.compare_and_set
          t
          ~if_phys_equal_to:State.unlocked_and_need_to_signal
          ~replace_with:State.unlocked_and_no_need_to_signal
      with
      | Set_here -> Awaitable.signal t
      | Compare_failed -> ()
    in
    let[@inline] undo_release t =
      let _ : State.t = State.incr_locked t in
      ()
    in
    (* At this point we assume we have previously incremented [locked] count. *)
    let prior = State.decr_locked t in
    if State.has_uncontended_awaiters prior
    then signal_awaiter t
    else if State.is_locked_permanently prior
    then undo_release t
  ;;

  let[@inline never] release_and_reraise exn t =
    let bt = Backtrace.Exn.most_recent () in
    release t;
    Exn.raise_with_original_backtrace exn bt
  ;;

  let[@inline never] poison_and_reraise exn t =
    let bt = Backtrace.Exn.most_recent () in
    poison t;
    Exn.raise_with_original_backtrace exn bt
  ;;

  let[@inline] create ?padded () =
    Awaitable.make ?padded State.unlocked_and_no_need_to_signal
  ;;
end

type 'k t = Prim.t

let create ?padded _key = Prim.create ?padded ()

module Guard = struct
  type 'k mutex = 'k t

  type 'k inner =
    { mutex : 'k mutex @@ many
    ; mutable should_poison : bool [@atomic]
    }

  type 'k t = { inner : 'k inner @@ aliased contended portable } [@@unboxed]

  let poison_if_locked ({ mutex; _ } as inner) =
    if Atomic.Loc.get [%atomic.loc inner.should_poison] then Prim.poison mutex
  ;;

  let create : 'k mutex -> 'k t @ unique =
    fun mutex ->
    let t = { mutex; should_poison = true } in
    Stdlib.Gc.Safe.finalise poison_if_locked t;
    { inner = t }
  ;;

  let with_key
    :  'k t @ unique
    -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ once unique)
       @ local once
    -> 'a * 'k t @ once unique
    =
    fun t ~f ->
    match f (Capsule.Key.unsafe_mk ()) with
    | #(res, _key) -> res, t
    | exception exn -> Prim.poison_and_reraise exn t.inner.mutex
  ;;

  let with_password
    :  'k t @ unique -> f:('k Capsule.Password.t @ local -> 'a @ unique) @ local once
    -> 'a * 'k t @ unique
    =
    fun t ~f ->
    let { many = a }, t =
      with_key
        t
        ~f:
          (Capsule.Key.with_password ~f:(fun password -> { many = f password })
          [@nontail])
    in
    a, t
  ;;

  let access
    :  'k t @ unique
    -> f:('k Capsule.Access.t -> 'a @ contended once portable unique)
       @ local once portable
    -> 'a * 'k t @ contended once portable unique
    =
    fun t ~f ->
    let { contended = { portable = a } }, t =
      with_key t ~f:(fun key ->
        let #(a, key) =
          Capsule.Key.access key ~f:(fun access -> { portable = f access })
        in
        #({ contended = a }, key))
    in
    a, t
  ;;

  let release { inner = { mutex; _ } as inner } =
    Atomic.Loc.set [%atomic.loc inner.should_poison] false;
    (* Make sure the stack root for [inner] stays alive at least this long, to make sure
       that if its finalizer runs, it sees [should_poison] set to [false]. *)
    let _ : _ = (Sys.opaque_identity [@mode contended]) inner in
    Prim.release mutex
  ;;

  let poison : 'k t @ unique -> 'k Capsule.Key.t @ unique =
    fun { inner = { mutex; _ } as inner } ->
    (* Not techincally releasing, but setting this value to [false] prevents calling
       [poison] again in the finalizer, which is moderately more efficient. *)
    Atomic.Loc.set [%atomic.loc inner.should_poison] false;
    Prim.poison mutex;
    Capsule.Key.unsafe_mk ()
  ;;

  let is_poisoning { inner } = Atomic.Loc.get [%atomic.loc inner.should_poison]
end

let acquire w t =
  Prim.acquire w t;
  Guard.create t
;;

let acquire_or_cancel
  : Await.t @ local -> Cancellation.t @ local -> 'k t -> 'k Guard.t Or_canceled.t @ unique
  =
  fun w c t ->
  match Prim.acquire_or_cancel w c t with
  | Canceled -> Canceled
  | Completed () -> Completed (Guard.create t)
;;

[%%template
[@@@alloc.default a @ l = (heap_global, stack_local)]

let[@inline] with_key
  : type (a : value_or_null) k.
    Await.t @ local
    -> k t @ local
    -> f:(k Capsule.Key.t @ unique -> #(a * k Capsule.Key.t) @ l once unique) @ local once
    -> a @ l once unique
  =
  fun w t ~f ->
  (Prim.acquire w t;
   match f (Capsule.Key.unsafe_mk ()) with
   | #(res, _key) ->
     Prim.release t;
     res
   | exception exn -> Prim.release_and_reraise exn t)
  [@exclave_if_stack a]
;;

let[@inline] with_key_or_cancel
  :  Await.t @ local -> Cancellation.t @ local -> 'k t @ local
  -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ l once unique)
     @ local once
  -> 'a Or_canceled.t @ l once unique
  =
  fun w c t ~f ->
  match[@exclave_if_stack a] Prim.acquire_or_cancel w c t with
  | Canceled -> Canceled
  | Completed () ->
    (match f (Capsule.Key.unsafe_mk ()) with
     | #(res, _key) ->
       Prim.release t;
       Completed res
     | exception exn -> Prim.release_and_reraise exn t)
;;

let[@inline] with_key_poisoning
  :  Await.t @ local -> 'k t @ local
  -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ l once unique)
     @ local once
  -> 'a @ l once unique
  =
  fun w t ~f ->
  (Prim.acquire w t;
   match f (Capsule.Key.unsafe_mk ()) with
   | #(res, _key) ->
     Prim.release t;
     res
   | exception exn -> Prim.poison_and_reraise exn t)
  [@exclave_if_stack a]
;;

let[@inline] with_key_or_cancel_poisoning
  :  Await.t @ local -> Cancellation.t @ local -> 'k t @ local
  -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ l once unique)
     @ local once
  -> 'a Or_canceled.t @ l once unique
  =
  fun w c t ~f ->
  match[@exclave_if_stack a] Prim.acquire_or_cancel w c t with
  | Canceled -> Canceled
  | Completed () ->
    (match f (Capsule.Key.unsafe_mk ()) with
     | #(res, _key) ->
       Prim.release t;
       Completed res
     | exception exn -> Prim.poison_and_reraise exn t)
;;]

let with_access_poisoning w t ~f =
  (with_key_poisoning w t ~f:(fun key ->
     let #(a, key) = Capsule.Key.access key ~f:(fun access -> { portable = f access }) in
     #({ contended = a }, key)))
    .contended
    .portable
;;

let with_access w t ~f =
  (with_key w t ~f:(fun key ->
     let #(a, key) = Capsule.Key.access key ~f:(fun access -> { portable = f access }) in
     #({ contended = a }, key)))
    .contended
    .portable
;;

let with_access_or_cancel_poisoning w c t ~f : _ Or_canceled.t =
  match
    with_key_or_cancel_poisoning w c t ~f:(fun key ->
      let #(a, key) = Capsule.Key.access key ~f:(fun access -> { portable = f access }) in
      #({ contended = a }, key))
  with
  | Canceled -> Canceled
  | Completed { contended = { portable = a } } -> Completed a
;;

let with_access_or_cancel w c t ~f : _ Or_canceled.t =
  match
    with_key_or_cancel w c t ~f:(fun key ->
      let #(a, key) = Capsule.Key.access key ~f:(fun access -> { portable = f access }) in
      #({ contended = a }, key))
  with
  | Canceled -> Canceled
  | Completed { contended = { portable = a } } -> Completed a
;;

let with_password_poisoning w t ~f =
  (with_key_poisoning w t ~f:(fun key ->
     Capsule.Key.with_password key ~f:(fun password -> { many = f password }) [@nontail]))
    .many
;;

let with_password w t ~f =
  (with_key w t ~f:(fun key ->
     Capsule.Key.with_password key ~f:(fun password -> { many = f password }) [@nontail]))
    .many
;;

let with_password_or_cancel_poisoning w c t ~f : _ Or_canceled.t =
  match
    with_key_or_cancel_poisoning w c t ~f:(fun key ->
      Capsule.Key.with_password key ~f:(fun password -> { many = f password }) [@nontail])
  with
  | Canceled -> Canceled
  | Completed { many = a } -> Completed a
;;

let with_password_or_cancel w c t ~f : _ Or_canceled.t =
  match
    with_key_or_cancel w c t ~f:(fun key ->
      Capsule.Key.with_password key ~f:(fun password -> { many = f password }) [@nontail])
  with
  | Canceled -> Canceled
  | Completed { many = a } -> Completed a
;;

let release_temporarily
  :  Await.t @ local -> 'k t @ local -> 'k Capsule.Key.t @ unique
  -> f:(unit -> 'a @ unique) @ local once -> #('a * 'k Capsule.Key.t) @ unique
  =
  fun w t k ~f ->
  Prim.release t;
  let res = f () in
  Prim.acquire w t;
  #(res, k)
;;

let release_temporarily_or_cancel
  : ('a : value_or_null).
  Await.t @ local
  -> Cancellation.t @ local
  -> 'k t @ local
  -> 'k Capsule.Key.t @ unique
  -> f:(unit -> 'a @ unique) @ local once
  -> (#('a * 'k Capsule.Key.t) Or_canceled.t[@kind value_or_null & void]) @ unique
  =
  fun w c t k ~f ->
  Prim.release t;
  let res = f () in
  match Prim.acquire_or_cancel w c t with
  | Canceled -> Canceled
  | Completed () -> Completed #(res, k)
;;

let acquire_and_poison : Await.t @ local -> 'k t @ local -> 'k Capsule.Key.t @ unique =
  fun w t ->
  Prim.acquire w t;
  Prim.poison t;
  Capsule.Key.unsafe_mk ()
;;

let acquire_and_poison_or_cancel
  :  Await.t @ local -> Cancellation.t @ local -> 'k t @ local
  -> ('k Capsule.Key.t Or_canceled.t[@kind void]) @ unique
  =
  fun w c t ->
  match Prim.acquire_or_cancel w c t with
  | Canceled -> Canceled
  | Completed () ->
    Prim.poison t;
    Completed (Capsule.Key.unsafe_mk ())
;;

let poison_unacquired : 'k t @ local -> unit = fun t -> Prim.poison t
let is_poisoned t = Prim.State.is_locked_permanently (Awaitable.get t)

let poison t _key =
  Prim.poison t;
  Capsule.Key.unsafe_mk ()
;;

module Condition = struct
  include Condition_common

  let wait w t ~lock key = wait ~acquire:Prim.acquire ~release:Prim.release w t ~lock key
end

module For_testing = struct
  let is_exclusive t = Prim.State.is_locked (Awaitable.get t)

  include Awaitable.For_testing
end

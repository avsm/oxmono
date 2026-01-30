open Base
open Basement
open Portable_kernel
open Await_kernel

module Queue : sig @@ portable
  type 'a t : immutable_data with 'a

  val empty : 'a t
  val length : 'a t -> int
  val add : 'a -> 'a t -> 'a t
  val filter : f:('a -> bool) @ local -> 'a t -> 'a t

  module All_or_first : sig
    type t =
      | All
      | First
  end

  module Keep_or_return_or_drop : sig
    type t =
      | Keep
      | Return
      | Drop
  end

  val split
    :  f:('a -> Keep_or_return_or_drop.t) @ local
    -> All_or_first.t
    -> 'a t
    -> 'a t * 'a list
end = struct
  type 'a t = 'a list

  let empty = []
  let length xs = List.length xs
  let add x xs = xs @ [ x ]
  let filter ~f xs = List.filter ~f xs

  module All_or_first = struct
    type t =
      | All
      | First
  end

  module Keep_or_return_or_drop = struct
    type t =
      | Keep
      | Return
      | Drop
  end

  let split ~f all_or_first xs =
    let rec loop f all_or_first ~keep ~return = function
      | [] -> List.rev keep, List.rev return
      | x :: xs ->
        (match (f x : Keep_or_return_or_drop.t) with
         | Keep -> loop f all_or_first ~keep:(x :: keep) ~return xs
         | Drop -> loop f all_or_first ~keep ~return xs
         | Return ->
           let return = x :: return in
           (match (all_or_first : All_or_first.t) with
            | All -> loop f all_or_first ~keep ~return xs
            | First -> List.rev_append keep xs, return))
    in
    loop f all_or_first ~keep:[] ~return:[] xs
  ;;
end

type ('a : value_or_null) awaiter =
  { comparand : 'a @@ contended portable
  ; trigger : Trigger.Source.t
  }

type ('a : value_or_null) awaitable =
  { mutable value : 'a portended [@atomic]
  ; mutable queue : 'a awaiter Queue.t [@atomic]
  }

type ('a : value_or_null) t = 'a awaitable contended

let[@inline] make ?padded value =
  { contended =
      Portable_common.Padding.copy_as
        ?padded
        { value = { portended = value }; queue = Queue.empty }
  }
;;

let[@inline] get t = (Atomic.Loc.get [%atomic.loc t.contended.value]).portended

let[@inline] compare_and_set t ~if_phys_equal_to ~replace_with =
  Atomic.Loc.compare_and_set
    [%atomic.loc t.contended.value]
    ~if_phys_equal_to:{ portended = if_phys_equal_to }
    ~replace_with:{ portended = replace_with }
;;

module Compare_failed_or_set_here = Atomic.Compare_failed_or_set_here

let[@inline] compare_exchange t ~if_phys_equal_to ~replace_with =
  (Atomic.Loc.compare_exchange
     [%atomic.loc t.contended.value]
     ~if_phys_equal_to:{ portended = if_phys_equal_to }
     ~replace_with:{ portended = replace_with })
    .portended
;;

let[@inline] exchange t value =
  (Atomic.Loc.exchange [%atomic.loc t.contended.value] { portended = value }).portended
;;

(* This is safe because [portended] is [[@@unboxed]], meaning [int] and [int portended]
   have the same runtime representation. *)
external fetch_and_add_portended_loc
  :  (int portended Atomic.Loc.t[@local_opt]) @ contended
  -> int
  -> int
  @@ portable
  = "%atomic_fetch_add_loc"

let[@inline] fetch_and_add t n =
  fetch_and_add_portended_loc [%atomic.loc t.contended.value] n
;;

let[@inline] set t value =
  Atomic.Loc.set [%atomic.loc t.contended.value] { portended = value }
;;

let[@inline] incr t = ignore (fetch_and_add t 1 : int)
let[@inline] decr t = ignore (fetch_and_add t (-1) : int)

let[@inline] get_and_update t ~pure_f =
  let[@inline] rec aux backoff =
    let old = get t in
    let new_ = pure_f old in
    match compare_and_set t ~if_phys_equal_to:old ~replace_with:new_ with
    | Set_here -> old
    | Compare_failed -> aux (Backoff.once backoff)
  in
  aux Backoff.default [@nontail]
;;

let[@inline] update (type a : value_or_null) (t : a t) ~pure_f =
  Basement.Stdlib_shim.ignore_contended (get_and_update t ~pure_f : a)
;;

type _ await =
  | Signaled : [> `Signaled ] await
  | Terminated : [> `Terminated ] await
  | Canceled : [> `Canceled ] await

let add t awaiter =
  Atomic.Loc.update [%atomic.loc t.contended.queue] ~pure_f:(fun queue ->
    Queue.add awaiter queue)
  [@nontail]
;;

let remove_signalled (t @ local) =
  Atomic.Loc.update [%atomic.loc t.contended.queue] ~pure_f:(fun queue ->
    Queue.filter
      ~f:(fun awaiter -> not (Trigger.Source.is_signalled awaiter.trigger))
      queue)
  [@nontail]
;;

let resume t all_or_first =
  let rec loop backoff =
    let value = (Atomic.Loc.get [%atomic.loc t.contended.value]).portended in
    let before = Atomic.Loc.get [%atomic.loc t.contended.queue] in
    let after, to_signal =
      Queue.split all_or_first before ~f:(fun awaiter ->
        if Trigger.Source.is_signalled awaiter.trigger
        then Drop
        else if phys_equal awaiter.comparand value
        then Keep
        else Return)
    in
    match
      Atomic.Loc.compare_and_set
        [%atomic.loc t.contended.queue]
        ~if_phys_equal_to:before
        ~replace_with:after
    with
    | Set_here ->
      List.iter ~f:(fun awaiter -> Trigger.Source.signal awaiter.trigger) to_signal
    | Compare_failed -> loop (Backoff.once backoff)
  in
  loop Backoff.default [@nontail]
;;

let signal t = resume t First
let broadcast t = resume t All

let await_or_cancel_as w cancellation t comparand on_canceled =
  let trigger = Trigger.create () in
  let awaiter = { comparand; trigger = Trigger.source trigger } in
  (* We assume that the caller has just a couple of nanoseconds earlier obtained the
     [comparand] value from the awaitable and so we don't bother to do the equality test
     before adding the [awaiter]. *)
  add t awaiter;
  (* Now that the [awaiter] has been added to the queue we are guaranteed not to miss a
     signal, but it is possible that a signal happened concurrently with adding the
     awaiter and before suspending the fiber we must check whether the value of the
     awaitable is equal to [comparand] or not. *)
  if phys_equal (get t) comparand
  then (
    let forward t result =
      remove_signalled t;
      signal t;
      result
    in
    Await.await_until_terminated_or_canceled w cancellation trigger;
    if Await.is_terminated w
    then forward t Terminated
    else if Cancellation.is_canceled cancellation
    then forward t on_canceled
    else Signaled)
  else (
    Trigger.Source.signal (Trigger.source trigger);
    remove_signalled t;
    Signaled)
;;

let await w t ~until_phys_unequal_to:comparand =
  await_or_cancel_as w Cancellation.never t comparand Terminated
;;

let await_or_cancel w c t ~until_phys_unequal_to:comparand =
  await_or_cancel_as w c t comparand Canceled
;;

module Awaiter = struct
  type t =
    | T :
        ('a : value_or_null).
        { awaitable : 'a awaitable contended
        ; awaiter : 'a awaiter @@ global
        }
        -> t

  let%template create_and_add t trigger ~until_phys_unequal_to:comparand =
    let awaiter = { comparand; trigger } in
    add t awaiter;
    T { awaitable = t; awaiter } [@exclave_if_local l]
  [@@mode l = (global, local)]
  ;;

  let cancel_and_remove (T { awaitable = t; awaiter }) =
    Trigger.Source.signal awaiter.trigger;
    remove_signalled t;
    signal t
  ;;
end

module For_testing = struct
  let length t = Queue.length (Atomic.Loc.get [%atomic.loc t.contended.queue])
end

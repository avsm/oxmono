open Base
open Basement
open Await_kernel
open Await_sync_intf

type ('a, _) state : value mod contended portable =
  | Value :
      { value : 'a @@ contended portable
      ; mutable wait : bool
      }
      -> ('a, [> `Value ]) state
  | Update :
      { value : 'a @@ contended portable
      ; pure_f : 'a @ contended portable -> 'a @ contended portable @@ portable
      }
      -> ('a, [> `Update ]) state
  | Poisoned : { value : 'a @@ contended portable } -> ('a, [> `Poisoned ]) state
[@@unsafe_allow_any_mode_crossing
  (* The updates of [wait] are safe.  Search for "happens-before" in comments below. *)]

let[@inline] value_of state =
  (* When [r.value] is at the same position in all cases the match and projection below
     can be optimized to a single instruction. *)
  match state with
  | Value r -> r.value
  | Update r -> r.value
  | Poisoned r -> r.value
;;

let[@inline] should_broadcast : (_, [< `Value ]) state -> _ = function
  | Value r -> r.wait
;;

let[@inline] set_wait : (_, [< `Value ]) state -> _ = function
  | Value r -> if not r.wait then r.wait <- true
;;

type 'a t = ('a, [ `Value | `Update | `Poisoned ]) state Awaitable.t

let rec poison_and_raise t exn bt =
  match Awaitable.get t with
  | (Value _ | Update _) as before ->
    (* At this point we known that an update has raised. So, we do not try to finish the
       update. *)
    let after = Poisoned { value = value_of before } in
    (match Awaitable.compare_and_set t ~if_phys_equal_to:before ~replace_with:after with
     | Set_here ->
       Awaitable.broadcast t;
       Exn.raise_with_original_backtrace exn bt
     | Compare_failed -> poison_and_raise t exn bt)
  | Poisoned _ -> raise Poisoned
;;

let make ?padded value = Awaitable.make ?padded (Value { value; wait = false })
let[@inline] get t = value_of (Awaitable.get t)

type op =
  | As_value
  | As_poisoned

let finish_as t (Update r as before : (_, [ `Update ]) state) op =
  match r.pure_f r.value with
  | value ->
    if phys_equal before (Awaitable.get t)
    then (
      let after =
        match op with
        | As_value -> Value { value; wait = false }
        | As_poisoned -> Poisoned { value }
      in
      Awaitable.compare_and_set t ~if_phys_equal_to:before ~replace_with:after)
    else Compare_failed
  | exception exn ->
    let bt = Backtrace.Exn.most_recent () in
    poison_and_raise t exn bt
;;

let rec poison t =
  match Awaitable.get t with
  | Value _ as before ->
    let after = Poisoned { value = value_of before } in
    (match Awaitable.compare_and_set t ~if_phys_equal_to:before ~replace_with:after with
     | Set_here -> Awaitable.broadcast t
     | Compare_failed -> poison t)
  | Update _ as before ->
    (match finish_as t before As_poisoned with
     | Set_here -> ()
     | Compare_failed -> poison t)
  | Poisoned _ -> ()
;;

let rec update_contended t pure_f backoff =
  match Awaitable.get t with
  | Value before_r as before ->
    let (Update after_r as after : (_, [ `Update ]) state) =
      Update { value = before_r.value; pure_f }
    in
    (match Awaitable.compare_and_set t ~if_phys_equal_to:before ~replace_with:after with
     | Set_here ->
       (* Check broadcast after CAS to ensure any [set_wait] happens-before. *)
       if should_broadcast before then Awaitable.broadcast t;
       let _ : Awaitable.Compare_failed_or_set_here.t = finish_as t after As_value in
       before_r.value
     | Compare_failed -> update_contended t after_r.pure_f (Backoff.once backoff))
  | Update _ as before -> finish_and_update_contended t pure_f backoff before
  | Poisoned _ -> raise Poisoned

and finish_and_update_contended t pure_f backoff before =
  let backoff =
    match finish_as t before As_value with
    | Set_here -> backoff
    | Compare_failed -> Backoff.once backoff
  in
  update_contended t pure_f backoff
;;

let get_and_update t ~pure_f =
  match Awaitable.get t with
  | Value before_r as before ->
    (match pure_f before_r.value with
     | value ->
       if not (phys_equal value before_r.value)
       then (
         let after = Value { value; wait = false } in
         match
           Awaitable.compare_and_set t ~if_phys_equal_to:before ~replace_with:after
         with
         | Set_here ->
           (* Check broadcast after CAS to ensure any [set_wait] happens-before. *)
           if should_broadcast before then Awaitable.broadcast t;
           before_r.value
         | Compare_failed -> update_contended t pure_f (Backoff.once Backoff.default))
       else value
     | exception exn ->
       let bt = Backtrace.Exn.most_recent () in
       poison_and_raise t exn bt)
  | Update _ as before -> finish_and_update_contended t pure_f Backoff.default before
  | Poisoned _ -> raise Poisoned
;;

let[@inline] update t ~pure_f =
  let _ : _ = get_and_update t ~pure_f in
  ()
;;

let rec wait_and_return w t ~until_phys_unequal_to:value =
  match Awaitable.get t with
  | Value before_r as before ->
    if phys_equal before_r.value value
    then (
      set_wait before;
      (* As noted in documentation of [Awaitable.await], any read/write in program order
         happens-before the comparison done by [Awaitable.await]. *)
      match Awaitable.await w t ~until_phys_unequal_to:before with
      | Signaled -> wait_and_return w t ~until_phys_unequal_to:value
      | Terminated -> raise Await.Terminated)
    else before_r.value
  | Update _ as before ->
    let _ : Awaitable.Compare_failed_or_set_here.t = finish_as t before As_value in
    wait_and_return w t ~until_phys_unequal_to:value
  | Poisoned _ -> raise Poisoned
;;

let[@inline] wait w t ~until_phys_unequal_to =
  let _ : _ = wait_and_return w t ~until_phys_unequal_to in
  ()
;;

let[@inline never] rec wait_for_and_return_loop w t ~f value =
  let value = wait_and_return w t ~until_phys_unequal_to:value in
  if f value then value else wait_for_and_return_loop w t ~f value
;;

let wait_for_and_return w t ~f =
  let value = get t in
  if f value then value else wait_for_and_return_loop w t ~f value
;;

let[@inline] wait_for w t ~f =
  let _ : _ = wait_for_and_return w t ~f in
  ()
;;

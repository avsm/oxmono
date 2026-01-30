open Base
open Await_kernel
open Await_sync_intf

(*=The underlying state machine of an ivar:

                                                     [create_full]
                                                           |
                                                           v
    [create]--> Empty --[read]--> Awaiters --[fill_*]--> Value
                  |                                        ^
                  |                                        |
                  +----------------------------------------+

   The [Value] state is terminal. *)

module State : sig @@ portable
  type +'a t : immutable_data with 'a @@ contended portable
  (*== | Empty
       | Awaiters
       | Value of 'a @@ contended portable
  *)

  (** Constructors *)

  val empty : 'a t
  val awaiters : 'a t
  val of_value : 'a @ contended portable -> 'a t

  (** Destructors *)

  val is_empty : 'a t @ contended local -> bool
  val is_awaiters : 'a t @ contended local -> bool
  val value_opt : 'a t @ contended -> 'a or_null @ contended portable
  val value_unsafe : 'a t @ contended -> 'a @ contended portable
end = struct
  type +'a t : immutable_data with 'a @@ contended portable

  let empty = (Obj.magic [@mode contended portable]) (ref 0)
  let awaiters = (Obj.magic [@mode contended portable]) (ref 1)
  let[@inline] of_value x = (Obj.magic [@mode contended portable]) x
  let[@inline] is_empty (x @ local) = phys_equal empty x
  let[@inline] is_awaiters (x @ local) = phys_equal awaiters x
  let[@inline] is_value x = not (is_empty x || is_awaiters x)

  let[@inline] value_opt x =
    if is_value x then This ((Obj.magic [@mode contended portable]) x) else Null
  ;;

  let[@inline] value_unsafe x =
    assert%debug (is_value x);
    (Obj.magic [@mode contended portable]) x
  ;;
end

type 'a t = { t : 'a State.t Awaitable.t } [@@unboxed]

let create () = { t = Awaitable.make State.empty }
let create_full v = { t = Awaitable.make (State.of_value v) }

type on_full =
  | Drop
  | Raise

let fill_as (type a) ({ t } : a t) (v : a @ portable) on_empty =
  let v = State.of_value v in
  let before = Awaitable.get t in
  let[@inline] try_empty t =
    State.is_empty before
    &&
    match Awaitable.compare_and_set t ~if_phys_equal_to:State.empty ~replace_with:v with
    | Set_here -> true
    | Compare_failed -> false
  in
  let[@inline] try_awaiters t =
    match
      Awaitable.compare_and_set t ~if_phys_equal_to:State.awaiters ~replace_with:v
    with
    | Set_here -> Awaitable.broadcast t
    | Compare_failed ->
      (match on_empty with
       | Drop -> ()
       | Raise -> raise Already_full)
  in
  if not (try_empty t) then try_awaiters t
;;

let fill_if_empty t v = fill_as t v Drop
let fill_exn t v = fill_as t v Raise

type ('a, _) result =
  | Value : ('a, 'a) result
  | Or_canceled : ('a, 'a Or_canceled.t) result

let read_as (type a r) w c ({ t } : a t) (r : (a, r) result) : r =
  let value_or_empty_or_awaiters = Awaitable.get t in
  let value_or_awaiters =
    if State.is_empty value_or_empty_or_awaiters
    then (
      match
        Awaitable.compare_and_set
          t
          ~if_phys_equal_to:State.empty
          ~replace_with:State.awaiters
      with
      | Set_here -> State.awaiters
      | Compare_failed -> Awaitable.get t)
    else value_or_empty_or_awaiters
  in
  if State.is_awaiters value_or_awaiters
  then (
    match r with
    | Value ->
      (match Awaitable.await w t ~until_phys_unequal_to:State.awaiters with
       | Signaled ->
         (* [Awaitable.await] will not return [Signaled] unless the state has been changed
            and the only state change from [awaiters] is to a value. *)
         State.value_unsafe (Awaitable.get t)
       | Terminated -> raise Await.Terminated)
    | Or_canceled ->
      (match Awaitable.await_or_cancel w c t ~until_phys_unequal_to:State.awaiters with
       | Signaled ->
         (* [Awaitable.await] will not return [Signaled] unless the state has been changed
            and the only state change from [awaiters] is to a value. *)
         Completed (State.value_unsafe (Awaitable.get t))
       | Canceled -> Canceled
       | Terminated -> raise Await.Terminated))
  else (
    (* At this point we know that the state we got was a value. *)
    match r with
    | Value -> State.value_unsafe value_or_awaiters
    | Or_canceled -> Completed (State.value_unsafe value_or_awaiters))
;;

let read w t = read_as w Cancellation.never t Value
let read_or_cancel w c t = read_as w c t Or_canceled

let peek { t } =
  let value_or_empty_or_awaiters = Awaitable.get t in
  State.value_opt value_or_empty_or_awaiters
;;

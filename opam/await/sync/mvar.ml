open Base
open Basement
open Await_kernel
open Await_sync_intf

(*=The underlying state machine of an mvar:

                                                    [create_full]
                  +--[set]-----------------------------+  |
                  |                                    v  v
    [create]--> Empty --[read]--> Readers --[set_*]--> Value --[take_*]--+
                ^ |                                        ^             |
                | |                                        |             |
                | +---------------------------------------+              |
                +--------------------------------------------------------+


   An implementation note: we don't try to differentiate between "Value, and there are
   waiting putters" and "Value, but there aren't any waiting putters", and hence call
   {!Awaitable.broadcast} unconditionally on [take] et all. This is mostly a tradeoff
   between the size of the state representation and the cost of the spurious broadcasts *)

(* SAFETY:

   Nothing internal to this module enforces the [unique once] properties in the API - that
   values are only ever read by as many threads as they are written. It's important to
   verify the implementation carefully to make sure this property is upheld internally.
   Also, there's an extensive quickcheck test that verifies that property.
*)

module State : sig @@ portable
  type !'a t : immutable_data with 'a @@ contended portable
  (*== | Empty
       | Readers
       | Value of 'a @@ contended portable *)

  (** Constructors *)

  val empty : 'a t
  val readers : 'a t
  val of_value : 'a @ contended once portable unique -> 'a t

  (** Destructors *)

  val is_readers : 'a t @ contended local -> bool
  val is_value : 'a t @ contended local -> bool
  val value : 'a t @ contended -> 'a or_null @ contended once portable unique
end = struct
  type !+'a t : immutable_data with 'a @@ contended portable

  let empty = (Obj.magic [@mode contended portable]) (ref 0)
  let readers = (Obj.magic [@mode contended portable]) (ref 1)

  let[@inline] of_value x =
    (Obj.magic [@mode contended once portable unique]) x
    |> (Obj.magic_uncontended [@mode once])
    |> Obj.magic_many
  ;;

  let[@inline] is_empty x = phys_equal empty x
  let[@inline] is_readers x = phys_equal readers x
  let[@inline] is_value x = not (is_empty x || is_readers x)

  let[@inline] value_unsafe x =
    (Obj.magic [@mode contended many portable aliased]) x
    |> (Obj.magic_uncontended [@mode portable])
    |> Obj.magic_unique
    |> (Obj.magic_portable [@mode unique])
  ;;

  let[@inline] value x = if is_value x then This (value_unsafe x) else Null
end

type 'a t = 'a State.t Awaitable.t

let create ?padded () = Awaitable.make ?padded State.empty
let create_full ?padded v = Awaitable.make ?padded (State.of_value v)

type ('a, 'r) result =
  | Value : ('a, 'a) result
  | Or_canceled : ('a, 'a Or_canceled.t) result

let put_as (type r) w c t v (r : (unit, r) result) : r =
  let next = State.of_value v in
  let[@inline] rec go backoff before : r =
    if not (State.is_value before)
    then (
      match Awaitable.compare_and_set t ~if_phys_equal_to:before ~replace_with:next with
      | Set_here ->
        if State.is_readers before then Awaitable.broadcast t;
        (match r with
         | Value -> ()
         | Or_canceled -> Completed ())
      | Compare_failed ->
        (* Another putter beat us to writing; try again. *)
        let backoff = Backoff.once backoff in
        go backoff (Awaitable.get t))
    else (
      match r with
      | Value ->
        (match Awaitable.await w t ~until_phys_unequal_to:before with
         | Signaled -> go Backoff.default (Awaitable.get t)
         | Terminated -> raise Await.Terminated)
      | Or_canceled ->
        (match Awaitable.await_or_cancel w c t ~until_phys_unequal_to:before with
         | Signaled -> go Backoff.default (Awaitable.get t)
         | Terminated -> raise Await.Terminated
         | Canceled -> Canceled))
  in
  go Backoff.default (Awaitable.get t) [@nontail]
;;

let put w t v = put_as w Cancellation.never t v Value
let put_or_cancel w c t v = put_as w c t v Or_canceled

module Ok_or_already_full = struct
  type t =
    | Ok
    | Already_full
  [@@deriving sexp_of]
end

let try_put t v =
  let after = State.of_value v in
  let[@inline] rec go () : Ok_or_already_full.t =
    let before = Awaitable.get t in
    if State.is_value before
    then Already_full
    else (
      match Awaitable.compare_and_set t ~if_phys_equal_to:before ~replace_with:after with
      | Set_here ->
        if State.is_readers before then Awaitable.broadcast t;
        Ok
      | Compare_failed -> go ())
  in
  go () [@nontail]
;;

let put_exn t v =
  match try_put t v with
  | Ok -> ()
  | Already_full -> raise Already_full
;;

let take_as (type a r) w c (t : a t) (r : (a, r) result) : r =
  let[@inline] rec go backoff before : r =
    match State.value before with
    | This v ->
      (match
         Awaitable.compare_and_set t ~if_phys_equal_to:before ~replace_with:State.empty
       with
       | Set_here ->
         (* Note: we unconditionally broadcast here since the state representation doesn't
            differentiate between "full" and "full, with waiting putters". *)
         Awaitable.broadcast t;
         (match r with
          | Value -> v
          | Or_canceled -> Completed v)
       | Compare_failed ->
         let backoff = Backoff.once backoff in
         go backoff (Awaitable.get t))
    | Null ->
      let after_set_to_readers =
        if State.is_readers before
        then State.readers
        else (
          match
            Awaitable.compare_and_set
              t
              ~if_phys_equal_to:State.empty
              ~replace_with:State.readers
          with
          | Set_here -> State.readers
          | Compare_failed -> Awaitable.get t)
      in
      if State.is_readers after_set_to_readers
      then (
        match r with
        | Value ->
          (match Awaitable.await w t ~until_phys_unequal_to:after_set_to_readers with
           | Signaled -> go Backoff.default (Awaitable.get t)
           | Terminated -> raise Await.Terminated)
        | Or_canceled ->
          (match
             Awaitable.await_or_cancel w c t ~until_phys_unequal_to:after_set_to_readers
           with
           | Signaled -> go Backoff.default (Awaitable.get t)
           | Terminated -> raise Await.Terminated
           | Canceled -> Canceled))
      else go (Backoff.once backoff) after_set_to_readers
  in
  go Backoff.default (Awaitable.get t) [@nontail]
;;

let take w t = take_as w Cancellation.never t Value
let take_or_cancel w c t = take_as w c t Or_canceled

let try_take t =
  let[@inline] rec go backoff =
    let before = Awaitable.get t in
    match State.value before with
    | Null -> Null
    | This v ->
      (match
         Awaitable.compare_and_set t ~if_phys_equal_to:before ~replace_with:State.empty
       with
       | Set_here ->
         (* Note: we unconditionally broadcast here since the state representation doesn't
            differentiate between "full" and "full, with waiting putters". *)
         Awaitable.broadcast t;
         This v
       | Compare_failed -> go (Backoff.once backoff))
  in
  go Backoff.default [@nontail]
;;

let take_exn t =
  match try_take t with
  | This v -> v
  | Null -> raise Empty
;;

let is_full t = t |> Awaitable.get |> State.is_value

let rec wait_until_empty_as
  : type a r.
    Await.t @ local -> Cancellation.t @ local -> a t @ local -> (unit, r) result -> r
  =
  fun w c t r ->
  let before = Awaitable.get t in
  if State.is_value before
  then (
    match r with
    | Value ->
      (match Awaitable.await w t ~until_phys_unequal_to:before with
       | Signaled -> wait_until_empty_as w c t r
       | Terminated -> raise Await.Terminated)
    | Or_canceled ->
      (match Awaitable.await_or_cancel w c t ~until_phys_unequal_to:before with
       | Signaled -> wait_until_empty_as w c t r
       | Terminated -> raise Await.Terminated
       | Canceled -> Canceled))
  else (
    match r with
    | Value -> ()
    | Or_canceled -> Completed ())
;;

let wait_until_empty w t = wait_until_empty_as w Cancellation.never t Value
let wait_until_empty_or_cancel w c t = wait_until_empty_as w c t Or_canceled

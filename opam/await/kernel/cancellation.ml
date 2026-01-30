(*=The underlying state machine of a cancellation [Token]:

      [with_*]
         |
         +------------------------+
         |                        |
         v                        |
      Nil/Cons -- [add_trigger] --+
         |
      [cancel]
         |
         v
      Canceled

   The [Canceled] state is terminal. *)

open! Base
open Basement
open Portable_kernel

type%fuelproof _ state_inner : value mod contended portable =
  | Canceled : [> `Canceled ] state_inner
  | Nil : [> `Nil ] state_inner
  | Cons :
      { countdown : int
          (* [countdown] is used to amortize the cost of removing signalled triggers.

             Each time a [Cons _] is added for a new trigger, the [countdown] is
             decremented unless it was zero, in which case [cleanup] will be called
             instead.

             [cleanup unsignalled possibly_signalled] is called with a [Cons _] of a new
             [unsignalled] trigger to be added and it then goes through the list of
             [possibly_signalled] triggers to accumulate all the unsignalled triggers and
             count them as the value of the [countdown] in the [Cons _] of the accumulated
             list of unsignalled triggers.

             This amortizes the cost of cleaning up signalled triggers to to O(1) per
             trigger and also guarantees that no more than O(max n) space is used where
             [max n] is the maximum number of unsignalled triggers at any point.

             Also, the expectation is that triggers are signalled frequently relative to
             adding them. *)
      ; trigger : Trigger.Source.t
      ; next : [ `Nil | `Cons ] state_inner
      }
      -> [> `Cons ] state_inner

type state = S : [< `Canceled | `Nil | `Cons ] state_inner -> state [@@unboxed]

type%fuelproof _ token : value mod portable =
  | Never : [> `Never ] token
  | Token : { mutable state : state [@atomic] } -> [> `Token ] token

type t = { token : [ `Never | `Token ] token @@ aliased contended global } [@@unboxed]

let same = Base.phys_equal
let never = { token = Never }
let always = { token = Token { state = S Canceled } }

module Source = struct
  type t = { token : [ `Token ] token @@ aliased contended global } [@@unboxed]

  let cancel { token = Token r } =
    match Atomic.Loc.get [%atomic.loc r.state] with
    | S Canceled -> ()
    | S (Nil | Cons _) ->
      (match Atomic.Loc.exchange [%atomic.loc r.state] (S Canceled) with
       | S ((Nil | Cons _) as before) ->
         let rec signal = function
           | Nil -> ()
           | Cons r ->
             Trigger.Source.signal r.trigger;
             signal r.next
         in
         signal before
       | S Canceled -> ())
  ;;

  let is_canceled { token = Token r } =
    match Atomic.Loc.get [%atomic.loc r.state] with
    | S Canceled -> true
    | S (Nil | Cons _) -> false
  ;;
end

let is_canceled (t : t) =
  match t.token with
  | Never -> false
  | Token _ as token -> Source.is_canceled { token }
;;

let check t : unit Or_canceled.t = if is_canceled t then Canceled else Completed ()

module Link = struct
  type t =
    | Attached
    | Canceled
    | Signaled
  [@@deriving equal ~localize, sexp ~stackify]
end

let rec add_trigger (Token token_r as t : [ `Token ] token) trigger backoff : Link.t =
  match Atomic.Loc.get [%atomic.loc token_r.state] with
  | S Canceled -> Canceled
  | S (Nil as before) ->
    if Trigger.Source.is_signalled trigger
    then Signaled
    else (
      let after = Cons { countdown = 1; trigger; next = Nil } in
      match
        Atomic.Loc.compare_and_set
          [%atomic.loc token_r.state]
          ~if_phys_equal_to:(S before)
          ~replace_with:(S after)
      with
      | Set_here -> Attached
      | Compare_failed -> add_trigger t trigger (Backoff.once backoff))
  | S (Cons r as before) ->
    if Trigger.Source.is_signalled trigger
    then Signaled
    else if 0 < r.countdown
    then (
      let after = Cons { countdown = r.countdown - 1; trigger; next = before } in
      match
        Atomic.Loc.compare_and_set
          [%atomic.loc token_r.state]
          ~if_phys_equal_to:(S before)
          ~replace_with:(S after)
      with
      | Set_here -> Attached
      | Compare_failed -> add_trigger t trigger (Backoff.once backoff))
    else (
      let rec cleanup (Cons after_r as after : [ `Cons ] state_inner) = function
        | Nil -> after
        | Cons before_r ->
          if Trigger.Source.is_signalled before_r.trigger
          then cleanup after before_r.next
          else
            cleanup
              (Cons
                 { countdown = after_r.countdown + 1
                 ; trigger = before_r.trigger
                 ; next = after
                 })
              before_r.next
      in
      let after = cleanup (Cons { countdown = 1; trigger; next = Nil }) before in
      if Trigger.Source.is_signalled trigger
      then Signaled
      else (
        match
          Atomic.Loc.compare_and_set
            [%atomic.loc token_r.state]
            ~if_phys_equal_to:(S before)
            ~replace_with:(S after)
        with
        | Set_here -> Attached
        | Compare_failed -> add_trigger t trigger (Backoff.once backoff)))
;;

let[@inline] add_trigger t trigger : Link.t =
  match t.token with
  | Never -> Attached
  | Token _ as token -> add_trigger token trigger Backoff.default
;;

let check_clean_and_close (Token r : [ `Token ] token) =
  match Atomic.Loc.get [%atomic.loc r.state] with
  | S Canceled -> ()
  | S ((Nil | Cons _) as before) ->
    let rec check = function
      | Nil -> true
      | Cons r -> Trigger.Source.is_signalled r.trigger && check r.next
    in
    if check before
    then (
      let _ : state =
        Atomic.Loc.compare_exchange
          [%atomic.loc r.state]
          ~if_phys_equal_to:(S before)
          ~replace_with:(S Canceled)
      in
      ())
    else failwith "Cancellation: unsignalled triggers leaked"
;;

let[@inline] is_cancellable t =
  match t.token with
  | Never -> false
  | Token _ -> true
;;

let[@inline] source t =
  match t.token with
  | Never -> Null
  | Token _ as token -> This { Source.token }
;;

let with_linked_multi parents body =
  let (Token _ as token) : [ `Token ] token = Token { state = S Nil } in
  let source = { Source.token } in
  let trigger = Trigger.create_with_action ~f:Source.cancel source in
  let rec add_to_parents = function
    | parent :: parents ->
      (match add_trigger parent (Trigger.source trigger) with
       | Attached -> add_to_parents parents
       | (Canceled | Signaled) as result -> result)
    | [] -> Attached
  in
  (match add_to_parents parents with
   | Attached | Signaled -> ()
   | Canceled -> if Trigger.drop trigger then Source.cancel source);
  match body { token } with
  | result ->
    let _ : bool = Trigger.drop trigger in
    check_clean_and_close token;
    result
  | exception exn ->
    let bt = Backtrace.Exn.most_recent () in
    let _ : bool = Trigger.drop trigger in
    check_clean_and_close token;
    Exn.raise_with_original_backtrace exn bt
;;

let with_linked parent body = with_linked_multi [ parent ] body [@nontail]
let with_ body = with_linked_multi [] body

module Expert = struct
  let globalize { token } = { token }
  let create () = { token = Token { state = S Nil } }
end

module For_testing = struct
  let get_countdown t =
    match t.token with
    | Never -> 0
    | Token r ->
      (match Atomic.Loc.get [%atomic.loc r.state] with
       | S (Canceled | Nil) -> 0
       | S (Cons r) -> r.countdown)
  ;;
end

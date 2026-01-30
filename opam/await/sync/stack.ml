open Base
open Basement
open Await_kernel

type 'a t = 'a Modes.Portended.t list Awaitable.t

let create ?padded () = Awaitable.make ?padded []

let sexp_of_t (type a : value mod contended) sexp_of_a (t : a t) =
  Awaitable.get t |> List.map ~f:(fun { portended } -> portended) |> [%sexp_of: a list]
;;

let push t a =
  let[@inline] rec loop t a backoff =
    let before = Awaitable.get t in
    match
      Awaitable.compare_and_set t ~if_phys_equal_to:before ~replace_with:(a :: before)
    with
    | Set_here ->
      (match before with
       | [] -> Awaitable.signal t
       | _ :: _ -> ())
    | Compare_failed -> loop t a (Backoff.once backoff)
  in
  loop t { portended = a } Backoff.default
;;

type state =
  | Never_awaited
  | Signaled

let pop await t =
  let[@inline] rec loop backoff await t state =
    match Awaitable.get t with
    | [] ->
      (match Awaitable.await await t ~until_phys_unequal_to:[] with
       | Terminated -> raise Await.Terminated
       | Signaled -> loop Backoff.default await t Signaled)
    | x :: xs as cur ->
      (match Awaitable.compare_and_set t ~if_phys_equal_to:cur ~replace_with:xs with
       | Set_here ->
         (* Signal another awaiter in case we consumed a signal and there are items. *)
         (match state, xs with
          | Signaled, _ :: _ -> Awaitable.signal t
          | Never_awaited, _ | _, [] -> ());
         x.portended
       | Compare_failed -> loop (Backoff.once backoff) await t state)
  in
  loop Backoff.default await t Never_awaited
;;

let pop_or_cancel await c t =
  let[@inline] rec loop backoff await c t state : _ Or_canceled.t =
    match Awaitable.get t with
    | [] ->
      (match Awaitable.await_or_cancel await c t ~until_phys_unequal_to:[] with
       | Terminated -> raise Await.Terminated
       | Canceled -> Canceled
       | Signaled -> loop Backoff.default await c t Signaled)
    | x :: xs as cur ->
      (match Awaitable.compare_and_set t ~if_phys_equal_to:cur ~replace_with:xs with
       | Set_here ->
         (* Signal another awaiter in case we consumed a signal and there are items. *)
         (match state, xs with
          | Signaled, _ :: _ -> Awaitable.signal t
          | Never_awaited, _ | _, [] -> ());
         Completed x.portended
       | Compare_failed -> loop (Backoff.once backoff) await c t state)
  in
  loop Backoff.default await c t Never_awaited
;;

let pop_nonblocking t =
  let[@inline] rec loop backoff t =
    match Awaitable.get t with
    | [] -> Null
    | x :: xs as cur ->
      (match Awaitable.compare_and_set t ~if_phys_equal_to:cur ~replace_with:xs with
       | Set_here -> This x.portended
       | Compare_failed -> loop (Backoff.once backoff) t)
  in
  loop Backoff.default t
;;

external portended_list
  :  'a Modes.Portended.t list
  -> 'a list @ contended portable
  @@ portable
  = "%identity"

let drain t = Awaitable.exchange t [] |> portended_list

module For_testing = Awaitable.For_testing

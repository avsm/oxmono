open! Core
open Async_kernel

module State = struct
  type 'a t =
    | Init
    | Waiting_for_next_value of 'a Ivar.t
    | Unseen_value of 'a
end

type 'a t =
  { mutable state : 'a State.t
  ; mutable unsubscribe : unit -> unit
  }

let create () = { state = Init; unsubscribe = (fun () -> ()) }

let push t value =
  t.state
  <- (match t.state with
      | Init -> Unseen_value value
      | Waiting_for_next_value ivar ->
        Ivar.fill_exn ivar value;
        Init
      | Unseen_value _ -> Unseen_value value)
;;

let take t =
  match t.state with
  | Init ->
    let ivar = Ivar.create () in
    t.state <- Waiting_for_next_value ivar;
    Ivar.read ivar
  | Waiting_for_next_value ivar -> Ivar.read ivar
  | Unseen_value value ->
    t.state <- Init;
    Deferred.return value
;;

let unsubscribe t =
  let unsubscribe = t.unsubscribe in
  t.state <- Init;
  t.unsubscribe <- (fun () -> ());
  unsubscribe ()
;;

let subscribe t bus =
  unsubscribe t;
  let subscriber = Bus.subscribe_exn bus ~f:(fun response -> push t response) in
  t.unsubscribe <- (fun () -> Bus.unsubscribe bus subscriber)
;;

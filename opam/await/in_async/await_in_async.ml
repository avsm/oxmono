open Core
open Async
open Await_kernel
open Portable

type 'a op = Await : Trigger.t -> unit op [@@unboxed]

module Eff = struct
  include Handled_effect.Make (struct
      type 'a t = 'a op
    end)

  let rec handle = function
    | Value value -> value
    | Exception e -> raise e
    | Operation (op, k) -> await op k

  and await : type t. t op -> (t, _, _) Handled_effect.Continuation.t @ unique -> _ =
    fun (Await trigger) k ->
    let k = Capsule.Expert.(Data.wrap_unique ~access:(Access.unbox initial)) k in
    let continue = Capsule.Initial.Data.wrap continue in
    let context = Capsule.Initial.Data.wrap (Scheduler.current_execution_context ()) in
    match
      Trigger.on_signal trigger k ~f:(fun k ->
        Async_kernel_scheduler.portable_enqueue_job context continue k)
    with
    | Null -> ()
    | This k ->
      let k = Capsule.Expert.(Data.unwrap_unique ~access:(Access.unbox initial)) k in
      handle (Handled_effect.continue k () [])

  and continue : #(_ * (unit, _, _) Handled_effect.Continuation.t) @ unique -> _ =
    fun #(_, k) -> handle (Handled_effect.continue k () [])
  ;;
end

let await handler trigger =
  Eff.perform
    ((Capsule.Initial.Data.unwrap [@mode local]) handler)
    (Await trigger) [@nontail]
;;

let yield handler =
  let trigger = Trigger.create () in
  Deferred.upon (Async_kernel_scheduler.yield ()) (fun () ->
    Trigger.Source.signal (Trigger.source trigger));
  await handler trigger
;;

module Expert = struct
  let with_await terminator ~(f : _ @ local -> unit) =
    let terminator = Terminator.Expert.globalize terminator in
    Eff.handle
      ((Eff.run [@alert "-experimental_runtime5"]) (fun handler ->
         let handler = (Capsule.Initial.Data.wrap [@mode local]) handler in
         Await.with_ ~terminator ~await ~yield:(This yield) handler ~f:(fun [@inline] w ->
           (f [@inlined hint]) w [@nontail])
         [@nontail]))
  ;;

  let with_yield ~f =
    with_await Terminator.never ~f:(fun await -> f (Yield.of_await await) [@nontail])
    [@nontail]
  ;;

  let thread_safe_spawn context action =
    Async_kernel_scheduler.thread_safe_enqueue_job context action ()
  ;;
end

let schedule_with_await ?monitor ?priority terminator ~f =
  Deferred.create (fun ivar ->
    schedule ?monitor ?priority (fun () ->
      Expert.with_await terminator ~f:(fun w ->
        match f w with
        | value -> Ivar.fill_exn ivar value
        | exception exn -> Monitor.send_exn (Monitor.current ()) exn)
      [@nontail]))
;;

let non_eager_await_deferred t deferred =
  let trigger = Trigger.create () in
  Deferred.upon deferred (fun _value -> Trigger.Source.signal (Trigger.source trigger));
  Await.await_until_terminated t trigger;
  if Deferred.is_determined deferred
  then Deferred.value_exn deferred
  else raise Await.Terminated
;;

let await_deferred t deferred =
  if Deferred.is_determined deferred
  then Deferred.value_exn deferred
  else non_eager_await_deferred t deferred
;;

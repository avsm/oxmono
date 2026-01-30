open! Base
open Async
open Await

[@@@alert "-experimental_runtime5"]

let scheduler ?monitor ?priority () =
  let open struct
    type spawn =
      { spawn :
          'r 'a.
          ('r, 'a, Capsule.Initial.k Capsule.Access.boxed) Concurrent.Scheduler.spawn_fn
      }
    [@@unboxed]
  end in
  let rec spawn
    : type r a.
      (r, a, Capsule.Initial.k Capsule.Access.boxed) Concurrent.Scheduler.spawn_fn
    =
    fun scope #{ fn; affinity = _; name = _ } r ->
    let token = Scope.add scope in
    schedule ?monitor ?priority (fun () ->
      try
        Await_in_async.Expert.with_await Terminator.never ~f:(fun await ->
          Scope.Token.use token ~f:(fun terminator task_handle ->
            let spawn = (Capsule.Initial.Data.wrap [@mode local]) { spawn } in
            Capsule.Expert.Password.with_current
              (Capsule.Access.unbox Capsule.Initial.access)
              (fun password ->
                 let scheduler =
                   (Concurrent.Scheduler.create [@alloc stack] [@mode portable])
                     ~spawn:(fun (type s b) (scope : b Scope.t @ local) task (r : s) ->
                       Capsule.Expert.access ~password ~f:(fun access ->
                         let { spawn } = Capsule.Expert.Data.Local.unwrap ~access spawn in
                         spawn scope task r [@nontail])
                       [@nontail])
                 in
                 let concurrent =
                   (Concurrent.create [@mode portable])
                     (Await.with_terminator await terminator)
                     ~scheduler
                 in
                 fn task_handle Capsule.Expert.initial concurrent r [@nontail])
            [@nontail])
          [@nontail])
        [@nontail]
      with
      | exn -> Monitor.send_exn (Monitor.current ()) exn);
    Spawned
  in
  Concurrent.Scheduler.create ~spawn
;;

let[@inline] schedule_with_concurrent ?monitor ?priority terminator ~f =
  Await_in_async.schedule_with_await
    ?monitor
    ?priority
    (Terminator.Expert.globalize terminator)
    ~f:(fun await ->
      let concurrent =
        Concurrent.create await ~scheduler:(scheduler ?monitor ?priority ())
      in
      f concurrent [@nontail])
;;

let[@inline] spawn_deferred s ~f =
  Concurrent.spawn_onto_initial s ~f:(fun s c t ->
    Await_in_async.await_deferred
      (Concurrent.await t)
      ((f [@inlined hint]) s c t) [@nontail])
  [@nontail]
;;

module Portable = struct
  let with_await = Capsule.Initial.Data.wrap Await_in_async.Expert.with_await
  let monitor_send_exn = Capsule.Initial.Data.wrap Monitor.send_exn
  let monitor_current = Capsule.Initial.Data.wrap Monitor.current

  let scheduler () =
    let execution_context =
      Async_kernel_scheduler.current_execution_context () |> Capsule.Initial.Data.wrap
    in
    let rec spawn
      : type r a.
        (r, a, Capsule.Initial.k Capsule.Access.boxed) Concurrent.Scheduler.spawn_fn
      =
      fun scope #{ fn; affinity = _; name = _ } r ->
      let token = Scope.add scope in
      Async_kernel_scheduler.portable_enqueue_job
        execution_context
        (Capsule.Expert.Data.create_once (fun () : _ ->
           fun #(access, token) ->
           let with_await = Capsule.Data.unwrap ~access with_await in
           try
             with_await Terminator.never ~f:(fun await ->
               Scope.Token.use token ~f:(fun terminator task_handle ->
                 let concurrent =
                   (Concurrent.create [@mode portable])
                     (Await.with_terminator await terminator)
                     ~scheduler:((Concurrent.Scheduler.create [@mode portable]) ~spawn)
                 in
                 fn task_handle (Capsule.Access.box access) concurrent r [@nontail])
               [@nontail])
             [@nontail]
           with
           | exn ->
             (Capsule.Data.unwrap ~access monitor_send_exn)
               ((Capsule.Data.unwrap ~access monitor_current) ())
               exn))
        (Capsule.Expert.Data.create_unique (fun () -> token));
      Spawned
    in
    (Concurrent.Scheduler.create [@mode portable]) ~spawn
  ;;
end

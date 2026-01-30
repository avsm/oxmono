open! Core
open Async
open Await

(** An implementation of concurrency that uses {!Async} to spawn concurrent tasks.

    {1 Example}

    {[
      open Async

      let result = Ivar.create ();;

      Concurrent_in_async.schedule_with_concurrent Await.Terminator.never ~f:(fun conc ->
        (* Create a new scope for concurrent tasks to be spawned in *)
        Concurrent.with_scope conc () ~f:(fun s ->
          (* [Concurrent.spawn_onto_initial] allows nonportable tasks to be spawned *)
          Concurrent.spawn_onto_initial s ~f:(fun _ _ _c ->
            Ivar.fill_exn result "hello from another concurrent task"))
        [@nontail])
    ]}

    The {{!Concurrent.with_options} spawn options} are interpreted as follows:

    - [affinity]: Ignored
    - [name]: Ignored *)

(** [schedule_with_concurrent terminator ~f] schedules [f conc] to be run on the async
    scheduler, where [conc] is an implementation of concurrency which spawns tasks onto
    the async scheduler. The tasks are given {{!Capsule.Access.t} access} to the
    {{!Capsule.Initial.k} initial} capsule, which allows them to run nonportable functions
    via {!Concurrent.spawn_onto_initial}.

    [?monitor] and [?priority], if passed, are forwarded onto {!Async.schedule} *)
val schedule_with_concurrent
  :  ?monitor:Monitor.t
  -> ?priority:Priority.t
  -> Terminator.t @ local
  -> f:(Capsule.Initial.k Capsule.Access.boxed Concurrent.t @ local -> 'a) @ once
  -> 'a Deferred.t

(** [scheduler ?monitor ?priority ()] is a handle that allows spawning concurrent tasks in
    the Async scheduler. *)
val scheduler
  :  ?monitor:Monitor.t
  -> ?priority:Priority.t
  -> unit
  -> Capsule.Initial.k Capsule.Access.boxed Concurrent.Scheduler.t

(** [spawn_deferred spawn ~f] spawns a deferred-returning task executing
    [f scope access concurrent] using [spawn] *)
val spawn_deferred
  :  ('scope_ctx, Capsule.Initial.k Capsule.Access.boxed) Concurrent.Spawn.t @ local
  -> f:
       ('scope_ctx Scope.t @ local
        -> Capsule.Initial.k Capsule.Access.boxed @ local
        -> Capsule.Initial.k Capsule.Access.boxed Concurrent.t @ local
        -> unit Deferred.t)
     @ once
  -> unit

(** Capabilities for submitting jobs to the async scheduler from other threads *)
module Portable : sig
  (** [scheduler ()] is a handle that allows spawning concurrent tasks in the Async
      scheduler from other threads. It uses the thread-safe external job queue in the
      async scheduler under the hood, so is less efficient than the non-portable
      [scheduler] if you don't need to spawn tasks from arbitrary threads.

      {b Usage example}

      {[
        open! Core
        open! Async
        open! Await

        let print_endline = Capsule.Initial.Data.wrap Async_unix.Print.print_endline

        let () =
          let scheduler = Concurrent_in_async.Portable.scheduler () in
          Mdx_async.run (fun () ->
            Concurrent_in_async.schedule_with_concurrent Terminator.never ~f:(fun conc ->
              Concurrent.with_scope
                conc
                (* Use the scheduler as our scope context to avoid allocating a closure *)
                scheduler
                ~f:(fun s ->
                  (* Spawn a thread, given the handle to the scheduler *)
                  Concurrent.spawn
                    (Concurrent_in_thread.spawn_into s)
                    ~f:(fun scope _ _ ->
                      (* Recover the scheduler *)
                      let scheduler = Concurrent.Scope.context scope in
                      (* We can now spawn tasks into the async scheduler *)
                      Concurrent.Scheduler.spawn scheduler scope ~f:(fun _ access _ ->
                        (* Those tasks get access to the initial capsule, which they can
                           use to call functions that use async *)
                        let print_endline =
                          Capsule.Data.unwrap
                            ~access:(Capsule.Access.unbox access)
                            print_endline
                        in
                        print_endline "Hello from async!")
                      [@nontail])
                  [@nontail])))
        ;;
      ]} *)
  val scheduler
    :  unit
    -> Capsule.Initial.k Capsule.Access.boxed Concurrent.Scheduler.t @ portable
end

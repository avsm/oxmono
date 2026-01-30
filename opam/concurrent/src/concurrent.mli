@@ portable

open! Base
open Await
open Types

(** [t] is the type of implementations of structured concurrency. Operations which need to
    create (optionally nested) concurrent scopes take a [t], providing them an
    implementation of concurrency to use.

    The ['concurrent_ctx] type parameter is the per-task context type - a value of type
    ['concurrent_ctx] will be passed in [@ local] as the second argument to each spawned
    task. *)
type 'concurrent_ctx t : value mod contended non_float = 'concurrent_ctx concurrent

type packed = T : 'concurrent_ctx t -> packed [@@unboxed]

(** [await t] is the implementation of awaiting associated with the implementation of
    concurrency [t] *)
val await : 'concurrent_ctx t @ local -> Await.t @ local

(** {1 Structured concurrency} *)

(** {2 Spawning and waiting on one or more tasks} *)

[%%template:
[@@@mode.default p = (portable, nonportable)]

(** [spawn_join t b f] executes [f s c t] in a concurrent task, and returns its result
    when it has finished. *)
val spawn_join
  :  'concurrent_ctx t @ local p
  -> 'scope_ctx @ portable
  -> f:
       ('scope_ctx Scope.t @ local
        -> 'concurrent_ctx @ local
        -> 'concurrent_ctx t @ local p
        -> 'a @ contended portable)
     @ once portable
  -> 'a @ contended portable

(** [spawn_join2 t b f1 f2] executes [f1 s c t] and [f2 s c t] in two concurrent tasks,
    and returns when both have finished. *)
val spawn_join2
  :  'concurrent_ctx t @ local p
  -> 'scope_ctx @ portable
  -> ('scope_ctx Scope.t @ local
      -> 'concurrent_ctx @ local
      -> 'concurrent_ctx t @ local p
      -> 'a @ contended portable)
     @ once portable
  -> ('scope_ctx Scope.t @ local
      -> 'concurrent_ctx @ local
      -> 'concurrent_ctx t @ local p
      -> 'b @ contended portable)
     @ once portable
  -> #('a * 'b) @ contended portable

(** [spawn_join3 t b f1 f2 f3] executes [f1 s c t], [f2 s c t] and [f3 s c t] in three
    concurrent tasks, and returns when all have finished. *)
val spawn_join3
  :  'concurrent_ctx t @ local p
  -> 'scope_ctx @ portable
  -> ('scope_ctx Scope.t @ local
      -> 'concurrent_ctx @ local
      -> 'concurrent_ctx t @ local p
      -> 'a @ contended portable)
     @ once portable
  -> ('scope_ctx Scope.t @ local
      -> 'concurrent_ctx @ local
      -> 'concurrent_ctx t @ local p
      -> 'b @ contended portable)
     @ once portable
  -> ('scope_ctx Scope.t @ local
      -> 'concurrent_ctx @ local
      -> 'concurrent_ctx t @ local p
      -> 'c @ contended portable)
     @ once portable
  -> #('a * 'b * 'c) @ contended portable

(** [spawn_join4 t b f1 f2 f3 f4] executes [f1 s c t], [f2 s c t], [f3 s c t] and
    [f4 s c t] in four concurrent tasks, and returns when all have finished. *)
val spawn_join4
  :  'concurrent_ctx t @ local p
  -> 'scope_ctx @ portable
  -> ('scope_ctx Scope.t @ local
      -> 'concurrent_ctx @ local
      -> 'concurrent_ctx t @ local p
      -> 'a @ contended portable)
     @ once portable
  -> ('scope_ctx Scope.t @ local
      -> 'concurrent_ctx @ local
      -> 'concurrent_ctx t @ local p
      -> 'b @ contended portable)
     @ once portable
  -> ('scope_ctx Scope.t @ local
      -> 'concurrent_ctx @ local
      -> 'concurrent_ctx t @ local p
      -> 'c @ contended portable)
     @ once portable
  -> ('scope_ctx Scope.t @ local
      -> 'concurrent_ctx @ local
      -> 'concurrent_ctx t @ local p
      -> 'd @ contended portable)
     @ once portable
  -> #('a * 'b * 'c * 'd) @ contended portable

(** [spawn_join5 t b f1 f2 f3 f4 f5] executes [f1 s c t], [f2 s c t], [f3 s c t],
    [f4 s c t] and [f5 s c t] in five concurrent tasks, and returns when all have
    finished. *)
val spawn_join5
  :  'concurrent_ctx t @ local p
  -> 'scope_ctx @ portable
  -> ('scope_ctx Scope.t @ local
      -> 'concurrent_ctx @ local
      -> 'concurrent_ctx t @ local p
      -> 'a @ contended portable)
     @ once portable
  -> ('scope_ctx Scope.t @ local
      -> 'concurrent_ctx @ local
      -> 'concurrent_ctx t @ local p
      -> 'b @ contended portable)
     @ once portable
  -> ('scope_ctx Scope.t @ local
      -> 'concurrent_ctx @ local
      -> 'concurrent_ctx t @ local p
      -> 'c @ contended portable)
     @ once portable
  -> ('scope_ctx Scope.t @ local
      -> 'concurrent_ctx @ local
      -> 'concurrent_ctx t @ local p
      -> 'd @ contended portable)
     @ once portable
  -> ('scope_ctx Scope.t @ local
      -> 'concurrent_ctx @ local
      -> 'concurrent_ctx t @ local p
      -> 'e @ contended portable)
     @ once portable
  -> #('a * 'b * 'c * 'd * 'e) @ contended portable

(** [spawn_join_n t b ~n ~f] spawns [n] concurrent tasks executing [f s c t i], where [i]
    is the 0-based index of the task, waits for them all to return, and returns an iarray
    containing the results. *)
val spawn_join_n
  :  'concurrent_ctx t @ local p
  -> 'scope_ctx @ portable
  -> n:int
  -> f:
       ('scope_ctx Scope.t @ local
        -> 'concurrent_ctx @ local
        -> 'concurrent_ctx t @ local p
        -> int
        -> 'a @ contended portable)
     @ portable
  -> 'a Iarray.t @ contended portable

(** [map t iarr s ~f] creates a new concurrent scope onto which a new task executing
    [f s c t a] is spawned for each [a] in [iarr], with [c] being the task-local value
    provided by [t]. Returns an immutable array containing the results of each [f]. *)
val map
  : ('a : value mod non_float) ('b : value mod non_float).
  'concurrent_ctx concurrent @ local p
  -> 'a Iarray.t @ portable
  -> 'scope_ctx @ portable
  -> f:
       ('scope_ctx Scope.t @ contended local
        -> 'concurrent_ctx @ local
        -> 'concurrent_ctx concurrent @ local portable
        -> 'a @ contended portable
        -> 'b @ contended portable)
     @ portable
  -> 'b Iarray.t @ contended portable

(** [iter t l s ~f] creates a new concurrent scope onto which a new task executing
    [f s c t a] is spawned for each [a] in [l], with [c] being the task-local value
    provided by [t]. *)
val iter
  :  'concurrent_ctx concurrent @ local p
  -> 'a Iarray.t @ portable
  -> 'scope_ctx @ portable
  -> f:
       ('scope_ctx Scope.t @ contended local
        -> 'concurrent_ctx @ local
        -> 'concurrent_ctx concurrent @ local portable
        -> 'a @ contended portable
        -> unit)
     @ portable
  -> unit]

(** {2 Structured concurrency with scopes} *)

module Scope = Scope

[%%template:
[@@@mode.default p = (portable, nonportable)]

(** [with_scope conc ctx ~f] calls [f] with a [Spawn.t] for spawning tasks into a new
    local scope for structured concurrency.

    The [ctx] context argument is available to spawned tasks via {!Scope.context},
    allowing threading local values to concurrently spawned tasks.

    [with_scope] does not return until all of the (non-daemon) tasks spawned into the
    scope have finished executing. If any task added to the scope raises an exception, or
    if [f] raises an exception, then [with_scope] will raise the exception that was caught
    by the scope first. *)
val with_scope
  :  'concurrent_ctx t @ local p
  -> 'scope_ctx @ portable
  -> f:(('scope_ctx, 'concurrent_ctx) spawn @ local p -> 'r) @ local once
  -> 'r]

(** [spawn s ~f] spawns a new concurrent task to execute [f scope concurrent_ctx conc]
    using [s] as the implementation of spawning.

    [scope] is the scope itself, allowing access to the context provided to [with_scope]
    via {!Scope.context}, and also spawning tasks into the parent scope via {!into_scope}.

    [concurrent_ctx] is the per-task concurrent context, defined by the implementation of
    concurrency.

    [conc] is the implementation of concurrency itself, allowing creating further nested
    scopes. *)
val spawn
  :  ('scope_ctx, 'concurrent_ctx) spawn @ local
  -> f:
       ('scope_ctx Scope.t @ local
        -> 'concurrent_ctx @ local
        -> 'concurrent_ctx t @ local portable
        -> unit)
     @ once portable
  -> unit

type 'resource spawn_result = 'resource Types.spawn_result =
  | Spawned
  | Failed of 'resource * exn @@ aliased many * Backtrace.t @@ aliased many

(** [spawn_with s ~f resource] spawns a new concurrent task to execute
    [f scope concurrent_ctx conc resource] using [s] as the implementation of spawning.

    [resource] is a unique resource that is either guaranteed to be passed to [f] or is
    returned as part of the {!Failed} {!spawn_result} to be handled by the caller.

    [scope] is the scope itself, allowing access to the context provided to [with_scope]
    via {!Scope.context}, and also spawning tasks into the parent scope via {!into_scope}.

    [concurrent_ctx] is the per-task concurrent context, defined by the implementation of
    concurrency.

    [conc] is the implementation of concurrency itself, allowing creating further nested
    scopes. *)
val spawn_with
  :  ('scope_ctx, 'concurrent_ctx) spawn @ local
  -> f:
       ('scope_ctx Scope.t @ local
        -> 'concurrent_ctx @ local
        -> 'concurrent_ctx t @ local portable
        -> 'resource @ contended once portable unique
        -> unit)
     @ once portable
  -> 'resource @ contended once portable unique
  -> 'resource spawn_result @ contended once portable unique

(** [spawn_daemon scheduler scope ~f] spawns a concurrent {i daemon} task executing [f]
    into the given scope. Daemon tasks are given a cancellation token which is canceled
    once the outer scope and all non-daemon tasks spawned into it finish. *)
val spawn_daemon
  :  ('scope_ctx, 'concurrent_ctx) spawn @ local
  -> f:
       ('scope_ctx Scope.t @ local
        -> Cancellation.t @ local
        -> 'concurrent_ctx @ local
        -> 'concurrent_ctx t @ local portable
        -> unit Or_canceled.t)
     @ once portable
  -> unit

(** [spawn_daemon'] is like [spawn_daemon], except the function given to it returns [unit]
    instead of [unit Or_canceled.t]. *)
val spawn_daemon'
  :  ('scope_ctx, 'concurrent_ctx) spawn @ local
  -> f:
       ('scope_ctx Scope.t @ local
        -> Cancellation.t @ local
        -> 'concurrent_ctx @ local
        -> 'concurrent_ctx t @ local portable
        -> unit)
     @ once portable
  -> unit

(** {3 Spawning nonportable tasks} *)

(** [spawn_nonportable ~access spawn ~f] uses [access] to a capsule to spawn a nonportable
    function [f] into an implementation of concurrency that provides access to that same
    capsule. *)
val spawn_nonportable
  :  access:'k Capsule.Access.t
  -> ('scope_ctx, 'k Capsule.Access.boxed) spawn @ local
  -> f:
       ('scope_ctx Scope.t @ local
        -> 'k Capsule.Access.boxed @ local
        -> 'k Capsule.Access.boxed t @ local
        -> unit)
     @ once
  -> unit

(** [spawn_onto_initial spawn ~f] is [spawn_nonportable Capsule.Initial.access ~f]. *)
val spawn_onto_initial
  :  ('scope_ctx, Capsule.Initial.k Capsule.Access.boxed) spawn @ local
  -> f:
       ('scope_ctx Scope.t @ local
        -> Capsule.Initial.k Capsule.Access.boxed @ local
        -> Capsule.Initial.k Capsule.Access.boxed t @ local
        -> unit)
     @ once
  -> unit
  @@ nonportable

(** {1 Configuration for tasks} *)

(** [task f] constructs, but does not spawn, a concurrent {{!Task.t} task} which will
    execute [f] when spawned

    The options [name] and [affinity] have different implementations in different
    schedulers, but generally:

    - [name] gives a human-readable name to the task for debugging purposes
    - [affinity] indicates which worker the task should execute on, modulo the total
      number of workers in the scheduler. *)
val task : ?name:string -> ?affinity:int -> 'f @ once portable -> 'f task @ once portable

(** [nonportable_task] is like [task], except the function it expects does not have to be
    portable. *)
val nonportable_task : ?name:string -> ?affinity:int -> 'f @ once -> 'f task @ once

module (Task @@ nonportable) : sig @@ portable
  (** ['f Task.t] is a description of a concurrent task *)
  type 'f t : (immutable_data & immutable_data & immutable_data) with 'f = 'f task =
    #{ fn : 'f
     ; name : string or_null
     ; affinity : int or_null
     }

  (** [spawn s task] spawns the given [task] using [s] as the implementation of spawning. *)
  val spawn
    :  ('scope_ctx, 'concurrent_ctx) spawn @ local
    -> ('scope_ctx Scope.t @ local
        -> ('concurrent_ctx @ local
            -> ('concurrent_ctx concurrent @ local portable -> unit) @ local once)
           @ local once)
         t
       @ once portable
    -> unit

  (** [spawn_with s task resource] spawns a new concurrent task using [s] as the
      implementation of spawning. *)
  val spawn_with
    :  ('scope_ctx, 'concurrent_ctx) spawn @ local
    -> ('scope_ctx Scope.t @ local
        -> ('concurrent_ctx @ local
            -> ('concurrent_ctx concurrent @ local portable
                -> ('resource @ contended once portable unique -> unit) @ local once)
               @ local once)
           @ local once)
         t
       @ once portable
    -> 'resource @ contended once portable unique
    -> 'resource spawn_result @ contended once portable unique

  (** [spawn_daemon scheduler scope task] spawns a concurrent {i daemon} task executing
      [task] into the given scope. Daemon tasks are given a cancellation token which is
      canceled once the outer scope and all non-daemon tasks spawned into it finish. *)
  val spawn_daemon
    :  ('scope_ctx, 'concurrent_ctx) spawn @ local
    -> ('scope_ctx Scope.t @ local
        -> (Cancellation.t @ local
            -> ('concurrent_ctx @ local
                -> ('concurrent_ctx concurrent @ local portable -> unit Or_canceled.t)
                   @ local once)
               @ local once)
           @ local once)
         t
       @ once portable
    -> unit

  (** [spawn_daemon'] is like [spawn_daemon], except the task function it expects returns
      [unit] instead of [unit Or_canceled.t]. *)
  val spawn_daemon'
    :  ('scope_ctx, 'concurrent_ctx) spawn @ local
    -> ('scope_ctx Scope.t @ local
        -> (Cancellation.t @ local
            -> ('concurrent_ctx @ local
                -> ('concurrent_ctx concurrent @ local portable -> unit) @ local once)
               @ local once)
           @ local once)
         t
       @ once portable
    -> unit

  [%%template:
  [@@@mode.default p = (portable, nonportable)]

  (** [map t iarr s ~f] creates a new concurrent scope onto which a new task executing the
      [Task.t] returned by [f a] is spawned for each [a] in [iarr]. Returns an immutable
      array containing the results of each [task]. *)
  val map
    : ('a : value mod non_float) ('b : value mod non_float).
    'concurrent_ctx concurrent @ local p
    -> 'a Iarray.t @ portable
    -> 'scope_ctx @ portable
    -> f:
         ('a @ portable
          -> ('scope_ctx Scope.t @ contended local
              -> ('concurrent_ctx @ local
                  -> ('concurrent_ctx concurrent @ local portable
                      -> 'b @ contended portable)
                     @ local once)
                 @ local once)
               t
             @ once portable)
    -> 'b Iarray.t @ contended portable

  (** [iter t iarr s ~f] creates a new concurrent scope onto which a new task executing
      the [Task.t] returned by [f a] is spawned for each [a] in [iarr] *)
  val iter
    :  'concurrent_ctx concurrent @ local p
    -> 'a Iarray.t @ portable
    -> 'scope_ctx @ portable
    -> f:
         ('a @ portable
          -> ('scope_ctx Scope.t @ contended local
              -> ('concurrent_ctx @ local
                  -> ('concurrent_ctx concurrent @ local portable -> unit) @ local once)
                 @ local once)
               t
             @ once portable)
    -> unit

  (** [spawn_join t b task] executes the given [task] concurrently, and returns its result
      when it has finished. *)
  val spawn_join
    :  'concurrent_ctx concurrent @ local p
    -> 'scope_ctx @ portable
    -> ('scope_ctx Scope.t @ local
        -> ('concurrent_ctx @ local
            -> ('concurrent_ctx concurrent @ local p -> 'a @ contended portable)
               @ local once)
           @ local once)
         t
       @ once portable
    -> 'a @ contended portable

  (** [spawn_join2 t b task1 task2] executes [task1] and [task2] concurrently, and returns
      an unboxed tuple containing the results of the tasks when both have finished. *)
  val spawn_join2
    :  'concurrent_ctx concurrent @ local p
    -> 'scope_ctx @ portable
    -> ('scope_ctx Scope.t @ local
        -> ('concurrent_ctx @ local
            -> ('concurrent_ctx concurrent @ local p -> 'a @ contended portable)
               @ local once)
           @ local once)
         t
       @ once portable
    -> ('scope_ctx Scope.t @ local
        -> ('concurrent_ctx @ local
            -> ('concurrent_ctx concurrent @ local p -> 'b @ contended portable)
               @ local once)
           @ local once)
         t
       @ once portable
    -> #('a * 'b) @ contended portable

  (** [spawn_join3 t b task1 task2 task3] executes [task1], [task2], and [task3]
      concurrently, and returns an unboxed tuple containing the results of the tasks when
      all three have finished. *)
  val spawn_join3
    :  'concurrent_ctx concurrent @ local p
    -> 'scope_ctx @ portable
    -> ('scope_ctx Scope.t @ local
        -> ('concurrent_ctx @ local
            -> ('concurrent_ctx concurrent @ local p -> 'a @ contended portable)
               @ local once)
           @ local once)
         t
       @ once portable
    -> ('scope_ctx Scope.t @ local
        -> ('concurrent_ctx @ local
            -> ('concurrent_ctx concurrent @ local p -> 'b @ contended portable)
               @ local once)
           @ local once)
         t
       @ once portable
    -> ('scope_ctx Scope.t @ local
        -> ('concurrent_ctx @ local
            -> ('concurrent_ctx concurrent @ local p -> 'c @ contended portable)
               @ local once)
           @ local once)
         t
       @ once portable
    -> #('a * 'b * 'c) @ contended portable

  (** [spawn_join4 t b task1 task2 task3 task4] executes [task1], [task2], [task3], and
      [task4] concurrently, and returns an unboxed tuple containing the results of the
      tasks when all four have finished. *)
  val spawn_join4
    :  'concurrent_ctx concurrent @ local p
    -> 'scope_ctx @ portable
    -> ('scope_ctx Scope.t @ local
        -> ('concurrent_ctx @ local
            -> ('concurrent_ctx concurrent @ local p -> 'a @ contended portable)
               @ local once)
           @ local once)
         t
       @ once portable
    -> ('scope_ctx Scope.t @ local
        -> ('concurrent_ctx @ local
            -> ('concurrent_ctx concurrent @ local p -> 'b @ contended portable)
               @ local once)
           @ local once)
         t
       @ once portable
    -> ('scope_ctx Scope.t @ local
        -> ('concurrent_ctx @ local
            -> ('concurrent_ctx concurrent @ local p -> 'c @ contended portable)
               @ local once)
           @ local once)
         t
       @ once portable
    -> ('scope_ctx Scope.t @ local
        -> ('concurrent_ctx @ local
            -> ('concurrent_ctx concurrent @ local p -> 'd @ contended portable)
               @ local once)
           @ local once)
         t
       @ once portable
    -> #('a * 'b * 'c * 'd) @ contended portable

  (** [spawn_join4 t b task1 task2 task3 task4 task5] executes [task1], [task2], [task3],
      [task4], and [task5] concurrently, and returns an unboxed tuple containing the
      results of the tasks when all five have finished. *)
  val spawn_join5
    :  'concurrent_ctx concurrent @ local p
    -> 'scope_ctx @ portable
    -> ('scope_ctx Scope.t @ local
        -> ('concurrent_ctx @ local
            -> ('concurrent_ctx concurrent @ local p -> 'a @ contended portable)
               @ local once)
           @ local once)
         t
       @ once portable
    -> ('scope_ctx Scope.t @ local
        -> ('concurrent_ctx @ local
            -> ('concurrent_ctx concurrent @ local p -> 'b @ contended portable)
               @ local once)
           @ local once)
         t
       @ once portable
    -> ('scope_ctx Scope.t @ local
        -> ('concurrent_ctx @ local
            -> ('concurrent_ctx concurrent @ local p -> 'c @ contended portable)
               @ local once)
           @ local once)
         t
       @ once portable
    -> ('scope_ctx Scope.t @ local
        -> ('concurrent_ctx @ local
            -> ('concurrent_ctx concurrent @ local p -> 'd @ contended portable)
               @ local once)
           @ local once)
         t
       @ once portable
    -> ('scope_ctx Scope.t @ local
        -> ('concurrent_ctx @ local
            -> ('concurrent_ctx concurrent @ local p -> 'e @ contended portable)
               @ local once)
           @ local once)
         t
       @ once portable
    -> #('a * 'b * 'c * 'd * 'e) @ contended portable

  (** [spawn_join_n t b ~n ~f] spawns [n] concurrent tasks executing the [Task.t]s
      returned by [f i], where [i] is the 0-based index of the task, waits for them all to
      return, and returns an iarray containing the results. *)
  val spawn_join_n
    :  'concurrent_ctx concurrent @ local p
    -> 'scope_ctx @ portable
    -> n:int
    -> f:
         (int
          -> ('scope_ctx Scope.t @ local
              -> ('concurrent_ctx @ local
                  -> ('concurrent_ctx concurrent @ local p -> 'a @ contended portable)
                     @ local once)
                 @ local once)
               t
             @ once portable)
       @ portable
    -> 'a Iarray.t @ contended portable]

  (** [spawn_nonportable ~access spawn task] uses [access] to a capsule to spawn a
      nonportable task [task] into an implementation of concurrency that provides access
      to that same capsule. *)
  val spawn_nonportable
    :  access:'k Capsule.Access.t
    -> ('scope_ctx, 'k Capsule.Access.boxed) spawn @ local
    -> ('scope_ctx Scope.t @ local
        -> ('k Capsule.Access.boxed @ local
            -> ('k Capsule.Access.boxed concurrent @ local -> unit) @ local once)
           @ local once)
         t
       @ once
    -> unit

  (** [spawn_onto_initial spawn task] is [spawn_nonportable Capsule.Initial.access task]. *)
  val spawn_onto_initial
    :  ('scope_ctx, Capsule.Initial.k Capsule.Access.boxed) spawn @ local
    -> ('scope_ctx Scope.t @ local
        -> (Capsule.Initial.k Capsule.Access.boxed @ local
            -> (Capsule.Initial.k Capsule.Access.boxed concurrent @ local -> unit)
               @ local once)
           @ local once)
         t
       @ once
    -> unit
    @@ nonportable
end

(** {1 Lower-level internals} *)

(** {2 Direct handles to the concurrent scheduler} *)

(** [scheduler t] is the scheduler associated with the implementation of concurrency [t]. *)
val scheduler : 'ctx t @ local -> 'ctx scheduler @ local

module Scheduler : sig
  type 'ctx concurrent := 'ctx t

  type ('resource, 'scope_ctx, 'concurrent_ctx) spawn_fn =
    ('resource, 'scope_ctx, 'concurrent_ctx) Types.spawn_fn

  (** [Scheduler.t] is the type representing a handle to a concurrent scheduler. A handle
      to the scheduler allows spawning unstructured concurrent tasks. *)
  type 'ctx t : value mod aliased contended non_float = 'ctx scheduler

  type packed = T : 'ctx t -> packed [@@unboxed]

  (** [create ~spawn] creates a new scheduler with the given spawn function. *)
  val%template create
    : 'concurrent_ctx.
    spawn:('resource 'scope_ctx. ('resource, 'scope_ctx, 'concurrent_ctx) spawn_fn) @ l p
    -> 'concurrent_ctx t @ l p
  [@@alloc __ @ l = (heap_global, stack_local)] [@@mode p = (portable, nonportable)]

  (** [spawn scheduler scope task] spawns a new concurrent task into the given [scope] to
      execute the given [task] using the scheduler [scheduler]. *)
  val spawn
    :  'concurrent_ctx t @ local
    -> 'scope_ctx Scope.t @ local
    -> ('scope_ctx Scope.t @ local
        -> ('concurrent_ctx @ local
            -> ('concurrent_ctx concurrent @ local portable -> unit) @ local once)
           @ local once)
         Task.t
       @ once portable
    -> unit

  (** [spawn_with scheduler task resource] spawns a new concurrent task to execute the
      given [task], using the scheduler [scheduler] providing it with some unique
      [resource]

      The [resource] is guaranteed to either be passed to [f], or returned as part of the
      {!Failed} {!spawn_result} to be handled by the caller. *)
  val spawn_with
    :  'concurrent_ctx t @ local
    -> 'scope_ctx Scope.t @ local
    -> ('scope_ctx Scope.t @ local
        -> ('concurrent_ctx @ local
            -> ('concurrent_ctx concurrent @ local portable
                -> ('resource @ contended once portable unique -> unit) @ local once)
               @ local once)
           @ local once)
         Task.t
       @ once portable
    -> 'resource @ contended once portable unique
    -> 'resource spawn_result @ contended once portable unique

  (** [spawn_daemon scheduler scope task] spawns a concurrent {i daemon} task executing
      [f] into the given scope. Daemon tasks are given a cancellation token which is
      canceled once the outer scope and all non-daemon tasks spawned into it finish. *)
  val spawn_daemon
    :  'concurrent_ctx t @ local
    -> 'scope_ctx Scope.t @ local
    -> ('scope_ctx Scope.t @ local
        -> (Cancellation.t @ local
            -> ('concurrent_ctx @ local
                -> ('concurrent_ctx concurrent @ local portable -> unit Or_canceled.t)
                   @ local once)
               @ local once)
           @ local once)
         Task.t
       @ once portable
    -> unit

  (** [spawn_daemon'] is like [spawn_daemon], except the task function it expects returns
      [unit] instead of [unit Or_canceled.t]. *)
  val spawn_daemon'
    :  'concurrent_ctx t @ local
    -> 'scope_ctx Scope.t @ local
    -> ('scope_ctx Scope.t @ local
        -> (Cancellation.t @ local
            -> ('concurrent_ctx @ local
                -> ('concurrent_ctx concurrent @ local portable -> unit) @ local once)
               @ local once)
           @ local once)
         Task.t
       @ once portable
    -> unit
end

(** {2 The [Spawn] capability} *)

module Spawn : sig
  type 'ctx concurrent := 'ctx t

  (** [t] is the type representing the ability to spawn concurrent tasks with a particular
      implementation of concurrency, into a particular {!Scope.t}. Operations that need to
      spawn tasks into a containing scope take a [t] that provides an implementation of
      spawning for them to use, and call {!spawn} to spawn tasks onto the scope.

      There are two type parameters:

      1. ['scope_ctx] is the type of the context associated with the containing
         {!Scope.t}, and is passed to all spawned tasks [@ contended local].
      2. ['concurrent_ctx] is the type of the context associated with the {!Concurrent.t}
         that the {!Spawn.t} is associated with, if any, and is passed to all spawned
         tasks [@ local] *)
  type ('scope_ctx, 'concurrent_ctx) t : value mod contended non_float =
    ('scope_ctx, 'concurrent_ctx) spawn

  type 'scope_ctx packed = T : ('scope_ctx, 'concurrent_ctx) t -> 'scope_ctx packed
  [@@unboxed]

  [%%template:
  [@@@mode.default p = (portable, nonportable)]

  (** [create concurrent ~scope] creates a new capability value providing the ability to
      [spawn] concurrent tasks using the given implementation of concurrency into the
      given [scope]. *)
  val create
    : 'scope_ctx 'concurrent_ctx.
    'concurrent_ctx concurrent @ local p
    -> scope:'scope_ctx Scope.t @ local
    -> ('scope_ctx, 'concurrent_ctx) t @ local p]

  (** [with_scheduler s scheduler] is a capability providing the ability to [spawn] tasks
      guarded by the scope associated with [s], but running on the concurrent scheduler
      [scheduler] *)
  val with_scheduler
    :  ('scope_ctx, _) t @ local
    -> 'concurrent_ctx Scheduler.t @ local
    -> ('scope_ctx, 'concurrent_ctx) t @ local

  (** [concurrent t] is the implementation of concurrency associated with [t]. *)
  val concurrent
    :  ('scope_ctx, 'concurrent_ctx) t @ local
    -> 'concurrent_ctx concurrent @ local

  (** [await t] is [Concurrent.await (concurrent t)]. *)
  val await : (_, _) t @ local -> Await.t @ local

  (** [scheduler t] is [Concurrent.scheduler (concurrent t)]. *)
  val scheduler : (_, 'concurrent_ctx) t @ local -> 'concurrent_ctx Scheduler.t @ local

  (** [scope t] is the {!Scope} associated with [t]. *)
  val scope : ('scope_ctx, 'concurrent_ctx) t @ local -> 'scope_ctx Scope.t @ local

  (** [context t] is [Scope.context (scope t)]. *)
  val context : ('scope_ctx, _) t @ local -> 'scope_ctx @ contended local portable

  (** [terminator t] is [Scope.terminator (scope t)]. *)
  val terminator : (_, _) t @ local -> Terminator.t @ local
end

(** {2 Constructing {!Concurrent.t}s} *)

[%%template:
[@@@mode.default p = (portable, nonportable)]

(** [create await ~scheduler] creates a new implementation of concurrency, given an
    implementation of awaiting and a handle to a scheduler. *)
val create
  :  Await.t @ local p
  -> scheduler:'concurrent_ctx Scheduler.t @ local p
  -> 'concurrent_ctx t @ local p

(** [into_scope t scope] is a [Spawn.t] which spawns tasks into the given scope, using [t]
    as the implementation of concurrency. *)
val into_scope
  :  'concurrent_ctx t @ local p
  -> 'scope_ctx Scope.t @ local
  -> ('scope_ctx, 'concurrent_ctx) Spawn.t @ local p]

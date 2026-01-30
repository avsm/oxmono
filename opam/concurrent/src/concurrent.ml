open Base
open Await
module Scope = Scope
open Types

type 'concurrent_ctx t = 'concurrent_ctx concurrent =
  { await : Await.t
  ; scheduler : 'concurrent_ctx scheduler
  }
[@@deriving fields ~getters]

module Spawn = struct
  type ('scope_ctx, 'concurrent_ctx) t = ('scope_ctx, 'concurrent_ctx) spawn =
    { scope : 'scope_ctx Scope.t
    ; concurrent : 'concurrent_ctx concurrent
    }
  [@@deriving fields ~getters]

  type 'scope_ctx packed = T : ('scope_ctx, 'concurrent_ctx) t -> 'scope_ctx packed
  [@@unboxed]

  let%template create concurrent ~scope = exclave_ { scope; concurrent }
  [@@mode p = (portable, nonportable)]
  ;;

  let with_scheduler { scope; concurrent } scheduler = exclave_
    { scope; concurrent = { concurrent with scheduler } }
  ;;

  let await t = exclave_ await (concurrent t)
  let scheduler t = exclave_ scheduler (concurrent t)
  let context { scope; _ } = exclave_ Scope.context scope
  let terminator { scope; _ } = exclave_ Scope.terminator scope
end

[%%template
[@@@mode.default p = (portable, nonportable)]

let create await ~scheduler = exclave_ { await; scheduler }
let into_scope concurrent scope = exclave_ Spawn.(create [@mode p]) concurrent ~scope

let with_scope t b ~f =
  Scope.with_ t.await b ~f:(fun scope -> f ((into_scope [@mode p]) t scope) [@nontail])
  [@nontail]
;;]

module Task0 = struct
  type 'f t = 'f task =
    #{ fn : 'f
     ; name : string or_null
     ; affinity : int or_null
     }

  let[@inline] map_fn #{ fn; name; affinity } f = #{ fn = f fn; name; affinity }
end

let[@inline] task ?name ?affinity f =
  #{ Task0.fn = f; name = Or_null.of_option name; affinity = Or_null.of_option affinity }
;;

let[@inline] nonportable_task ?name ?affinity f =
  #{ Task0.fn = f; name = Or_null.of_option name; affinity = Or_null.of_option affinity }
;;

module Scheduler = struct
  type ('resource, 'scope_ctx, 'concurrent_ctx) spawn_fn =
    ('resource, 'scope_ctx, 'concurrent_ctx) Types.spawn_fn

  type 'ctx t = 'ctx scheduler =
    { spawn : 'resource 'scope_ctx. ('resource, 'scope_ctx, 'ctx) spawn_fn }
  [@@unboxed] [@@deriving fields ~getters]

  type packed = T : 'ctx t -> packed [@@unboxed]

  let%template create
    ~(spawn : 'resource 'scope_ctx. ('resource, 'scope_ctx, _) spawn_fn @ l)
    =
    { spawn } [@exclave_if_stack a]
  [@@alloc a @ l = (heap_global, stack_local)] [@@mode p = (portable, nonportable)]
  ;;

  let spawn_daemon' t scope task =
    match
      spawn
        t
        scope
        (Task0.map_fn task (fun fn ->
           fun [@inline] h b t () ->
           let #(s, c) = Scope.Task_handle.become_daemon h in
           fn s c b t [@nontail]))
        ()
    with
    | Spawned -> ()
    | Failed ((), exn, bt) -> Exn.raise_with_original_backtrace exn bt
  ;;

  let spawn_daemon t scope task =
    match
      spawn
        t
        scope
        (Task0.map_fn task (fun fn ->
           fun [@inline] h b t () ->
           let #(s, c) = Scope.Task_handle.become_daemon h in
           ignore (fn s c b t : unit Or_canceled.t)))
        ()
    with
    | Spawned -> ()
    | Failed ((), exn, bt) -> Exn.raise_with_original_backtrace exn bt
  ;;

  let spawn_with t scope task r =
    spawn
      t
      scope
      (Task0.map_fn task (fun fn ->
         fun [@inline] h b t r -> fn (Scope.Task_handle.into_scope h) b t r [@nontail]))
      r [@nontail]
  ;;

  let spawn t scope task =
    match
      spawn_with
        t
        scope
        (Task0.map_fn task (fun fn -> fun [@inline] s c t () -> fn s c t))
        ()
    with
    | Spawned -> ()
    | Failed ((), exn, bt) -> Exn.raise_with_original_backtrace exn bt
  ;;
end

(** Module for (unsafely!) recording the result(s) of a (set of) concurrent task(s) in a
    scope, and accessing those result(s) after the scope ends.

    This module is very unsafe! See SAFETY comments on each function for more of the
    contract callers later in this module must follow *)
module Unsafe_result : sig @@ portable
  type 'a t : value mod contended portable

  val make : unit -> 'a t

  (** SAFETY: This function is unsafe to call without ensuring that no other threads are
      calling either it or [racy_get]. *)
  val racy_fill : 'a t -> 'a @ contended portable -> unit

  (** SAFETY: This function is unsafe to call without ensuring that [racy_fill] has been
      called {i before} it is called. *)
  val racy_get : 'a t -> 'a @ contended portable

  module Array : sig
    type 'a t : value mod contended portable

    val make : len:int -> 'a t

    (** SAFETY: This function is unsafe to call:

        - With an index that is out-of-bounds for the array
        - Concurrently with any other threads calling [racy_fill] on the same index, or
          calling [racy_get] at all *)
    val racy_fill : 'a t -> int -> 'a @ contended portable -> unit

    (** SAFETY: This function is unsafe to call without ensuring that {i all} indices of
        the array have been filled by [racy_fill] {i before} it is called *)
    val racy_get : 'a t -> 'a Iarray.t @ contended portable
  end
end = struct
  type 'a t : value mod contended portable =
    { mutable contents : 'a or_null @@ contended portable }
  [@@unsafe_allow_any_mode_crossing (* See SAFETY comments in the interface *)]

  let make () = { contents = Null }
  let racy_fill t a = t.contents <- This a

  external unsafe_assume_init
    :  'a or_null @ contended portable
    -> 'a @ contended portable
    @@ portable
    = "%identity"

  let racy_get t = unsafe_assume_init t.contents

  module Array = struct
    type 'a t : value mod contended portable =
      { array : 'a portended or_null Uniform_array.t }
    [@@unboxed]
    [@@unsafe_allow_any_mode_crossing (* See SAFETY comments in the interface *)]

    let make ~len = { array = Uniform_array.create ~len Null }

    let racy_fill { array } i a =
      Uniform_array.unsafe_set array i (This { portended = a })
    ;;

    external unsafe_assume_init
      :  'a t
      -> 'a Iarray.t @ contended portable
      @@ portable
      = "%obj_magic"

    let racy_get t = unsafe_assume_init t
  end
end

module Task = struct
  include Task0

  let spawn { scope; concurrent } task = Scheduler.spawn concurrent.scheduler scope task

  let spawn_with { scope; concurrent } task r =
    Scheduler.spawn_with concurrent.scheduler scope task r
  ;;

  let spawn_daemon { scope; concurrent } task =
    Scheduler.spawn_daemon concurrent.scheduler scope task
  ;;

  let spawn_daemon' { scope; concurrent } task =
    Scheduler.spawn_daemon' concurrent.scheduler scope task
  ;;

  [%%template
  [@@@mode.default p = (portable, nonportable)]

  (* SAFETY:

     For the following functions, the safety depends on the properties of [with_scope].
     Notably:

     - Each function [spawn]ed into a scope either runs to completion, or raises
     - If any function is [spawn]ed into a scope, the entire scope raises
     - [with_scope] does not return until all functions [spawn]ed into the scope return or
       raise.

     In each spawn_join function, iter, and map, we must ensure:
     - each result (either [Unsafe_result.t] or, in the case of [map],
       [Unsafe_result.Array.t]) is filled within a task spawned into the scope
     - We don't call [racy_get] until after the scope is finished
  *)

  let[@inline] racy_wrap_task result (task : _ t) =
    map_fn task (fun fn ->
      fun [@inline] s c t -> Unsafe_result.racy_fill result ((fn [@inlined hint]) s c t))
  ;;

  let spawn_join t b task =
    let result = Unsafe_result.make () in
    (with_scope [@mode p]) t b ~f:(fun s ->
      spawn s ((racy_wrap_task [@mode p]) result task));
    Unsafe_result.racy_get result
  ;;

  let spawn_join2 t b task1 task2 =
    let result1 = Unsafe_result.make () in
    let result2 = Unsafe_result.make () in
    (with_scope [@mode p]) t b ~f:(fun s ->
      spawn s ((racy_wrap_task [@mode p]) result1 task1);
      spawn s ((racy_wrap_task [@mode p]) result2 task2));
    #(Unsafe_result.racy_get result1, Unsafe_result.racy_get result2)
  ;;

  let spawn_join3 t b task1 task2 task3 =
    let result1 = Unsafe_result.make () in
    let result2 = Unsafe_result.make () in
    let result3 = Unsafe_result.make () in
    (with_scope [@mode p]) t b ~f:(fun s ->
      spawn s ((racy_wrap_task [@mode p]) result1 task1);
      spawn s ((racy_wrap_task [@mode p]) result2 task2);
      spawn s ((racy_wrap_task [@mode p]) result3 task3));
    #( Unsafe_result.racy_get result1
     , Unsafe_result.racy_get result2
     , Unsafe_result.racy_get result3 )
  ;;

  let spawn_join4 t b task1 task2 task3 task4 =
    let result1 = Unsafe_result.make () in
    let result2 = Unsafe_result.make () in
    let result3 = Unsafe_result.make () in
    let result4 = Unsafe_result.make () in
    (with_scope [@mode p]) t b ~f:(fun s ->
      spawn s ((racy_wrap_task [@mode p]) result1 task1);
      spawn s ((racy_wrap_task [@mode p]) result2 task2);
      spawn s ((racy_wrap_task [@mode p]) result3 task3);
      spawn s ((racy_wrap_task [@mode p]) result4 task4));
    #( Unsafe_result.racy_get result1
     , Unsafe_result.racy_get result2
     , Unsafe_result.racy_get result3
     , Unsafe_result.racy_get result4 )
  ;;

  let spawn_join5 t b task1 task2 task3 task4 task5 =
    let result1 = Unsafe_result.make () in
    let result2 = Unsafe_result.make () in
    let result3 = Unsafe_result.make () in
    let result4 = Unsafe_result.make () in
    let result5 = Unsafe_result.make () in
    (with_scope [@mode p]) t b ~f:(fun s ->
      spawn s ((racy_wrap_task [@mode p]) result1 task1);
      spawn s ((racy_wrap_task [@mode p]) result2 task2);
      spawn s ((racy_wrap_task [@mode p]) result3 task3);
      spawn s ((racy_wrap_task [@mode p]) result4 task4);
      spawn s ((racy_wrap_task [@mode p]) result5 task5));
    #( Unsafe_result.racy_get result1
     , Unsafe_result.racy_get result2
     , Unsafe_result.racy_get result3
     , Unsafe_result.racy_get result4
     , Unsafe_result.racy_get result5 )
  ;;

  let iter t iarr c ~f =
    (with_scope [@mode p]) t c ~f:(fun s ->
      for idx = 0 to Iarray.length iarr - 1 do
        let a = (Iarray.unsafe_get [@mode portable]) iarr idx in
        let task = f a in
        spawn s task
      done)
  ;;

  let map t iarr c ~f =
    let len = Iarray.length iarr in
    if len = 0
    then [::]
    else (
      let results = Unsafe_result.Array.make ~len in
      (with_scope [@mode p]) t c ~f:(fun s ->
        for idx = 0 to len - 1 do
          let a = (Iarray.unsafe_get [@mode portable]) iarr idx in
          let task = f a in
          spawn
            s
            (Task0.map_fn task (fun fn ->
               fun [@inline] s c t ->
               let result = (fn [@inlined hint]) s c t in
               Unsafe_result.Array.racy_fill results idx result))
        done);
      Unsafe_result.Array.racy_get results)
  ;;

  let spawn_join_n t b ~n ~f =
    if n = 0
    then [::]
    else (
      let results = Unsafe_result.Array.make ~len:n in
      (with_scope [@mode p]) t b ~f:(fun s ->
        for i = 0 to n - 1 do
          let task = f i in
          spawn
            s
            (Task0.map_fn task (fun fn ->
               fun [@inline] s c t ->
               let result = (fn [@inlined hint]) s c t in
               Unsafe_result.Array.racy_fill results i result))
        done);
      Unsafe_result.Array.racy_get results)
  ;;]

  let spawn_nonportable ~access s t =
    let #{ fn; affinity; name } = t in
    spawn
      s
      #{ Task0.fn =
           (let fn = Capsule.Expert.Data.wrap_once ~access fn in
            fun [@inline] ctx access conc ->
              let fn =
                Capsule.Expert.Data.unwrap_once ~access:(Capsule.Access.unbox access) fn
              in
              fn ctx access conc)
       ; affinity
       ; name
       }
  ;;

  let spawn_onto_initial s t =
    spawn_nonportable ~access:Capsule.(Access.unbox Initial.access) s t
  ;;
end

type packed = T : 'concurrent_ctx concurrent -> packed [@@unboxed]

type 'resource spawn_result = 'resource Types.spawn_result =
  | Spawned
  | Failed of 'resource * exn @@ aliased many * Backtrace.t @@ aliased many

let spawn s ~f = Task.spawn s (task f)
let spawn_with s ~f r = Task.spawn_with s (task f) r
let spawn_daemon s ~f = Task.spawn_daemon s (task f)
let spawn_daemon' s ~f = Task.spawn_daemon' s (task f)

let spawn_nonportable ~access s ~f =
  let f = Capsule.Expert.Data.wrap_once ~access f in
  spawn s ~f:(fun ctx access conc ->
    let f = Capsule.Expert.Data.unwrap_once ~access:(Capsule.Access.unbox access) f in
    f ctx access conc [@nontail])
  [@nontail]
;;

let spawn_onto_initial s ~f =
  spawn_nonportable ~access:(Capsule.Access.unbox Capsule.Expert.initial) s ~f
;;

[%%template
[@@@mode.default p = (portable, nonportable)]

let spawn_join t c ~f = (Task.spawn_join [@mode p]) t c (task f)
let spawn_join2 t c f1 f2 = (Task.spawn_join2 [@mode p]) t c (task f1) (task f2)

let spawn_join3 t c f1 f2 f3 =
  (Task.spawn_join3 [@mode p]) t c (task f1) (task f2) (task f3)
;;

let spawn_join4 t c f1 f2 f3 f4 =
  (Task.spawn_join4 [@mode p]) t c (task f1) (task f2) (task f3) (task f4)
;;

let spawn_join5 t c f1 f2 f3 f4 f5 =
  (Task.spawn_join5 [@mode p]) t c (task f1) (task f2) (task f3) (task f4) (task f5)
;;

let spawn_join_n t c ~n ~f =
  (Task.spawn_join_n [@mode p]) t c ~n ~f:(fun [@inline] n ->
    task (fun [@inline] s c t -> (f [@inlined hint]) s c t n))
;;

let map t l c ~f =
  (Task.map [@mode p]) t l c ~f:(fun a ->
    task (fun [@inline] s c t -> (f [@inlined hint]) s c t a))
;;

let iter t l c ~f =
  (Task.iter [@mode p]) t l c ~f:(fun a ->
    task (fun [@inline] s c t -> (f [@inlined hint]) s c t a))
;;]

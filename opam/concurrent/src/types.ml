open Base
open Await
module Scope = Scope

type 'resource spawn_result =
  | Spawned
  | Failed of 'resource * exn @@ aliased many * Backtrace.t @@ aliased many

type 'f task =
  #{ fn : 'f
   ; name : string or_null
   ; affinity : int or_null
   }

type ('resource, 'scope_ctx, 'concurrent_ctx) spawn_fn =
  'scope_ctx Scope.t @ local
  -> ('scope_ctx Scope.Task_handle.t @ local unique
      -> ('concurrent_ctx @ local
          -> ('concurrent_ctx concurrent @ local portable
              -> ('resource @ contended once portable unique -> unit) @ local once)
             @ local once)
         @ local once)
       task
     @ once portable
  -> 'resource @ contended once portable unique
  -> 'resource spawn_result @ contended once portable unique

and 'concurrent_ctx concurrent =
  { await : Await.t
  ; scheduler : 'concurrent_ctx scheduler
  }
[@@deriving fields ~getters]

and 'ctx scheduler =
  { spawn : 'resource 'scope_ctx. ('resource, 'scope_ctx, 'ctx) spawn_fn }
[@@unboxed]

and ('scope_ctx, 'concurrent_ctx) spawn =
  { scope : 'scope_ctx Scope.t
  ; concurrent : 'concurrent_ctx concurrent
  }

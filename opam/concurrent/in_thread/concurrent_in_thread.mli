@@ portable

open Base
open Await

(** An implementation of concurrency that spawns tasks as systhreads onto domains.

    The {{!Concurrent.with_options} spawn options} are interpreted as follows:

    - [affinity]: Selects the domain to spawn onto, modulo the {{!Multicore.max_domains}
      maximum number of domains}.
    - [name]: Sets the name of the spawned thread using [pthread_setname_np]. Names are
      limited to 15 characters on Linux - any longer names will be truncated. *)

(** [scheduler] is a concurrent scheduler which spawns tasks as systhreads onto domains,
    using [await] as the implementation of awaiting *)
val scheduler : unit Concurrent.Scheduler.t

(** [create await] is an implementation of concurrency which spawns tasks as systhreads
    onto domains, using [await] as the implementation of awaiting *)
val create : Await.t @ local portable -> unit Concurrent.t @ local portable

(** [with_blocking terminator ~f] calls [f] with an implementation of concurrency that
    spawns preemptively scheduled threads onto arbitrary (managed) domains.

    All scopes created using this implementation of concurrency block the calling thread
    using {!Await_blocking}. If you rely on this thread to run the [Async] scheduler,
    instead call {!create} with an implementation of awaiting from {!Await_in_async}. *)
val with_blocking
  :  Terminator.t @ local
  -> f:(unit Concurrent.t @ local portable -> 'r) @ local once
  -> 'r

(** [spawn_into spawn] is a {!Concurrent.Spawn.t} which spawns task into the scope
    associated with the given [spawn] as threads in arbitrary (managed) domains. *)
val spawn_into
  :  ('a, _) Concurrent.Spawn.t @ local
  -> ('a, unit) Concurrent.Spawn.t @ local

(** [in_scope await scope] is a {!Concurrent.Spawn.t} which spawns tasks into the given
    [scope] as threads in arbitrary (managed) domains, and uses [await] as its
    implementation of awaiting *)
val in_scope
  :  Await.t @ local
  -> 'a Scope.t @ local
  -> ('a, unit) Concurrent.Spawn.t @ local portable

module Expert = Multicore

@@ portable

(** A scope for structured concurrency. *)

open Base
open Await_kernel

(** [t] represents a scope for structured concurrency.

    Scopes are usually created via {!with_} with a local context value which is provided
    to each task, may have new tasks {{!add} added} to them (from any implementation of
    concurrency), and wait until all tasks exit. This way it is safe to destroy resources
    used within the scope after the scope exits.

    Any uncaught exception within the scope will terminate the scope and all the tasks
    spawned into it and close the scope. This way error handling becomes simpler as one
    doesn't have to otherwise explicitly arrange for termination of siblings and children
    in case of unhandled errors. *)
type !'a t : value mod contended portable

(** [with_ await context ~f] calls [f scope] with a new scope for concurrency and does not
    return until all the tasks added to the scope have exited. An uncaught exception from
    [f] or any task added to the scope will terminate all of the tasks and the exception
    will then be raised out of [with_] after all of the tasks have exited. *)
val with_ : Await.t @ local -> 'a @ portable -> f:('a t @ local -> 'b) @ local once -> 'b

module Global : sig
  (** Allows creating non-local scopes for performing unstructured concurrency. *)

  (** [create context ~on_exit] creates a new global scope.

      If a task within the scope raises an uncaught exception or the scope is explicitly
      terminated, the [on_exit] callback will be called with the exception that was raised
      (if any) and the scope itself. The exception is accessible via {!Scope.failure}, but
      is passed to [on_exit] to push callers to think about uncaught exceptions. *)
  val create
    : ('a : value mod global).
    'a @ portable
    -> on_exit:('a t -> (exn * Backtrace.t) or_null -> unit) @ once portable
    -> 'a t

  (** [create_with_trigger context ~finished] creates a new scope. The trigger [finished]
      will be signaled after the scope has been terminated and all of the tasks added to
      it have exited. Note that this will only happen in case a task added to the scope
      exits with an uncaught exception or the terminator of the scope is explicitly
      terminated. *)
  val create_with_trigger
    : ('a : value mod global).
    'a @ portable -> finished:Trigger.Source.t -> 'a t
end

(** [context scope] returns the context value that the scope was created with. *)
val context : 'a t @ local -> 'a @ contended local portable

(** [terminator scope] returns the terminator of the scope. *)
val terminator : 'a t @ local -> Terminator.t @ local

(** [terminate scope] terminates the scope. *)
val terminate : 'a t @ local -> unit

module Task_handle : sig
  type 'a scope := 'a t

  (** Represents a newly started task that either becomes a daemon or a regular task. *)
  type 'a t : value mod contended portable

  (** [into_scope task_handle] consumes the task handle and turns it into the scope the
      task was spawned into. *)
  val into_scope : 'a t @ local unique -> 'a scope @ local

  (** [become_daemon task_handle] consumes the task handle, turns the task it represents
      into a daemon task, and returns the scope the daemon task was spawned into, along
      with a cancellation token that is canceled once all non-daemon tasks exit. *)
  val become_daemon : 'a t @ local unique -> #('a scope * Cancellation.t) @ local
end

module Token : sig
  (** Represents an ongoing process to add a task to the scope. *)
  type !'a t : value mod contended many portable

  (** [use token ~f] consumes the token and turns it into a task by calling
      [f terminator task_handle]. *)
  val use
    :  'a t @ unique
    -> f:(Terminator.t @ local -> 'a Task_handle.t @ local unique -> unit) @ local once
    -> unit

  (** [drop token] consumes the token without spawning a task. *)
  val drop : 'a t @ unique -> unit
end

(** [add scope] gives a linear token representing a task being added to the scope. The
    token must be {{!Token.use} used} or {{!Token.drop} dropped}. *)
val add : 'a t @ local -> 'a Token.t @ unique

(** [failure] returns [This (exn, bt)] containing the first uncaught exception raised
    within the scope or [Null] in case there have been none. This is usually only called
    after the scope is known to have finished. *)
val failure : 'a t @ local -> (exn * Backtrace.t) or_null

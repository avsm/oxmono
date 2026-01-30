@@ portable

open Await_kernel

(** A write-once cell that can be empty or full (i.e., hold a single value). *)

type !'a t : value mod contended portable

(** [create ()] returns an empty ivar. *)
val create : unit -> 'a t

(** [create_full v] returns an ivar filled with [v]. *)
val create_full : 'a @ contended portable -> 'a t

(** [fill_exn t v] fills [t] with value [v] if [t] was empty.

    @raise Already_full in case [t] was already full. *)
val fill_exn : 'a t @ local -> 'a @ contended portable -> unit

(** [fill_if_empty t v] fills [t] with value [v] if [t] was empty. Otherwise does nothing. *)
val fill_if_empty : 'a t @ local -> 'a @ contended portable -> unit

(** [read w t] waits until the ivar [t] is filled and returns the value [v] after the ivar
    is filled with [v].

    @raise Terminated in case [w] was terminated. *)
val read : Await.t @ local -> 'a t @ local -> 'a @ contended portable

(** [read_or_cancel w c t] is [Completed (read w t)] if [c] was not canceled, otherwise it
    is [Canceled].

    @raise Terminated in case [w] was terminated, even if [c] was canceled. *)
val read_or_cancel
  :  Await.t @ local
  -> Cancellation.t @ local
  -> 'a t @ local
  -> 'a Or_canceled.t @ contended

(** [peek t] returns [This v] iff [t] is full with value [v], or [Null] if [t] is not
    full. *)
val peek : 'a t @ local -> 'a or_null @ contended

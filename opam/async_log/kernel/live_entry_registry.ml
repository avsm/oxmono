open! Core
open Async_kernel
open! Import

module type S = sig
  type t [@@deriving equal]

  val hash : t -> int
  val flushed : t -> unit Deferred.t
  val is_closed : t -> bool
  val flush_and_close : t -> unit Deferred.t
end

module Make (M : S) = struct
  module Weak_table = Stdlib.Weak.Make (M)

  let pending_actions = Bag.create ()
  let live_entries = Weak_table.create 1

  let add_pending t ~action =
    if not (M.is_closed t)
    then (
      let finished = action t in
      let tag = Bag.add pending_actions finished in
      upon finished (fun () -> Bag.remove pending_actions tag))
  ;;

  let live_entries_flushed () =
    Weak_table.iter (add_pending ~action:M.flushed) live_entries;
    Deferred.all_unit (Bag.to_list pending_actions)
  ;;

  let register entry =
    Weak_table.remove live_entries entry;
    Weak_table.add live_entries entry;
    (* Historically, the finalizer for a log flushes /and/ closes it, while the shutdown
       hook only flushes it. We wrap the flush_and_close in [add_pending] to make sure a
       shutdown in the middle of finalization doesn't abruptly stop the flush.

       We could maybe change the [at_shutdown] call to also [flush_and_close], but didn't
       since closing also sets the outputs to none (which then causes the outputs to be
       closed if they're finalized) and we think it may cause future writes--in
       particular, potential writes in later shutdown hooks--to raise. Since [flush] was
       the historical value there, we thought it was safer to keep it as-is. *)
    Gc.add_finalizer_exn entry (add_pending ~action:M.flush_and_close)
  ;;
end

type 'a t =
  { register : 'a -> unit
  ; live_entries_flushed : unit -> unit Deferred.t
  }

let create (type a) (module M : S with type t = a) =
  let module M = Make (M) in
  { register = M.register; live_entries_flushed = M.live_entries_flushed }
;;

let register t entry = t.register entry
let live_entries_flushed t = t.live_entries_flushed ()

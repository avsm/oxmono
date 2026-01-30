open Atomic_lazy_intf
open Blocking_sync

module T = struct
  type ('a : value_or_null) inner =
    | Uncomputed of ('a t -> 'a)
    | Computing
    | Awaiting of
        { mutex : Mutex.t
        ; condition : Condition.t
        }
    | Computed of 'a
    | Error of exn

  and ('a : value_or_null) t = { inner : 'a inner Atomic.t @@ global } [@@unboxed]

  let from_val value = { inner = Atomic.make (Computed value) }

  (* We manually enforce that the thunk stored inside an [Uncomputed] lazy is only called
     once, by only exposing an API that allows each function to be called once, and have
     manually verified (via careful code review and tests) that all possible thread
     interleavings of calls to [force] only call the function once. This property cannot
     be enforced by the type system - so we have to use magic here to assert that the
     function cannot be called more than once. *)
  external magic_many_lazy_thunk : 'a @ once -> 'a @ many @@ portable = "%identity"

  let from_fun_fixed func =
    { inner = Atomic.make (Uncomputed (magic_many_lazy_thunk func)) }
  ;;

  let from_fun func = from_fun_fixed (fun _ -> func ())

  let rec force ({ inner = atomic } as t) =
    match Atomic.get atomic with
    | Computed value -> value
    | Error exn -> raise exn
    | Computing ->
      (* Someone else is forcing the lazy. Block until they're done *)
      let mutex = Mutex.create () in
      let condition = Condition.create () in
      let _ : bool =
        Atomic.compare_and_set atomic Computing (Awaiting { mutex; condition })
      in
      (* Either someone else already set the state to [Awaiting] or the computation was
         completed. This can only happen to us once, so we just try again without any
         calls to cpu_relax. *)
      force t
    | Awaiting { mutex; condition } ->
      Mutex.with_lock mutex ~f:(fun () ->
        match Atomic.get atomic with
        | Awaiting _ -> Condition.wait condition ~mutex
        | Computed _ | Error _ | Computing | Uncomputed _ -> ());
      force t
    | Uncomputed f as uncomputed ->
      let computing = Computing in
      (match Atomic.compare_and_set atomic uncomputed computing with
       | false ->
         (* Someone else beat us to starting the thunk! This can only happen to us once,
            so we just try again without any calls to cpu_relax. *)
         force t
       | true ->
         let computed =
           try Computed (f { inner = atomic }) with
           | exn -> Error exn
         in
         (* We know we must have been the one to set it to [Computing], so we can just
            [exchange] here to set it to the result. *)
         (match Atomic.exchange atomic computed with
          | Awaiting { mutex; condition } ->
            Mutex.with_lock mutex ~f:(fun _ -> ());
            Condition.broadcast condition
          | Computed _ | Error _ | Computing | Uncomputed _ -> ());
         (* We know that we set the value to either [Uncomputed] or [Error] - in either
            case we won't end up in [Uncomputed] again, so it's fine to just recurse here
            to either raise the error or return the result *)
         force t)
  ;;

  let globalize _ ({ inner } @ local) = { inner }

  let peek { inner } =
    match Atomic.get inner with
    | Computed value -> Or_null_shim.This value
    | Computing | Awaiting _ | Uncomputed _ | Error _ -> Or_null_shim.Null
  ;;

  let peek_opt { inner } =
    match Atomic.get inner with
    | Computed value -> Some value
    | Computing | Awaiting _ | Uncomputed _ | Error _ -> None
  ;;

  let is_val { inner } =
    match Atomic.get inner with
    | Computed _ -> true
    | Computing | Awaiting _ | Uncomputed _ | Error _ -> false
  ;;
end

(* Below is unsafe magic, applied safely, to treat ['a t] as covariant in ['a], despite
   the ['a t] type not being inferrable as covariant.

   The safety of this magic hinges on the fact that the operations exposed in [S] are
   covariant in ['a]. (An example of an operation that wouldn't be covariant in ['a]:
   [val set : 'a t -> 'a -> unit], that sets the value of a lazy to something other than
   the computation it was originally specified as.)
*)

external magically_covariant
  :  (module S_nonportable)
  -> (module S_nonportable_covariant)
  = "%identity"

module Nonportable = (val magically_covariant (module T))

(* Below is unsafe magic, applied safely, to produce a portable interface into
   [Atomic_lazy]. The type-safetyness of this magic is due to [S_nonportable] and
   [S_portable] having the same functions in the same order, and the mode-safetyness is
   justified by a non-magic implementation of the full interface in the test suite. *)

module type S_nonportable_concrete =
  S_nonportable with type ('a : value_or_null) t := 'a Nonportable.t

module type S_portable_concrete =
  S_portable with type ('a : value_or_null) t := 'a Nonportable.t

external magically_portable
  :  (module S_nonportable_concrete)
  -> (module S_portable_concrete)
  = "%identity"

module Portable = (val magically_portable (module Nonportable))
include Nonportable
include Portable

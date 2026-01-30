open! Base
open! Portable_kernel

type t = Cancellation.t

let is_terminated = Cancellation.is_canceled
let same = Cancellation.same
let never = Cancellation.never
let always = Cancellation.always

module Source = struct
  type t = Cancellation.Source.t

  let terminate = Cancellation.Source.cancel
end

let is_terminatable = Cancellation.is_cancellable
let source = Cancellation.source
let with_ = Cancellation.with_
let with_linked = Cancellation.with_linked
let with_linked_multi = Cancellation.with_linked_multi

module Link = struct
  type t =
    | Attached
    | Terminated
    | Signaled
  [@@deriving equal ~localize, sexp ~stackify]

  (* This should be the identity function in terms of the representations of
     [Cancellation.Link.t] and [Terminator.Link.t]. *)
  let[@inline] of_cancellation : Cancellation.Link.t -> t = function
    | Attached -> Attached
    | Canceled -> Terminated
    | Signaled -> Signaled
  ;;
end

let add_trigger t s = Link.of_cancellation (Cancellation.add_trigger t s)
let cancellation t = t

module Expert = Cancellation.Expert

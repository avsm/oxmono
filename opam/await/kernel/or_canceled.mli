@@ portable

open Base

type%template ('a : k) t =
  | Canceled
  | Completed of 'a
[@@kind
  k
  = ( void
    , value_or_null & void
    , value_or_null & value_or_null
    , (value_or_null & value_or_null) & value_or_null
    , value_or_null )]
[@@deriving
  compare ~localize, equal ~localize, globalize, sexp ~stackify, sexp_grammar, hash]

(** [completed_exn t] is [a] if [t] is [Completed a], otherwise it raises *)
val%template completed_exn : ('a : k). ('a t[@kind k]) -> 'a
[@@kind
  k
  = ( value_or_null
    , void
    , value_or_null & void
    , value_or_null & value_or_null
    , (value_or_null & value_or_null) & value_or_null )]

include Monad.S [@kind value_or_null] [@mode local] with type 'a t := 'a t

(** [never_completed t] can be used for an [Or_canceled.t] computation which either loops
    forever or is canceled. *)
val never_completed : Nothing.t t @ local -> unit

(**/**)

module Exn : sig
  (** This module provides a way to implement cancellable and non-cancellable variants of
      an operation through a single exception raising implementation.

      This is not meant for casual use. This is rather meant for internal implementation
      in cases where the burden of providing both cancellable and non-cancellable variants
      of operations would otherwise be high. The [Canceled] exception should not be
      allowed to propagate outside of such internal uses. *)

  (** Exception to be raised and shortly handled in case of cancellation. *)
  exception Canceled

  (** [catch fn] calls [Completed (fn ())] and handles the [Canceled] exception by
      returning the [Canceled] value. *)
  val%template catch : ('a : k). (unit -> 'a @ u) @ local once -> ('a t[@kind k]) @ u
  [@@kind
    k
    = ( value_or_null
      , void
      , value_or_null & void
      , value_or_null & value_or_null
      , (value_or_null & value_or_null) & value_or_null )]
  [@@mode u = (aliased, unique)]
end

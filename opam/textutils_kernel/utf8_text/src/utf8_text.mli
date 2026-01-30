(** This library is deprecated; use [Core.String.Utf8] instead. *)

open! Core

[@@@alert "-deprecated"]

(** Text encoded in UTF-8. *)
type t
[@@deriving compare, quickcheck, sexp_of]
[@@deprecated "[since 2023-12] Use [String.Utf8.t] instead."]

(** The invariant is that [t] is a sequence of well-formed UTF-8 code points. *)
include Invariant.S with type t := t

include Container.S0 with type t := t with type elt := Uchar.t
include Stringable.S with type t := t

module Stable : sig
  module V1 : sig
    type nonrec t = t
    [@@deriving bin_io, compare, sexp]
    [@@deprecated
      "[since 2023-12] Use [Core.Core_stable.String.Utf8.V1.t] instead. The type has the \
       same serialization and bin shape so no version bump is required."]

    include Stringable.S with type t := t
  end
end

(** [width t] approximates the display width of [t]. If you are migrating to
    [String.Utf8], use [String.Utf8.length_in_uchars], but see its documentation for why
    this is not a good way to compute display width. *)
val width : t -> int

(** [bytes t] is the number of bytes in the UTF-8 encoding of [t]. If you are migrating to
    [String.Utf8], use [String.length (t :> string)]. *)
val bytes : t -> int

val of_uchar_list : Uchar.t list -> t
val concat : ?sep:t -> t list -> t

(** [iteri t ~f] calls [f index uchar] for every [uchar] in [t]. [index] counts
    characters, not bytes. *)
val iteri : t -> f:(int -> Uchar.t -> unit) -> unit

(** [split t ~on] returns the substrings between and not including occurrences of [on].
    [on] must be an ASCII char (in range '\000' to '\127'). *)
val split : t -> on:char -> t list

@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** JSON safe integers.

    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-1.3} RFC 8620
     Section 1.3} restricts every integer in the protocol to the range an IEEE
    754 double represents exactly, which is -2{^ 53}+1 to 2{^ 53}-1 for an [Int]
    and 0 to 2{^ 53}-1 for an [UnsignedInt].

    @canonical Jmap.Proto.Int53 *)

(** Signed integers in the range -2{^ 53}+1 to 2{^ 53}-1. *)
module Signed : sig
  type t = int64
  (** The type for [Int] values. The type equality is exposed so that a caller
      may use every [Int64] operation on a decoded value. It is not itself a
      guarantee of range. {!of_int}, {!of_int64} and {!jsont} check the range,
      and nothing else does. *)

  val min_value : t
  (** [min_value] is [-9007199254740991L], which is -2{^ 53}+1. *)

  val max_value : t
  (** [max_value] is [9007199254740991L], which is 2{^ 53}-1. *)

  val of_int : int -> (t, string) result
  (** [of_int n] is [n] as an [Int]. The error holds a human readable message
      when [n] is outside the range, which a 64 bit OCaml [int] can be. *)

  val to_int : t -> int option
  (** [to_int n] is [n] as an OCaml [int], or [None] on a platform whose [int]
      is too narrow to hold it. *)

  val of_int64 : int64 -> (t, string) result
  (** [of_int64 n] is [n] as an [Int]. The error holds a human readable message
      when [n] is outside the range. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for an [Int], written as a JSON number. Decoding
      rejects a number that is not integral or is outside the range, and
      encoding errors on a value outside the range. *)
end

(** Unsigned integers in the range 0 to 2{^ 53}-1. *)
module Unsigned : sig
  type t = int64
  (** The type for [UnsignedInt] values. The type equality is exposed so that a
      caller may use every [Int64] operation on a decoded value. It is not
      itself a guarantee of range. {!of_int}, {!of_int64} and {!jsont} check the
      range, and nothing else does. *)

  val min_value : t
  (** [min_value] is [0L]. *)

  val max_value : t
  (** [max_value] is [9007199254740991L], which is 2{^ 53}-1. *)

  val of_int : int -> (t, string) result
  (** [of_int n] is [n] as an [UnsignedInt]. The error holds a human readable
      message when [n] is negative or above {!max_value}. *)

  val to_int : t -> int option
  (** [to_int n] is [n] as an OCaml [int], or [None] on a platform whose [int]
      is too narrow to hold it. *)

  val of_int64 : int64 -> (t, string) result
  (** [of_int64 n] is [n] as an [UnsignedInt]. The error holds a human readable
      message when [n] is negative or above {!max_value}. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for an [UnsignedInt], written as a JSON number.
      Decoding rejects a number that is not integral or is outside the range,
      and encoding errors on a value outside the range. *)
end

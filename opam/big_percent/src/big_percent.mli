@@ portable

(** An arbitrary-precision scale factor, not bounded between 0% and 100%. *)

open! Core

type t = private Bignum.t
[@@deriving compare ~localize, equal ~localize, hash, quickcheck, sexp_of]

include Comparable.S [@modality portable] with type t := t
include Hashable.S with type t := t

val ( * ) : t -> t -> t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( / ) : t -> t -> t
val ( // ) : t -> t -> Bignum.t
val zero : t
val one_hundred_percent : t
val neg : t -> t
val abs : t -> t
val is_zero : t -> bool
val is_nan : t -> bool
val is_inf : t -> bool

(** [apply t x] multiplies the percent [t] by [x], returning a {!type:Bignum.t}. *)
val apply : t -> Bignum.t -> Bignum.t

(** [scale t x] scales the percent [t] by [x], returning a new {!type:t}. *)
val scale : t -> Bignum.t -> t

(** [of_mult 5.] is [5x = 500% = 50_000bp]. *)
val of_mult : Bignum.t -> t

val to_mult : t -> Bignum.t

(** [of_percentage 5.] is [5% = 0.05x = 500bp]. *)
val of_percentage : Bignum.t -> t

val to_percentage : t -> Bignum.t

(** [of_bp 5.] is [5bp = 0.05% = 0.0005x]. *)
val of_bp : Bignum.t -> t

val to_bp : t -> Bignum.t

(** [of_bp_int] is like {!val:of_bp}, except it accepts an [int] instead of a [Bignum.t]. *)
val of_bp_int : int -> t

(** [to_bp_int t] rounds down. *)
val to_bp_int : t -> int option

(** @raise [Zarith.Z.Overflow] *)
val to_bp_int_exn : t -> int

val of_percent_decimal : Percent.t -> t
val of_percent_dyadic : Percent.t -> t
val to_percent : t -> Percent.t

(** [round_decimal_mult (of_percentage 0.0123456) ~decimal_digits:4] is [0.0001 = 1bp]. *)
val round_decimal_mult : t -> digits:int -> t

(** [round_decimal_percentage (of_percentage 0.0123456) ~decimal_digits:4] is
    [0.0123% = 1.23bp]. *)
val round_decimal_percentage : t -> digits:int -> t

(** [round_decimal_bp (of_percentage 0.0123456) ~decimal_digits:4] is [1.2346bp]. *)
val round_decimal_bp : t -> digits:int -> t

(** [sign_exn] returns the sign of a {!type:t}. Both [-0.] and [0.] map to {!val:zero}.

    @raise [Bignum.NaN] *)
val sign_exn : t -> Sign.t

(** @raise [Nan_or_inf] *)
val of_string_exn : string -> t

val of_string_allow_nan_and_inf : string -> t

(** [to_string_accurate] uses {!val:Bignum.to_string_accurate} under the hood before
    appending a percent suffix (e.g. bp, x, %). If necessary, the {!type:Bignum.t}
    representation is parenthesized.

    For example:
    {[
      to_string_accurate (Big_percent.of_string "1/3x") = "(33.333333333 + 1/3000000000)%"
    ]}

    If the percent is infinite, it returns [INFx] or [-INFx]. If the percent is nan, it
    returns [NANbp]. These follow the same convention as {!module:Core.Percent}.

    [to_string_accurate] is roundtrippable with {!val:of_string_allow_nan_and_inf}. *)
val to_string_accurate : t -> string

(** [to_string_hum t] pretty prints [t] in approximate decimal form, or prints [INFx],
    [-INFx], or [NANbp].

    For example:
    {[
      to_string_hum ~delimiter:',' ~decimals:3 ~strip_zero:false 1234.1999x = "1,234.200x"
    ]}

    No delimiters are inserted to the right of the decimal.

    Note that [99.9999....%] will be rounded as [100%] instead of [1x] and similarly for
    bps. *)
val to_string_hum : ?delimiter:char -> ?decimals:int -> ?strip_zero:bool -> t -> string

module Stable : sig
  module V3 : sig
    module For_testing : sig
      type target

      val tag_variants : (string * int) list
      val bin_rep_variants : (string * int) list
      val to_binable : t -> target
      val of_binable : target -> t
    end

    type nonrec t = t [@@deriving bin_io, compare ~localize, equal ~localize, hash]

    (** NOTE: The [Stable.V3] sexp representation is different from the [Unstable] sexp
        representation, even though they are type-equal.

        The stable sexp representation does not include percent formatting. I.e. it will
        serialize the value as a bignum.

        Trying to deserialize a stable type using [Unstable.t_of_sexp] will raise. *)
    include Sexpable.S with type t := t
  end

  module V4 : sig
    module For_testing : sig
      type target

      val tag_variants : (string * int) list
      val bin_rep_variants : (string * int) list
      val to_binable : t -> target
      val of_binable : target -> t
    end

    type nonrec t = t
    [@@deriving
      bin_io, compare ~localize, equal ~localize, hash, sexp_grammar, stable_witness]

    (** [Stable.V4] is the same as {!module:V3} except with a different sexp
        representation (i.e. correctly formatted with bp, x, or % suffixes).

        NOTE: We do not guarantee that this sexp representation is roundtrippable with the
        unstable sexp representation. *)
    include Sexpable.S with type t := t
  end
end

module Unstable : sig
  type nonrec t = t [@@deriving bin_io, compare ~localize, equal ~localize, hash]

  (** {!type:Core.Sexp.t}s are of the form [5bp] or [0.05%] or [0.0005x].

      {!val:sexp_of_t} defaults to using {!val:to_string_accurate}. {!val:t_of_sexp}
      defaults to using {!val:of_string_allow_nan_and_inf}.

      Serialization via sexps is therefore roundtrippable.

      NOTE: The [Unstable] sexp representation is different from the [Stable.V3] sexp
      representation, even though they are type-equal.

      Trying to deserialize an unstable type using [Stable.V3.t_of_sexp] will raise. *)
  include Sexpable.S with type t := t
end

(** [Always_percentage] does not format small values as [3bp] or large ones as [2x], but
    rather always uses percentages (e.g. [0.0003%] or [200%]).

    These are compatible with the standard {!val:t_of_sexp}. *)
module Always_percentage : sig
  type nonrec t = t [@@deriving sexp_of]

  val to_string_accurate : t -> string
  val to_string_hum : ?delimiter:char -> ?decimals:int -> ?strip_zero:bool -> t -> string
end

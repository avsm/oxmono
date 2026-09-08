@@ portable

(** Zulip object identifiers.

    Each identifier type is nominally distinct. Identifiers are nonnegative
    integers in the exactly representable JSON range. *)

module type S = sig
  @@ portable
  type t : immutable_data
  (** The type for identifiers. *)

  val of_int : int -> t
  (** [of_int n] is the identifier represented by [n].

      @raise Stdlib.exception-Invalid_argument
        if [n] is negative or greater than [2^53 - 1]. *)

  val to_int : t -> int
  (** [to_int id] is the integer represented by [id]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] represent the same integer. *)

  val compare : t -> t -> int
  (** [compare a b] orders [a] and [b] by their integer values. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf id] writes the decimal representation of [id] to [ppf]. *)

  val jsont : t Jsont.t
  (** [jsont] is a codec for nonnegative, exactly representable JSON integers.
      Decoding rejects fractions and nonfinite or out-of-range numbers. *)
end

module User : S
module Channel : S
module Message : S
module Recipient : S
module Event : S
module User_group : S
module Channel_folder : S
module Linkifier : S
module Profile_field : S
module Attachment : S
module Draft : S
module Scheduled_message : S
module Reminder : S
module Saved_snippet : S

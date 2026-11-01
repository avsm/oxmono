@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Apple Mail flag colors.

    Apple Mail encodes a message's flag color as a 3-bit pattern using the
    [$MailFlagBit0], [$MailFlagBit1] and [$MailFlagBit2] keywords. See
    {{:https://datatracker.ietf.org/doc/draft-ietf-mailmaint-messageflag-mailboxattribute#section-3}
     draft-ietf-mailmaint-messageflag-mailboxattribute Section 3}.

    Bit 0 is [$MailFlagBit0], bit 1 is [$MailFlagBit1] and bit 2 is
    [$MailFlagBit2]. The seven defined colors and their bit patterns are:

    - [Red] is 000.
    - [Orange] is 100.
    - [Yellow] is 010.
    - [Green] is 110.
    - [Blue] is 001.
    - [Purple] is 101.
    - [Gray] is 011.

    The pattern 111 is undefined; every function below that would otherwise
    produce or accept it returns or rejects [None] instead. *)

(** {1 Colors} *)

type t =
  [ `Red  (** No bits are set. The bit pattern is 000. *)
  | `Orange  (** Bit 0 is set. The bit pattern is 100. *)
  | `Yellow  (** Bit 1 is set. The bit pattern is 010. *)
  | `Green  (** Bits 0 and 1 are set. The bit pattern is 110. *)
  | `Blue  (** Bit 2 is set. The bit pattern is 001. *)
  | `Purple  (** Bits 0 and 2 are set. The bit pattern is 101. *)
  | `Gray  (** Bits 1 and 2 are set. The bit pattern is 011. *) ]
(** The type for Apple Mail flag colors. *)

(** {1 Keyword Conversion} *)

val to_keywords : t -> [ `MailFlagBit0 | `MailFlagBit1 | `MailFlagBit2 ] list
(** [to_keywords color] is the list of keyword bits set for [color]. [Red] is
    the empty list, since it needs no bits set. *)

val of_keywords :
  [ `MailFlagBit0 | `MailFlagBit1 | `MailFlagBit2 ] list -> t option
(** [of_keywords keywords] is the color encoded by [keywords]. It is [None] if
    [keywords] sets no bits, since that does not distinguish "no color" from
    [Red] (bit pattern 000), and [None] for the undefined pattern 111. Use
    {!of_keywords_default_red} to treat an empty [keywords] as [Red]. *)

val of_keywords_default_red :
  [ `MailFlagBit0 | `MailFlagBit1 | `MailFlagBit2 ] list -> t option
(** [of_keywords_default_red keywords] is like {!of_keywords}, except an empty
    [keywords] is [Red] rather than [None]. It is [None] only for the undefined
    pattern 111. *)

(** {1 String Conversion} *)

val to_string : t -> string
(** [to_string color] is the lowercase name of [color]. *)

val of_string : string -> t option
(** [of_string s] is the color named by [s]. Matching is case-insensitive and
    accepts ["grey"] as well as ["gray"]. It is [None] if [s] does not name a
    color. *)

(** {1 Comparison and Pretty Printing} *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] are the same color. *)

val compare : t -> t -> int
(** [compare a b] is a total order over colors. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf color] prints the name of [color] to [ppf]. *)

@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Phonetic systems and scripts.

    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.5.4} RFC 9553
     Section 1.5.4} defines the [phonetic], [phoneticScript] and
    [phoneticSystem] properties, which say how to pronounce a name or address
    component. The value of a [phoneticSystem] property is a {!t}.

    @canonical Jscontact.Phonetic *)

type t =
  [ `Ipa  (** The International Phonetic Alphabet. *)
  | `Jyut  (** The Cantonese romanization system "Jyutping". *)
  | `Piny  (** The Mandarin romanization system "Hanyu Pinyin". *)
  | `Vendor of string  (** A vendor-specific system. *) ]
(** The type for a phonetic system. *)

include Jscontact_enum.S with type t := t

val validate_script : string -> string Jscontact_valid.t
(** [validate_script s] is [Ok s] if [s] is a script subtag as defined by
    {{:https://www.rfc-editor.org/rfc/rfc5646.html#section-2.2.3} RFC 5646
     Section 2.2.3}, that is four letters. A [phoneticScript] property holds
    one. *)

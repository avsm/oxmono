@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Preferences.

    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.5.3} RFC 9553
     Section 1.5.3} defines the [pref] property, the preference of a piece of
    contact information in relation to the others of the same property. Lower
    values are more preferred. Information with no preference is least
    preferred.

    @canonical Jscontact.Pref *)

val validate : int -> int Jscontact_valid.t
(** [validate p] is [Ok p] if [p] is in the range 1 to 100. *)

val jsont : int Jsont.t
(** [jsont] is the codec for a [pref] property. Decoding errors on a number that
    is not an UnsignedInt and leaves the range to {!validate}. *)

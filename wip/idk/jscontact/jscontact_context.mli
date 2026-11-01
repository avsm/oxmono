@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The contexts in which to use contact information.

    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.5.1} RFC 9553
     Section 1.5.1} defines the [contexts] property, a [String[Boolean]] set of
    the contexts in which a piece of contact information may be used, and its
    two values.
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.5.1.1} Section
     2.5.1.1} adds two more for an Address alone.

    @canonical Jscontact.Context *)

type t =
  [ `Private  (** The information may be used in a private context. *)
  | `Work  (** The information may be used in a professional context. *)
  | `Billing  (** An address to be used for billing. Addresses only. *)
  | `Delivery
    (** An address to be used for delivering physical items. Addresses only. *)
  | `Vendor of string  (** A vendor-specific context. *) ]
(** The type for a context. *)

include Jscontact_enum.VALUE with type t := t

val validate_set : ?address:bool -> t list -> t list Jscontact_valid.t
(** [validate_set ~address cs] is {!validate} over [cs], and rejects a value the
    set may not hold. [address] says whether the set belongs to an Address,
    which alone may use [`Billing] and [`Delivery], and defaults to [false].
    This is {!Jscontact.Enum.S.validate_set} with the rule of Section 2.5.1.1
    added, and the reason {!Jscontact.Context} includes {!Jscontact.Enum.VALUE}
    rather than {!Jscontact.Enum.S}. *)

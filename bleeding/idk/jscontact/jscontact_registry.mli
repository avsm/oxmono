@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The property registry.

    The property names of the IANA "JSContact Properties" registry, the names
    {{:https://www.rfc-editor.org/rfc/rfc9553.html} RFC 9553} reserves, and the
    shape a name must have for IANA to register it, as defined by
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.7} RFC 9553 Section
     1.7}. A name that is in neither the registry nor this shape is admissible
    only as a vendor extension, for which see {!Jscontact.Vendor}.

    @canonical Jscontact.Registry *)

(** {1 Registered names} *)

val registered : string list
(** [registered] are the property names in the IANA "JSContact Properties"
    registry. It holds those of
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-3.5.2} RFC 9553
     Section 3.5.2}, and the [vCardName], [vCardParams] and [vCardProps] that
    {{:https://www.rfc-editor.org/rfc/rfc9555.html#section-5.3} RFC 9555 Section
     5.3} adds. This library types none of the last three, so a value carries
    them through its unknown members, but a name is registered whether or not it
    is typed here. [cryptoKeys] is listed although the table of Section 3.5.2
    omits it, since Section 2.6.1 defines the property. *)

val is_registered : string -> bool
(** [is_registered s] is [true] if [s] is one of {!registered}. *)

val is_registrable_name : string -> bool
(** [is_registrable_name s] is [true] if [s] has the shape of a property name
    IANA could register, per
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.7.2} RFC 9553
     Section 1.7.2}. Such a name is a non-empty string of ASCII alphanumerics
    and the commercial at, starting with a lowercase letter or with ["@"]. Lower
    camel case beyond the first character is a convention this check cannot
    verify. *)

(** {1 Reserved names} *)

val reserved : string list
(** [reserved] are the property names
    {{:https://www.rfc-editor.org/rfc/rfc9553.html} RFC 9553} reserves in every
    object. It holds ["extra"] alone, which
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.7.3.1} Section
     1.7.3.1} reserves so that an implementation has a name certain never to
    occur as a JSContact property. Section 1.7.3 makes an object carrying a
    reserved property invalid. *)

val reserved_in : (string * string list) list
(** [reserved_in] are the property names reserved in one object type rather than
    in every one, as the JSContact type name to the names reserved in it.
    {{:https://www.rfc-editor.org/rfc/rfc9610.html#section-7.5} RFC 9610 Section
     7.5} registers three entries. [id] and [addressBookIds] are reserved in a
    Card, and [blobId] in a Media. JMAP gives each of them a meaning of its own,
    so reserving the name keeps another specification from claiming it. A JMAP
    ContactCard is consequently not a valid standalone JSContact Card. *)

val is_reserved : ?in_type:string -> string -> bool
(** [is_reserved ?in_type s] is [true] if [s] is reserved in every object, or
    reserved in the JSContact object type named [in_type]. [in_type] defaults to
    absent, in which case only the names reserved in every object are checked.
*)

(** {1 Validation} *)

val validate_property_name :
  ?in_type:string -> string -> string Jscontact_valid.t
(** [validate_property_name ?in_type s] is [Ok s] if [s] is either an
    IANA-registrable name or a vendor-specific one, which
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.7.4} RFC 9553
     Section 1.7.4} requires of any property an implementation preserves without
    understanding. It is an [Error] if [s] is reserved in every object or
    reserved in the JSContact object type named [in_type]. It is also an [Error]
    if [s] differs only in case from a registered or reserved name, which
    Section 1.7.1 makes invalid. [in_type] defaults to absent, in which case
    only the names reserved in every object are checked. *)

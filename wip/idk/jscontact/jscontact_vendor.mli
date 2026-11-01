@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Vendor-specific names and values.

    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.8} RFC 9553 Section
     1.8} lets a vendor extend JSContact with properties and enumerated values
    of its own, provided the name is a domain name the vendor controls, a colon,
    and a free-form name. Section 1.8.1 gives the grammar:

    {v
    v-extension = v-prefix ":" v-name
    v-prefix    = v-label *("." v-label)
    v-label     = alnum-int / alnum-int *(alnum-int / "-") alnum-int
    alnum-int   = ALPHA / DIGIT / NON-ASCII
    v-name      = 1*(WSP / "!" / %x23-2e / %x30-7d / NON-ASCII)
    v}

    A [v-name] is therefore any non-empty run of characters other than the
    controls, the double quote, the solidus and the tilde. The last two are
    excluded because a property name appears in the JSON Pointer of a
    {!Jscontact.Patch.t}, where they would need escaping.

    The names IANA registers or
    {{:https://www.rfc-editor.org/rfc/rfc9553.html} RFC 9553} reserves are in
    {!Jscontact.Registry}.

    @canonical Jscontact.Vendor *)

val is_extension : string -> bool
(** [is_extension s] is [true] if [s] is a well formed [v-extension], such as
    ["example.com:foo"]. *)

val validate_extension : string -> string Jscontact_valid.t
(** [validate_extension s] is [Ok s] if {!is_extension} holds of [s], and
    otherwise an [Error] whose message says which rule [s] breaks. *)

val prefix : string -> string option
(** [prefix s] is the vendor prefix of [s], the part before its first colon, or
    [None] if [s] is not a well formed [v-extension]. The prefix ["ietf.org"]
    and its subdomains are reserved for IETF specifications. That reservation is
    not enforced. *)

val name : string -> string option
(** [name s] is the vendor name of [s], the part after its first colon, or
    [None] if [s] is not a well formed [v-extension]. *)

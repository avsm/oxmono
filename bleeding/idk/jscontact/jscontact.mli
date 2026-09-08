@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** JSContact contact data.

    An implementation of
    {{:https://www.rfc-editor.org/rfc/rfc9553.html} RFC 9553}, the JSON
    representation of contact data that succeeds vCard. Every object type of the
    specification has a module here with its record type, a [jsont] codec, an
    [equal], a [pp] and a [validate]. {!Card} is the topmost object and the one
    an application starts from. *)

(** {1 Data types} *)

module Id = Jscontact_id
(** Identifiers, the keys of the repeated properties of a Card, as defined by
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.4.1} RFC 9553
     Section 1.4.1}. *)

module Date = Jscontact_date
(** UTC date-times, partial dates and timestamps, as defined by Section 1.4.5
    and Section 2.8.1. *)

module Patch = Jscontact_patch
(** PatchObjects, the localizations of a Card, as defined by Section 1.4.3. *)

module Unknown = Jscontact_unknown
(** The members an object codec does not define, which Section 1.7.4 requires to
    be preserved. *)

module Vendor = Jscontact_vendor
(** The grammar of vendor-specific names and values, as defined by Section 1.8.
*)

module Registry = Jscontact_registry
(** The registered and reserved property names, as defined by Section 1.7. *)

(** {1 Common properties and syntaxes} *)

module Context = Jscontact_context
(** The [contexts] property, as defined by Section 1.5.1. *)

module Pref = Jscontact_pref
(** The [pref] property, as defined by Section 1.5.3. *)

module Phonetic = Jscontact_phonetic
(** The [phoneticSystem] and [phoneticScript] properties, as defined by Section
    1.5.4. *)

module Language = Jscontact_language
(** The language tags of
    {{:https://www.rfc-editor.org/rfc/rfc5646.html} RFC 5646}, which type the
    [language] of a Card and the keys of its [localizations]. *)

module Uri = Jscontact_uri
(** The URIs of {{:https://www.rfc-editor.org/rfc/rfc3986.html} RFC 3986}, which
    type the [uri] of every resource. *)

(** {1 Object types} *)

module Card = Jscontact_card
(** Cards, the topmost object of JSContact data, as defined by Section 2. *)

module Name = Jscontact_name
(** Names, their components and nicknames, as defined by Section 2.2.1 and
    Section 2.2.2. *)

module Org = Jscontact_org
(** Organizations, titles and forms of address, as defined by Section 2.2.3 to
    Section 2.2.5. *)

module Contact = Jscontact_contact
(** Email addresses, online services, phone numbers and preferred languages, as
    defined by Section 2.3. *)

module Calendar = Jscontact_calendar
(** Calendaring resources and scheduling addresses, as defined by Section 2.4.
*)

module Address = Jscontact_address
(** Postal addresses and geographical locations, as defined by Section 2.5. *)

module Resource = Jscontact_resource
(** The Resource data type of Section 1.4.4 and the cryptographic keys,
    directories, links and media of Section 2.6. *)

module Info = Jscontact_info
(** Relations, anniversaries, notes and personal information, as defined by
    Section 2.1.8 and Section 2.8. *)

(** {1 Building codecs and validators} *)

module Valid = Jscontact_valid
(** The combinators every [validate] function is written with. *)

module Enum = Jscontact_enum
(** The functor every enumerated value type is built with, as Section 1.7.5
    requires an enumeration to admit vendor-specific values. *)

module Json = Jscontact_json
(** The codecs for the [@type] member, the UnsignedInt data type and the JSON
    objects {{:https://www.rfc-editor.org/rfc/rfc9553.html} RFC 9553} uses as
    maps and sets. *)

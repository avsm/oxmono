(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** IDKit, contact data for OCaml.

    One package per specification, and the JSContact Card of
    {{:https://www.rfc-editor.org/rfc/rfc9553.html} RFC 9553} as the
    representation they meet at. A vCard read from a file, an address object
    fetched over CardDAV and a ContactCard received over JMAP all become the
    same {!Jscontact.Card.t}, and a Card written back to any of them keeps what
    it arrived with.

    This module is a view of the packages installed alongside it. Each is a
    library of its own and may be used without the others. Depend on [idk] to
    take them together, or on the one package the task needs.

    {1 The layers}

    Three layers stack, and each is useful on its own.

    The {e formats} are text on the wire. {!Vcard} is vCard 4.0 and {!Ical} is
    iCalendar. Both are read and written losslessly, so a document passes
    through with every property it holds, known to the library or not. They
    share a content line syntax, which is why {!Ical.Property} is
    {!Vcard.Property}.

    The {e model} is JSON. {!Jscontact} is the Card and every object type of RFC
    9553, with a codec, an [equal], a [pp] and a [validate] for each.
    {!Jscontact_vcard} converts between a Card and a vCard by
    {{:https://www.rfc-editor.org/rfc/rfc9555.html} RFC 9555}, in both
    directions and without loss, since a vCard property with no JSContact
    counterpart survives in the Card and a JSContact property with no vCard
    counterpart survives in the vCard.

    The {e protocols} are the DAV extensions that store the formats. {!Carddav}
    describes CardDAV requests and reads its responses, {!Caldav} does the same
    for CalDAV, and neither sends anything. The clients that do are in the
    [idk.eio] library, for which see {!Idk_eio}.

    {1 The Card in the middle}

    {!Jscontact_vcard} is the hinge. Everything that speaks vCard reaches the
    Card through it, so a program written against {!Jscontact.Card.t} reads a
    file, an address book and a JMAP response with one set of types.

    {v
    file, CardDAV, JMAP
              |
            Vcard.t
              |  Jscontact_vcard.of_vcard / to_vcard
              v
       Jscontact.Card.t
    v}

    {!Carddav_jscontact.card} is that hinge already applied. It is a
    {!Carddav.Data.t} of Cards, so every client function that reads or writes an
    address object yields a Card rather than a vCard.

    {1 Choosing a representation}

    A CardDAV server stores vCards and a CalDAV server stores calendar objects,
    but a program need not hold them that way. A {!Carddav.Data.t} says how an
    address object is represented, and every client function takes one, so the
    same call yields the vCard text with {!Carddav.Data.raw}, a {!Vcard.t} with
    {!Carddav.Data.vcard}, or a {!Jscontact.Card.t} with
    {!Carddav_jscontact.card}. {!Caldav.Data} does the same for calendar
    objects, with {!Caldav.Data.ical} in place of the vCard.

    {1 Shape versus meaning}

    Reading enforces shape. It holds a document to its syntax and a JSON object
    to the types of its members. It does not enforce the rules a specification
    states in prose, such as the cardinality of a vCard property or the range of
    a JSContact [pref]. Those live in a separate [validate], so a document a
    server sends still parses into a usable value, and a program that creates
    one holds itself to the specification when it chooses. {!Vcard.validate},
    {!Ical.validate} and the [validate] of each {!Jscontact} type are where the
    prose is checked. *)

(** {1 The model} *)

module Jscontact = Jscontact
(** JSContact, {{:https://www.rfc-editor.org/rfc/rfc9553.html} RFC 9553}. The
    Card and every object type of the specification, with jsont codecs, semantic
    validation and the localization algorithm. *)

module Jscontact_vcard = Jscontact_vcard
(** The conversion between a Card and a vCard,
    {{:https://www.rfc-editor.org/rfc/rfc9555.html} RFC 9555}, in both
    directions and without loss. *)

(** {1 The formats} *)

module Vcard = Vcard
(** vCard 4.0, {{:https://www.rfc-editor.org/rfc/rfc6350.html} RFC 6350}, with
    the parameter escapes of
    {{:https://www.rfc-editor.org/rfc/rfc6868.html} RFC 6868} and the JSContact
    extensions of {{:https://www.rfc-editor.org/rfc/rfc9554.html} RFC 9554} and
    RFC 9555. *)

module Ical = Ical
(** iCalendar, {{:https://www.rfc-editor.org/rfc/rfc5545.html} RFC 5545}, with
    the properties of {{:https://www.rfc-editor.org/rfc/rfc7986.html} RFC 7986}.
*)

(** {1 The protocols} *)

module Carddav = Carddav
(** CardDAV, {{:https://www.rfc-editor.org/rfc/rfc6352.html} RFC 6352}. The
    address book properties, the addressbook-query and addressbook-multiget
    reports, and the conditions, described and read without an HTTP client. *)

module Caldav = Caldav
(** CalDAV, {{:https://www.rfc-editor.org/rfc/rfc4791.html} RFC 4791}. The
    calendar properties, MKCALENDAR, the calendar-query, calendar-multiget and
    free-busy-query reports, and the conditions, described and read without an
    HTTP client. *)

module Carddav_jscontact = Carddav_jscontact
(** Address objects as JSContact cards. {!Carddav_jscontact.card} is the
    {!Carddav.Data.t} that reads and writes an address object as a
    {!Jscontact.Card.t}. *)

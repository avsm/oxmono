(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** CardDAV, {{:https://www.rfc-editor.org/rfc/rfc6352.html} RFC 6352}.

    The properties, reports, filters and conditions of the vCard extensions to
    WebDAV, built on {!Httpz_dav}. Requests are described and responses read
    here without an HTTP client. The [carddav.eio] library sends them, and the
    [carddav.jscontact] library adds the JSContact representation of an address
    object. *)

module Property = Carddav_property
(** Names and properties. *)

module Filter = Carddav_filter
(** Query filters. *)

module Address_data = Carddav_address_data
(** The address-data element. *)

module Report = Carddav_report
(** The addressbook-query and addressbook-multiget reports. *)

module Error = Carddav_error
(** Conditions. *)

module Data = Carddav_data
(** Address object representations. *)

module Addressbook = Carddav_addressbook
(** Address book collections. *)

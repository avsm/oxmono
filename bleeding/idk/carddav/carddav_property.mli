(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** CardDAV names and properties.

    The elements of {{:https://www.rfc-editor.org/rfc/rfc6352.html} RFC 6352}
    live in the namespace [urn:ietf:params:xml:ns:carddav]. The names here are
    its properties, Sections 6.2, 7.1 and 8.3.1, its resource type, Section 5.2,
    and its reports, Section 8.

    @canonical Carddav.Property *)

type name = Httpz_dav.name
(** The type for names. *)

val carddav : string -> name
(** [carddav local] is the name [local] in the CardDAV namespace. *)

(** {1 Resource types and reports} *)

val addressbook : name
(** [addressbook] is the resource type of an address book collection,
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-5.2} RFC 6352 Section
     5.2}. *)

val addressbook_query : name
(** [addressbook_query] is the report of
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-8.6} RFC 6352 Section
     8.6}. *)

val addressbook_multiget : name
(** [addressbook_multiget] is the report of
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-8.7} RFC 6352 Section
     8.7}. *)

(** {1 Properties} *)

val addressbook_home_set : name
(** [addressbook_home_set] is the property of a principal listing the
    collections its address books live in,
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-7.1.1} RFC 6352
     Section 7.1.1}. *)

val principal_address : name
(** [principal_address] is the property of a principal naming the address object
    that describes it,
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-7.1.2} RFC 6352
     Section 7.1.2}. *)

val addressbook_description : name
(** [addressbook_description] is
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-6.2.1} RFC 6352
     Section 6.2.1}. *)

val supported_address_data : name
(** [supported_address_data] is
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-6.2.2} RFC 6352
     Section 6.2.2}. *)

val max_resource_size : name
(** [max_resource_size] is
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-6.2.3} RFC 6352
     Section 6.2.3}. *)

val supported_collation_set : name
(** [supported_collation_set] is
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-8.3.1} RFC 6352
     Section 8.3.1}. *)

(** {1 Readers} *)

val is_addressbook : Httpz_dav.element -> bool
(** [is_addressbook p] is [true] if the [DAV:resourcetype] [p] holds
    [CARDDAV:addressbook]. *)

val address_data_types : Httpz_dav.element -> (string * string) list
(** [address_data_types p] are the content type and version pairs a
    [CARDDAV:supported-address-data] lists. A pair defaults to ["text/vcard"]
    and ["3.0"], and a server with no such property accepts only that pair,
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-6.2.2} RFC 6352
     Section 6.2.2}. *)

val collations : Httpz_dav.element -> string list
(** [collations p] are the collations a [CARDDAV:supported-collation-set] lists.
*)

val max_size : Httpz_dav.element -> int option
(** [max_size p] is the value of a [CARDDAV:max-resource-size]. *)

(** {1 Constructors} *)

val description : ?lang:string -> string -> Httpz_dav.element
(** [description ~lang s] is a [CARDDAV:addressbook-description] of [s], tagged
    [xml:lang] [lang] when given. *)

val collection_props : name list
(** [collection_props] are the properties a client asks of a collection to tell
    address books apart and describe them. They are [DAV:resourcetype],
    [DAV:displayname], [DAV:getetag], [DAV:sync-token],
    [DAV:supported-report-set], [DAV:current-user-privilege-set],
    [CARDDAV:addressbook-description], [CARDDAV:supported-address-data],
    [CARDDAV:max-resource-size] and [CARDDAV:supported-collation-set]. *)

val principal_props : name list
(** [principal_props] are [CARDDAV:addressbook-home-set],
    [CARDDAV:principal-address], [DAV:displayname] and [DAV:principal-URL]. *)

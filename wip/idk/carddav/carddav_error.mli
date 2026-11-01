(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** CardDAV conditions.

    The preconditions and postconditions of
    {{:https://www.rfc-editor.org/rfc/rfc6352.html} RFC 6352} a server names in
    a [DAV:error] body, Sections 5.1.1.1, 6.3.2.1 and 8.6.

    @canonical Carddav.Error *)

val supported_address_data : Httpz_dav.name
(** [supported_address_data] is [CARDDAV:supported-address-data], which a server
    names when the resource's media type is not one the targeted address book
    collection accepts,
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-6.3.2.1} RFC 6352
     Section 6.3.2.1}. *)

val valid_address_data : Httpz_dav.name
(** [valid_address_data] is [CARDDAV:valid-address-data], which a server names
    when the submitted data is not valid vCard data,
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-6.3.2.1} RFC 6352
     Section 6.3.2.1}. *)

val no_uid_conflict : Httpz_dav.name
(** [no_uid_conflict] is [CARDDAV:no-uid-conflict], which a server names when
    the UID is already in use by another resource in the targeted address book,
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-6.3.2.1} RFC 6352
     Section 6.3.2.1}. *)

val addressbook_collection_location_ok : Httpz_dav.name
(** [addressbook_collection_location_ok] is
    [CARDDAV:addressbook-collection-location-ok], which a server names when an
    address book collection cannot be created at the destination,
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-6.3.2.1} RFC 6352
     Section 6.3.2.1}. *)

val max_resource_size : Httpz_dav.name
(** [max_resource_size] is [CARDDAV:max-resource-size], which a server names
    when the resource is larger than the targeted address book collection
    accepts,
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-6.3.2.1} RFC 6352
     Section 6.3.2.1}. *)

val supported_address_data_conversion : Httpz_dav.name
(** [supported_address_data_conversion] is
    [CARDDAV:supported-address-data-conversion], which a server names when the
    resource cannot be converted to the media type requested,
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-5.1.1.1} RFC 6352
     Section 5.1.1.1}. *)

val supported_filter : Httpz_dav.name
(** [supported_filter] is [CARDDAV:supported-filter], which a server names when
    a filter names a vCard property or parameter the server does not support
    querying,
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-8.6} RFC 6352 Section
     8.6}. *)

val supported_collation : Httpz_dav.name
(** [supported_collation] is [CARDDAV:supported-collation], which a server names
    when the collation named is not one the server supports,
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-8.6} RFC 6352 Section
     8.6}. *)

val conflicting_uid : Httpz_dav.element list -> string option
(** [conflicting_uid e] is the href of the resource already using the UID when
    [e] names [CARDDAV:no-uid-conflict],
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-6.3.2.1} RFC 6352
     Section 6.3.2.1}. *)

val describe : Httpz_dav.name -> string
(** [describe n] is a sentence explaining the condition [n], for the conditions
    of {{:https://www.rfc-editor.org/rfc/rfc6352.html} RFC 6352},
    {{:https://www.rfc-editor.org/rfc/rfc4918.html} RFC 4918} and
    {{:https://www.rfc-editor.org/rfc/rfc6578.html} RFC 6578}, and the local
    name of any other. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let carddav local = Httpz_dav.carddav local
let supported_address_data = carddav "supported-address-data"
let valid_address_data = carddav "valid-address-data"
let no_uid_conflict = carddav "no-uid-conflict"

let addressbook_collection_location_ok =
  carddav "addressbook-collection-location-ok"

let max_resource_size = carddav "max-resource-size"

let supported_address_data_conversion =
  carddav "supported-address-data-conversion"

let supported_filter = carddav "supported-filter"
let supported_collation = carddav "supported-collation"

let conflicting_uid e =
  match Httpz_dav.Condition.hrefs no_uid_conflict e with
  | h :: _ -> Some h
  | [] -> None

let describe (ns, local) =
  if ns = "urn:ietf:params:xml:ns:carddav" then
    match local with
    | "supported-address-data" ->
        "The resource's media type is not one this address book collection \
         accepts."
    | "valid-address-data" -> "The submitted data is not valid vCard data."
    | "no-uid-conflict" ->
        "The UID is already in use by another resource in this address book."
    | "addressbook-collection-location-ok" ->
        "An address book collection cannot be created at that destination."
    | "max-resource-size" ->
        "The resource is larger than this address book collection accepts."
    | "supported-address-data-conversion" ->
        "The resource cannot be converted to the media type requested."
    | "supported-filter" ->
        "The filter names a vCard property or parameter this server does not \
         support querying."
    | "supported-collation" ->
        "The collation named is not one this server supports."
    | _ -> local
  else if ns = "DAV:" then
    match local with
    | "cannot-modify-protected-property" ->
        "The property is protected and cannot be set."
    | "preserved-live-properties" ->
        "A live property of the resource could not be preserved."
    | "propfind-finite-depth" ->
        "The server does not support an infinite-depth PROPFIND here."
    | "no-external-entities" ->
        "The request body must not reference an external XML entity."
    | "no-conflicting-lock" ->
        "a lock on the resource conflicts with the one requested"
    | "lock-token-submitted" ->
        "A resource is locked and no valid lock token was submitted."
    | "allow-client-defined-uri" ->
        "The collection does not allow the client to name the new member."
    | "valid-sync-token" -> "The sync token is not valid for this collection."
    | "number-of-matches-within-limits" ->
        "The number of matches did not fit within the server's limits."
    | "supported-report" ->
        "The report named is not supported on this resource."
    | "sync-traversal-supported" ->
        "The requested synchronisation depth is not supported here."
    | "need-privileges" ->
        "The current user lacks the privileges the request requires."
    | _ -> local
  else local

(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Contact store with XDG-compliant storage.

    The contact store manages reading and writing contact metadata
    using XDG-compliant storage locations. Contacts are stored as
    YAML files (one per contact) using the handle as the filename. *)

module Contact = Sortal_schema.Contact
module Temporal = Sortal_schema.Temporal

type t

(** [create fs app_name] creates a new contact store.

    The store will use XDG data directories for persistent storage
    of contact metadata. Each contact is stored as a separate YAML
    file named after its handle.

    @param fs Eio filesystem for file operations
    @param app_name Application name for XDG directory structure *)
val create : Eio.Fs.dir_ty Eio.Path.t -> string -> t

(** [create_from_xdg xdg] creates a contact store from an XDG context.

    This is a convenience function for creating a store when you already
    have an XDG context (e.g., from your own XDG initialization).
    The store will use the XDG data directory for the application.

    @param xdg An existing XDG context
    @return A contact store using the XDG data directory *)
val create_from_xdg : Xdge.t -> t

(** {1 Storage Operations} *)

(** [save t contact] saves a contact to the store.

    The contact is serialized to YAML and written to a file
    named "handle.yaml" in the XDG data directory.

    If a contact with the same handle already exists, it is overwritten. *)
val save : t -> Contact.t -> unit

(** [lookup t handle] retrieves a contact by handle.

    Searches for a file named "handle.yaml" in the XDG data directory
    and deserializes it if found.

    @return [Some contact] if found, [None] if not found or deserialization fails *)
val lookup : t -> string -> Contact.t option

(** [delete t handle] removes a contact from the store.

    Deletes the file "handle.yaml" from the XDG data directory.
    Does nothing if the contact does not exist. *)
val delete : t -> string -> unit

(** {1 Contact Modification} *)

(** [add_email t handle email] adds an email to an existing contact.

    @param t The store
    @param handle The contact handle
    @param email The email entry to add
    @return [Ok ()] on success, [Error msg] if contact not found
    @raise Failure if the contact cannot be saved *)
val add_email : t -> string -> Contact.email -> (unit, string) result

(** [remove_email t handle address] removes an email from a contact.

    Removes all email entries with the given address.

    @param t The store
    @param handle The contact handle
    @param address The email address to remove
    @return [Ok ()] on success, [Error msg] if contact not found *)
val remove_email : t -> string -> string -> (unit, string) result

(** [add_service t handle service] adds a service to an existing contact.

    @param t The store
    @param handle The contact handle
    @param service The service entry to add
    @return [Ok ()] on success, [Error msg] if contact not found *)
val add_service : t -> string -> Contact.service -> (unit, string) result

(** [remove_service t handle url] removes a service from a contact.

    Removes all service entries with the given URL.

    @param t The store
    @param handle The contact handle
    @param url The service URL to remove
    @return [Ok ()] on success, [Error msg] if contact not found *)
val remove_service : t -> string -> string -> (unit, string) result

(** [add_organization t handle org] adds an organization to an existing contact.

    @param t The store
    @param handle The contact handle
    @param org The organization entry to add
    @return [Ok ()] on success, [Error msg] if contact not found *)
val add_organization : t -> string -> Contact.organization -> (unit, string) result

(** [remove_organization t handle name] removes an organization from a contact.

    Removes all organization entries with the given name.

    @param t The store
    @param handle The contact handle
    @param name The organization name to remove
    @return [Ok ()] on success, [Error msg] if contact not found *)
val remove_organization : t -> string -> string -> (unit, string) result

(** [add_url t handle url_entry] adds a URL to an existing contact.

    @param t The store
    @param handle The contact handle
    @param url_entry The URL entry to add
    @return [Ok ()] on success, [Error msg] if contact not found *)
val add_url : t -> string -> Contact.url_entry -> (unit, string) result

(** [remove_url t handle url] removes a URL from a contact.

    Removes all URL entries with the given URL.

    @param t The store
    @param handle The contact handle
    @param url The URL to remove
    @return [Ok ()] on success, [Error msg] if contact not found *)
val remove_url : t -> string -> string -> (unit, string) result

(** [update_contact t handle f] updates a contact by applying function [f].

    Looks up the contact, applies [f] to transform it, and saves the result.

    @param t The store
    @param handle The contact handle
    @param f Function to transform the contact
    @return [Ok ()] on success, [Error msg] if contact not found *)
val update_contact : t -> string -> (Contact.t -> Contact.t) -> (unit, string) result

(** [list t] returns all contacts in the store.

    Scans the XDG data directory for all .yaml files and attempts
    to deserialize them as contacts. Files that fail to parse are
    silently skipped.

    @return A list of all successfully loaded contacts *)
val list : t -> Contact.t list

(** [thumbnail_path t contact] returns the absolute filesystem path to the contact's thumbnail.

    Returns [None] if the contact has no thumbnail set, or [Some path] with
    the full path to the thumbnail file in Sortal's data directory.

    @param t The Sortal store
    @param contact The contact whose thumbnail path to retrieve *)
val thumbnail_path : t -> Contact.t -> Eio.Fs.dir_ty Eio.Path.t option

(** [png_thumbnail_path t contact] returns the path to the PNG version of the contact's thumbnail.

    Returns [None] if the contact has no thumbnail set or if no PNG version exists.
    This looks for a .png file with the same base name as the contact's thumbnail.
    Use this after running [sync] to get the converted PNG thumbnails.

    @param t The Sortal store
    @param contact The contact whose PNG thumbnail path to retrieve *)
val png_thumbnail_path : t -> Contact.t -> Eio.Fs.dir_ty Eio.Path.t option

(** {1 Searching} *)

(** [find_by_handle t handle] finds a contact by exact handle match.

    This is an alias for {!lookup} for API compatibility.

    @return [Some contact] if found, [None] if not found *)
val find_by_handle : t -> string -> Contact.t option

(** [find_by_name t name] searches for contacts by name.

    Performs a case-insensitive search through all contacts,
    checking if any of their names match the provided name.

    @param name The name to search for (case-insensitive)
    @return The matching contact if exactly one match is found
    @raise Not_found if no contacts match the name
    @raise Invalid_argument if multiple contacts match the name *)
val find_by_name : t -> string -> Contact.t

(** [lookup_by_name t name] searches for contacts by name, raising on failure.

    Like {!find_by_name} but raises [Failure] instead of [Not_found]
    or [Invalid_argument]. This matches the semantics of Bushel's
    original contact lookup.

    @param name The name to search for (case-insensitive)
    @return The matching contact if exactly one match is found
    @raise Failure if no contacts match or multiple contacts match *)
val lookup_by_name : t -> string -> Contact.t

(** [find_by_name_opt t name] searches for contacts by name, returning an option.

    Like {!find_by_name} but returns [None] instead of raising exceptions
    when no match or multiple matches are found.

    @param name The name to search for (case-insensitive)
    @return [Some contact] if exactly one match is found, [None] otherwise *)
val find_by_name_opt : t -> string -> Contact.t option

(** [search_all t query] searches for contacts matching a query string.

    Performs a flexible search through all contact names, looking for:
    - Exact matches (case-insensitive)
    - Names that start with the query
    - Multi-word names where any word starts with the query

    This is useful for autocomplete or fuzzy search functionality.

    @param t The contact store
    @param query The search query (case-insensitive)
    @return A list of matching contacts, sorted by handle *)
val search_all : t -> string -> Contact.t list

(** {1 Temporal Queries} *)

(** [find_by_email_at t ~email ~date] finds a contact by email address at a specific date.

    Searches for a contact that had the given email address valid at [date].

    @param email Email address to search for
    @param date ISO 8601 date string
    @return The first matching contact, or [None] if not found *)
val find_by_email_at : t -> email:string -> date:Temporal.date ->
                       Contact.t option

(** [find_by_org t ~org ?from ?until ()] finds contacts who worked at an organization.

    Searches for contacts whose organization records overlap with the given period.
    If [from] and [until] are omitted, returns all contacts who ever worked there.

    @param org Organization name (case-insensitive substring match)
    @param from Start date of period to check (inclusive, optional)
    @param until End date of period to check (exclusive, optional)
    @return List of matching contacts, sorted by handle *)
val find_by_org : t -> org:string -> ?from:Temporal.date ->
                  ?until:Temporal.date -> unit -> Contact.t list

(** [list_at t ~date] returns contacts that were active at a specific date.

    A contact is considered active at a date if it has at least one
    email, organization, or URL valid at that date.

    @param date ISO 8601 date string
    @return List of active contacts at that date *)
val list_at : t -> date:Temporal.date -> Contact.t list

(** {1 Utilities} *)

(** [handle_of_name name] generates a handle from a full name.

    Creates a handle by concatenating the initials of all words
    in the name with the full last name, all in lowercase.

    Examples:
    - "Anil Madhavapeddy" -> "ammadhavapeddy"
    - "John Smith" -> "jssmith"

    @param name The full name to convert
    @return A suggested handle *)
val handle_of_name : string -> string

(** {1 Pretty Printing} *)

(** [pp ppf t] pretty prints the contact store showing statistics. *)
val pp : Format.formatter -> t -> unit

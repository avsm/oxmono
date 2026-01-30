(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Git-backed contact store with automatic version control.

    This module wraps {!Sortal_store} to provide automatic git versioning
    of all contact modifications. Each change (add, update, delete) is
    automatically committed to a git repository with descriptive commit
    messages. *)

module Contact = Sortal_schema.Contact

type t
(** A git-backed contact store. *)

(** {1 Creation and Initialization} *)

val create : Sortal_store.t -> Eio_unix.Stdenv.base -> t
(** [create store env] creates a git-backed store wrapping [store].

    @param store The underlying contact store
    @param env The Eio environment for spawning git processes *)

val init : t -> (unit, string) result
(** [init t] initializes a git repository in the data directory.

    Creates a new git repository with an initial commit if one doesn't exist.
    Safe to call multiple times - returns [Ok ()] if already initialized.

    @return [Ok ()] if initialized successfully or already initialized,
            [Error msg] if git initialization fails *)

val is_initialized : t -> bool
(** [is_initialized t] checks if the data directory is a git repository.

    @return [true] if a .git directory exists, [false] otherwise *)

(** {1 Contact Operations} *)

val save : t -> Contact.t -> (unit, string) result
(** [save t contact] saves a contact and commits the change to git.

    If the contact is new, commits with message "Add contact @handle (Name)".
    If updating an existing contact, commits with "Update contact @handle (Name)".

    @param contact The contact to save *)

val delete : t -> string -> (unit, string) result
(** [delete t handle] deletes a contact and commits the removal to git.

    Commits with message "Delete contact @handle (Name)".

    @param handle The contact handle to delete
    @return [Error msg] if contact not found *)

(** {1 Contact Modification} *)

val add_email : t -> string -> Contact.email -> (unit, string) result
(** [add_email t handle email] adds an email to a contact and commits.

    Commits with message "Update @handle: add email address@example.com". *)

val remove_email : t -> string -> string -> (unit, string) result
(** [remove_email t handle address] removes an email and commits.

    Commits with message "Update @handle: remove email address@example.com". *)

val add_service : t -> string -> Contact.service -> (unit, string) result
(** [add_service t handle service] adds a service to a contact and commits.

    Commits with message "Update @handle: add service Kind (url)". *)

val remove_service : t -> string -> string -> (unit, string) result
(** [remove_service t handle url] removes a service and commits.

    Commits with message "Update @handle: remove service url". *)

val add_organization : t -> string -> Contact.organization -> (unit, string) result
(** [add_organization t handle org] adds an organization and commits.

    Commits with message "Update @handle: add organization Org Name". *)

val remove_organization : t -> string -> string -> (unit, string) result
(** [remove_organization t handle name] removes an organization and commits.

    Commits with message "Update @handle: remove organization Org Name". *)

val add_url : t -> string -> Contact.url_entry -> (unit, string) result
(** [add_url t handle url_entry] adds a URL and commits.

    Commits with message "Update @handle: add URL url". *)

val remove_url : t -> string -> string -> (unit, string) result
(** [remove_url t handle url] removes a URL and commits.

    Commits with message "Update @handle: remove URL url". *)

(** {1 Low-level Operations} *)

val update_contact : t -> string -> (Contact.t -> Contact.t) ->
                     msg:string -> (unit, string) result
(** [update_contact t handle f ~msg] updates a contact and commits with custom message.

    This is a low-level function that applies transformation [f] to the contact
    and commits with the provided commit message.

    @param handle The contact handle
    @param f Function to transform the contact
    @param msg The git commit message *)

val store : t -> Sortal_store.t
(** [store t] returns the underlying contact store.

    Use this when you need direct store access without git commits. *)

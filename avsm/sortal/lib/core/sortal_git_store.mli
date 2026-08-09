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

val set_account : t -> string -> Contact.Account.t -> (unit, string) result
(** [set_account t handle account] adds [account] to the contact named
    [handle], replacing any existing account on the same platform, and
    commits the change. *)

val unset_account : t -> string -> Contact.Platform.id -> (unit, string) result
(** [unset_account t handle platform] removes every account [handle] holds on
    [platform] and commits the change. *)

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

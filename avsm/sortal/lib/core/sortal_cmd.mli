(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Cmdliner terms and commands for contact management.

    This module provides ready-to-use Cmdliner terms for building
    CLI applications that work with contact metadata. *)

module Contact = Sortal_schema.Contact

(** {1 Command Implementations} *)

(** [list_cmd] is a Cmdliner command that lists all contacts.

    Returns a function that takes an XDG context and returns an exit code. *)
val list_cmd : (Xdge.t -> int)

(** [show_cmd handle] creates a command to show detailed contact information.

    @param handle The contact handle to display *)
val show_cmd : string -> (Xdge.t -> int)

(** [thumbnail_cmd handle] prints the thumbnail file path for a contact.

    Returns exit code 0 and prints the path if the contact has a thumbnail,
    or exit code 1 if the contact is not found or has no thumbnail.

    @param handle The contact handle to look up *)
val thumbnail_cmd : string -> (Xdge.t -> int)

(** [search_cmd query] creates a command to search contacts by name.

    @param query The search query string *)
val search_cmd : string -> (Xdge.t -> int)

(** [stats_cmd] is a command that shows database statistics. *)
val stats_cmd : unit -> (Xdge.t -> int)

(** [sync_cmd ~force] is a command that synchronizes and normalizes contact data.

    Performs the following operations:
    - Converts non-PNG thumbnail images to PNG using ImageMagick
    - Fetches face thumbnails from Immich for contacts without thumbnails
    When [force] is true, re-fetches all thumbnails and overwrites existing ones. *)
val sync_cmd : force:bool -> unit -> Xdge.t -> Eio_unix.Stdenv.base -> int

(** [git_init_cmd xdg env] initializes a git repository in the data directory.

    Once initialized, all contact modifications will be automatically committed.
    @param xdg XDG context
    @param env Eio environment for process spawning *)
val git_init_cmd : Xdge.t -> Eio_unix.Stdenv.base -> int

(** [add_cmd handle names kind email github url orcid xdg env] creates a new contact.

    @param handle Contact handle (unique identifier)
    @param names List of names (first is primary)
    @param kind Optional contact kind
    @param email Optional email address
    @param github Optional GitHub handle
    @param url Optional personal/professional website
    @param orcid Optional ORCID identifier
    @param xdg XDG context
    @param env Eio environment for git operations *)
val add_cmd : string -> string list -> Contact.kind option ->
              string option -> string option -> string option -> string option ->
              Xdge.t -> Eio_unix.Stdenv.base -> int

(** [delete_cmd handle xdg env] deletes a contact.

    @param handle The contact handle to delete
    @param xdg XDG context
    @param env Eio environment for git operations *)
val delete_cmd : string -> Xdge.t -> Eio_unix.Stdenv.base -> int

(** [set_cmd handle platform_key value xdg env] sets [handle]'s account on the
    platform named by [platform_key] to [value]. For a federated platform,
    [value] is [user@host]. It exits with a message listing every known
    platform key if [platform_key] does not name one. *)
val set_cmd : string -> string -> string -> Xdge.t -> Eio_unix.Stdenv.base -> int

(** [unset_cmd handle platform_key xdg env] removes [handle]'s account on the
    platform named by [platform_key]. It exits with a message listing every
    known platform key if [platform_key] does not name one. *)
val unset_cmd : string -> string -> Xdge.t -> Eio_unix.Stdenv.base -> int

(** [migrate_cmd ~dry_run xdg] rewrites every V1 contact file in the store
    at [xdg] into V2, or reports what it would rewrite when [dry_run] is
    true without writing anything. It prints the number migrated, the
    number already V2, and the number that failed, naming each failed
    contact with its reason, and returns a non-zero exit code if any
    contact failed to migrate. *)
val migrate_cmd : dry_run:bool -> Xdge.t -> int

(** {1 Cmdliner Info Objects} *)

(** [list_info] is the command info for the list command. *)
val list_info : Cmdliner.Cmd.info

(** [show_info] is the command info for the show command. *)
val show_info : Cmdliner.Cmd.info

(** [thumbnail_info] is the command info for the thumbnail command. *)
val thumbnail_info : Cmdliner.Cmd.info

(** [search_info] is the command info for the search command. *)
val search_info : Cmdliner.Cmd.info

(** [stats_info] is the command info for the stats command. *)
val stats_info : Cmdliner.Cmd.info

(** [sync_info] is the command info for the sync command. *)
val sync_info : Cmdliner.Cmd.info

(** [git_init_info] is the command info for the git-init command. *)
val git_init_info : Cmdliner.Cmd.info

(** [add_info] is the command info for the add command. *)
val add_info : Cmdliner.Cmd.info

(** [delete_info] is the command info for the delete command. *)
val delete_info : Cmdliner.Cmd.info

(** [set_info] is the command info for the set command. *)
val set_info : Cmdliner.Cmd.info

(** [unset_info] is the command info for the unset command. *)
val unset_info : Cmdliner.Cmd.info

(** [migrate_info] is the command info for the migrate command. *)
val migrate_info : Cmdliner.Cmd.info

(** {1 Cmdliner Argument Definitions} *)

(** [handle_arg] is the positional argument for a contact handle. *)
val handle_arg : string Cmdliner.Term.t

(** [query_arg] is the positional argument for a search query. *)
val query_arg : string Cmdliner.Term.t

(** [add_handle_arg] is the positional argument for a new contact handle. *)
val add_handle_arg : string Cmdliner.Term.t

(** [add_names_arg] is the repeatable option for contact names. *)
val add_names_arg : string list Cmdliner.Term.t

(** [add_kind_arg] is the optional argument for contact kind. *)
val add_kind_arg : Contact.kind option Cmdliner.Term.t

(** [add_email_arg] is the optional argument for email. *)
val add_email_arg : string option Cmdliner.Term.t

(** [add_github_arg] is the optional argument for GitHub handle. *)
val add_github_arg : string option Cmdliner.Term.t

(** [add_url_arg] is the optional argument for URL. *)
val add_url_arg : string option Cmdliner.Term.t

(** [add_orcid_arg] is the optional argument for ORCID. *)
val add_orcid_arg : string option Cmdliner.Term.t

(** [platform_arg] is the positional argument for a platform key, used by
    both [set] and [unset]. *)
val platform_arg : string Cmdliner.Term.t

(** [value_arg] is the positional argument for an account handle. *)
val value_arg : string Cmdliner.Term.t

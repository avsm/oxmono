(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Cmdliner terms and commands for contact management.

    This module provides ready-to-use Cmdliner terms for building
    CLI applications that work with contact metadata. *)

module Contact = Sortal_schema.Contact
module Temporal = Sortal_schema.Temporal

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

(** [sync_cmd] is a command that synchronizes and normalizes contact data.

    Currently performs the following operations:
    - Converts non-JPG thumbnail images to PNG using ImageMagick *)
val sync_cmd : unit -> (Xdge.t -> int)

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
val add_cmd : string -> string list -> Contact.contact_kind option ->
              string option -> string option -> string option -> string option ->
              Xdge.t -> Eio_unix.Stdenv.base -> int

(** [delete_cmd handle xdg env] deletes a contact.

    @param handle The contact handle to delete
    @param xdg XDG context
    @param env Eio environment for git operations *)
val delete_cmd : string -> Xdge.t -> Eio_unix.Stdenv.base -> int

(** [add_email_cmd handle address type_ from until note xdg env] adds an email to a contact.

    @param handle Contact handle
    @param address Email address
    @param type_ Email type (work, personal, other)
    @param from Start date of validity
    @param until End date of validity
    @param note Contextual note
    @param xdg XDG context
    @param env Eio environment for git operations *)
val add_email_cmd : string -> string -> Contact.email_type option ->
                    string option -> string option -> string option ->
                    Xdge.t -> Eio_unix.Stdenv.base -> int

(** [remove_email_cmd handle address xdg env] removes an email from a contact. *)
val remove_email_cmd : string -> string -> Xdge.t -> Eio_unix.Stdenv.base -> int

(** [add_service_cmd handle url kind service_handle label xdg env] adds a service to a contact.

    @param handle Contact handle
    @param url Service URL
    @param kind Service kind
    @param service_handle Service username/handle
    @param label Human-readable label
    @param xdg XDG context
    @param env Eio environment for git operations *)
val add_service_cmd : string -> string -> Contact.service_kind option ->
                      string option -> string option -> Xdge.t -> Eio_unix.Stdenv.base -> int

(** [remove_service_cmd handle url xdg env] removes a service from a contact. *)
val remove_service_cmd : string -> string -> Xdge.t -> Eio_unix.Stdenv.base -> int

(** [add_org_cmd handle org_name title department from until org_email org_url xdg env]
    adds an organization to a contact. *)
val add_org_cmd : string -> string -> string option -> string option ->
                  string option -> string option -> string option -> string option ->
                  Xdge.t -> Eio_unix.Stdenv.base -> int

(** [remove_org_cmd handle org_name xdg env] removes an organization from a contact. *)
val remove_org_cmd : string -> string -> Xdge.t -> Eio_unix.Stdenv.base -> int

(** [add_url_cmd handle url label xdg env] adds a URL to a contact. *)
val add_url_cmd : string -> string -> string option -> Xdge.t -> Eio_unix.Stdenv.base -> int

(** [remove_url_cmd handle url xdg env] removes a URL from a contact. *)
val remove_url_cmd : string -> string -> Xdge.t -> Eio_unix.Stdenv.base -> int

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

(** [add_email_info] is the command info for the add-email command. *)
val add_email_info : Cmdliner.Cmd.info

(** [remove_email_info] is the command info for the remove-email command. *)
val remove_email_info : Cmdliner.Cmd.info

(** [add_service_info] is the command info for the add-service command. *)
val add_service_info : Cmdliner.Cmd.info

(** [remove_service_info] is the command info for the remove-service command. *)
val remove_service_info : Cmdliner.Cmd.info

(** [add_org_info] is the command info for the add-org command. *)
val add_org_info : Cmdliner.Cmd.info

(** [remove_org_info] is the command info for the remove-org command. *)
val remove_org_info : Cmdliner.Cmd.info

(** [add_url_info] is the command info for the add-url command. *)
val add_url_info : Cmdliner.Cmd.info

(** [remove_url_info] is the command info for the remove-url command. *)
val remove_url_info : Cmdliner.Cmd.info

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
val add_kind_arg : Contact.contact_kind option Cmdliner.Term.t

(** [add_email_arg] is the optional argument for email. *)
val add_email_arg : string option Cmdliner.Term.t

(** [add_github_arg] is the optional argument for GitHub handle. *)
val add_github_arg : string option Cmdliner.Term.t

(** [add_url_arg] is the optional argument for URL. *)
val add_url_arg : string option Cmdliner.Term.t

(** [add_orcid_arg] is the optional argument for ORCID. *)
val add_orcid_arg : string option Cmdliner.Term.t

(** [email_address_arg] is the positional argument for email address. *)
val email_address_arg : string Cmdliner.Term.t

(** [email_type_arg] is the optional argument for email type. *)
val email_type_arg : Contact.email_type option Cmdliner.Term.t

(** [date_arg name] creates a date argument with the given option name. *)
val date_arg : string -> string option Cmdliner.Term.t

(** [note_arg] is the optional argument for notes. *)
val note_arg : string option Cmdliner.Term.t

(** [service_url_arg] is the positional argument for service URL. *)
val service_url_arg : string Cmdliner.Term.t

(** [service_kind_arg] is the optional argument for service kind. *)
val service_kind_arg : Contact.service_kind option Cmdliner.Term.t

(** [service_handle_arg] is the optional argument for service handle. *)
val service_handle_arg : string option Cmdliner.Term.t

(** [label_arg] is the optional argument for labels. *)
val label_arg : string option Cmdliner.Term.t

(** [org_name_arg] is the positional argument for organization name. *)
val org_name_arg : string Cmdliner.Term.t

(** [org_title_arg] is the optional argument for job title. *)
val org_title_arg : string option Cmdliner.Term.t

(** [org_department_arg] is the optional argument for department. *)
val org_department_arg : string option Cmdliner.Term.t

(** [org_email_arg] is the optional argument for work email. *)
val org_email_arg : string option Cmdliner.Term.t

(** [org_url_arg] is the optional argument for work URL. *)
val org_url_arg : string option Cmdliner.Term.t

(** [url_value_arg] is the positional argument for URL. *)
val url_value_arg : string Cmdliner.Term.t

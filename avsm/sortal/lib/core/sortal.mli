(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Sortal - Username to metadata mapping with XDG storage

    This library provides a system for mapping usernames to various metadata
    including URLs, emails, ORCID identifiers, and social media handles.
    It uses XDG Base Directory Specification for storage locations and
    provides temporal support for time-bounded information like historical
    email addresses and employment records.

    {b Storage:}

    Contact metadata is stored as YAML files in the XDG data directory,
    with one file per contact using the handle as the filename. The YAML
    format uses the same Jsont codec definitions as JSON for seamless
    compatibility.

    {b Typical Usage:}

    {[
      let store = Sortal.create env#fs "myapp" in
      let contact = Sortal.Contact.make
        ~handle:"avsm"
        ~names:["Anil Madhavapeddy"]
        ~email:"anil@recoil.org"
        ~github:"avsm"
        ~orcid:"0000-0002-7890-1234"
        () in
      Sortal.save store contact;

      match Sortal.lookup store "avsm" with
      | Some c -> Printf.printf "Found: %s\n" (Sortal.Contact.name c)
      | None -> Printf.printf "Not found\n"
    ]}
*)

(** {1 Schema Modules}

    These modules define the data types and serialization formats.
    They are re-exported from {!Sortal_schema} for convenience.
    For version-specific access, use [Sortal_schema.V1.*]. *)

(** Temporal validity support for time-bounded contact fields. *)
module Temporal = Sortal_schema.Temporal

(** Feed subscription metadata. *)
module Feed = Sortal_schema.Feed

(** Contact metadata with temporal support. *)
module Contact = Sortal_schema.Contact

(** {1 Core Modules} *)

(** Contact store with XDG-compliant storage. *)
module Store = Sortal_store

(** Git-backed contact store with automatic version control. *)
module Git_store = Sortal_git_store

(** Cmdliner integration for CLI applications. *)
module Cmd = Sortal_cmd

(** {1 Convenience Re-exports}

    These are re-exported from {!Store} for easier top-level access. *)

(** The contact store type. *)
type t = Store.t

(** [create fs app_name] creates a new contact store.
    See {!Store.create} for details. *)
val create : Eio.Fs.dir_ty Eio.Path.t -> string -> t

(** [create_from_xdg xdg] creates a contact store from an XDG context.
    See {!Store.create_from_xdg} for details. *)
val create_from_xdg : Xdge.t -> t

(** [save t contact] saves a contact to the store.
    See {!Store.save} for details. *)
val save : t -> Contact.t -> unit

(** [lookup t handle] retrieves a contact by handle.
    See {!Store.lookup} for details. *)
val lookup : t -> string -> Contact.t option

(** [delete t handle] removes a contact from the store.
    See {!Store.delete} for details. *)
val delete : t -> string -> unit

(** [list t] returns all contacts in the store.
    See {!Store.list} for details. *)
val list : t -> Contact.t list

(** [thumbnail_path t contact] returns the path to a contact's thumbnail.
    See {!Store.thumbnail_path} for details. *)
val thumbnail_path : t -> Contact.t -> Eio.Fs.dir_ty Eio.Path.t option

(** [png_thumbnail_path t contact] returns the path to the PNG version of a contact's thumbnail.
    See {!Store.png_thumbnail_path} for details. *)
val png_thumbnail_path : t -> Contact.t -> Eio.Fs.dir_ty Eio.Path.t option

(** [find_by_name t name] searches for contacts by name.
    See {!Store.find_by_name} for details. *)
val find_by_name : t -> string -> Contact.t

(** [find_by_name_opt t name] searches for contacts by name.
    See {!Store.find_by_name_opt} for details. *)
val find_by_name_opt : t -> string -> Contact.t option

(** [search_all t query] searches for contacts matching a query.
    See {!Store.search_all} for details. *)
val search_all : t -> string -> Contact.t list

(** [handle_of_name name] generates a handle from a full name.
    See {!Store.handle_of_name} for details. *)
val handle_of_name : string -> string

(** [pp ppf t] pretty prints the contact store.
    See {!Store.pp} for details. *)
val pp : Format.formatter -> t -> unit

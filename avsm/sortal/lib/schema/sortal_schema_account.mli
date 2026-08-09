(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Accounts a contact holds on a platform.

    An account is a handle on a platform. Its URL is derived from the two and
    is never stored, so a platform, a handle and a URL cannot disagree. *)

module Platform = Sortal_schema_platform

type app = Bluesky | Tangled | Standard_site
(** A front end onto a single AT Protocol identity. *)

type atproto = {
  handle : string;      (** the AT Protocol handle, such as ["anil.recoil.org"] *)
  did : string option;  (** the resolved DID, [None] until a probe fills it *)
  apps : app list;      (** the front ends this identity is reachable through *)
}
(** An AT Protocol identity. One handle names one person, and [apps] lists
    the services that identity is usable through. *)

type t =
  | Simple of Platform.simple * string
      (** [Simple (p, handle)] is [handle] on [p]. *)
  | Federated of Platform.federated * string * string
      (** [Federated (p, user, host)] is [user] at [host] on [p]. *)
  | Atproto of atproto

val platform : t -> Platform.id
(** [platform a] is the platform [a] is held on. *)

val handle : t -> string
(** [handle a] is [a]'s handle. For a federated account this is the
    [user@host] form, and for an AT Protocol account it is the bare handle. *)

val url : t -> string
(** [url a] is [a]'s canonical URL, derived from its platform and handle. For
    an AT Protocol account this is the URL of its first app, or the Bluesky
    URL if it lists none. *)

val app_url : atproto -> app -> string
(** [app_url a app] is the URL of [a]'s identity on [app]. *)

val app_to_string : app -> string
(** [app_to_string app] is [app]'s name as it appears in YAML. *)

val app_of_string : string -> app option
(** [app_of_string s] is the app [s] names, or [None]. *)

val check : t -> (unit, string) result
(** [check a] is [Ok ()] if [a]'s handle is syntactically valid for its
    platform, or [Error why]. The check is local. *)

val json_t : t list Jsont.t
(** [json_t] maps the whole [accounts] mapping, because the mapping key
    carries the platform.

    A member's value is a string naming one handle, an array of strings
    naming several, or, for [atproto] alone, an object. A member whose name
    is not a platform key is a decoding error, and so is a member of the
    [atproto] object other than [handle], [did] and [apps]: adding a member
    there later requires a schema version bump, because an older reader
    rejects a file that uses it rather than skip it.

    Decoding goes through a string-keyed map, so the accounts come back
    ordered by platform key rather than in the order the file wrote them.
    Several handles under one key keep their order relative to each other.
    Nothing should depend on the order across platforms.

    Encoding canonicalises: a single-element sequence is written back as a
    scalar, and an [atproto] account with no [did] and no [apps] is written
    back as a bare handle rather than an object. A round trip through
    [json_t] preserves the decoded value, not the source syntax. *)

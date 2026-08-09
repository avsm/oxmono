(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Contact schema V2.

    A contact is an identity, the accounts it is reachable through, and the
    context around it. V2 replaces V1's [services], [urls], [orcid] and
    [atproto] fields, which recorded the same fact four ways, with a single
    {!Sortal_schema_account.t} list. *)

module Account = Sortal_schema_account
module Platform = Sortal_schema_platform
module Date = Sortal_schema_date
module Feed = Sortal_schema_feed

val version : int
(** [version] is the schema version this module reads and writes, [2]. *)

type kind = Person | Organization

type link = {
  url : string;
  label : string option;  (** a human description, rarely needed *)
}
(** A web page that is not an account. *)

type affiliation = {
  org : string;
  department : string option;
  title : string option;
  url : string option;
  address : string option;
  from : Date.t option;   (** inclusive *)
  until : Date.t option;  (** exclusive *)
}
(** An employment or academic affiliation. This is the only part of the
    schema that carries a date. *)

type t

val make :
  handle:string ->
  names:string list ->
  ?kind:kind ->
  ?emails:string list ->
  ?accounts:Account.t list ->
  ?links:link list ->
  ?affiliations:affiliation list ->
  ?photo:string ->
  ?feeds:Feed.t list ->
  ?vcard:(string * string) list ->
  unit -> t
  @@ portable
(** [make ~handle ~names ()] is a contact. [kind] defaults to [Person] and
    every list defaults to empty. [names] must not be empty, and its first
    entry is the primary name. [make] does not itself enforce this: use
    {!check} to reject an empty list. *)

(** {1 Accessors}

    Every accessor below is a pure projection out of {!t}, so it is
    [@@ portable] and may be called from a portable page such as
    {!Sortal_web}'s. *)

val kind : t -> kind @@ portable
val handle : t -> string @@ portable
val names : t -> string list @@ portable

val name : t -> string @@ portable
(** [name t] is [t]'s primary name, the first of {!names}, or [t]'s handle
    if {!names} is empty. *)

val emails : t -> string list @@ portable
(** [emails t] is [t]'s addresses, preferred first. *)

val accounts : t -> Account.t list @@ portable
val links : t -> link list @@ portable
val affiliations : t -> affiliation list @@ portable
val photo : t -> string option @@ portable
val feeds : t -> Feed.t list @@ portable

val vcard : t -> (string * string) list @@ portable
(** [vcard t] is [t]'s reserved passthrough data for a future CardDAV
    importer. Nothing in this module interprets it. *)

(** {1 Account queries} *)

val accounts_on : t -> Platform.id -> Account.t list @@ portable
(** [accounts_on t p] is every account [t] holds on [p]. Several accounts on
    one platform keep the order they were written in. *)

val account_on : t -> Platform.id -> Account.t option @@ portable
(** [account_on t p] is [t]'s first account on [p]. *)

val handle_on : t -> Platform.id -> string option @@ portable
(** [handle_on t p] is the handle of [t]'s first account on [p]. *)

val url_on : t -> Platform.id -> string option @@ portable
(** [url_on t p] is the URL of [t]'s first account on [p]. *)

val atproto : t -> Account.atproto option @@ portable
(** [atproto t] is [t]'s AT Protocol identity. *)

val atproto_handle : t -> string option @@ portable
val atproto_did : t -> string option @@ portable

val set_atproto_did : t -> string -> t @@ portable
(** [set_atproto_did t did] is [t] with its AT Protocol DID set. It is [t]
    unchanged if [t] has no AT Protocol account. *)

val best_url : t -> string option @@ portable
(** [best_url t] is the URL a reader should follow to find [t]. It is the
    first link if there is one, and otherwise the URL of the account whose
    platform sorts first by key. Account order does not survive decoding, so
    this cannot be "the first account written". *)

val current_affiliation : t -> affiliation option @@ portable
(** [current_affiliation t] is [t]'s first affiliation with no [until] date. *)

(** {1 Modification} *)

val add_feed : t -> Feed.t -> t

val remove_feed : t -> string -> t
(** [remove_feed t url] is [t] without any feed whose URL is [url]. It is
    [t] unchanged if no feed has that URL. *)

val check : t -> (unit, string) result @@ portable
(** [check t] is [Ok ()] if [t]'s names are non-empty and every account
    passes {!Sortal_schema_account.check}, or [Error why]. *)

(** {1 Comparison, display and encoding} *)

val compare : t -> t -> int @@ portable
val pp : Format.formatter -> t -> unit

val json_t : t Jsont.t
(** [json_t] maps a V2 contact. The [version] member is always encoded and
    must equal [2] on decoding, so a V1 file is rejected rather than
    misread. Empty collections and absent options are omitted on encoding. A
    link with no label encodes as a bare string. *)

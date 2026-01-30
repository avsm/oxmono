(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Contact schema V1 with temporal support.

    This module defines the V1 contact schema with support for time-bounded
    information such as emails and organizations that are valid only during
    specific periods.

    {b Schema Version Policy:}
    - New optional fields can be added without bumping the version
    - The version must be bumped only if the {i meaning} of an existing
      field changes
    - This allows forward compatibility: older readers can ignore new fields *)

(** {1 Schema Version} *)

val version : int
(** The schema version number for V1. Currently [1]. *)

(** {1 Types} *)

(** Contact kind - what type of entity this represents. *)
type contact_kind =
  | Person         (** Individual person *)
  | Organization   (** Company, lab, department *)
  | Group          (** Research group, project team *)
  | Role           (** Generic role email like info@, admin@ *)

(** ActivityPub service variants. *)
type activitypub_variant =
  | Mastodon          (** Mastodon instance *)
  | Pixelfed          (** Pixelfed instance *)
  | PeerTube          (** PeerTube instance *)
  | Other_activitypub of string  (** Other ActivityPub-compatible service *)

(** Service kind - categorization of online presence. *)
type service_kind =
  | ActivityPub of activitypub_variant  (** ActivityPub-compatible services *)
  | Bluesky        (** Bluesky / AT Protocol *)
  | Github         (** GitHub *)
  | Git            (** GitLab, Gitea, Codeberg, etc *)
  | Twitter        (** Twitter/X *)
  | Photo          (** Immich, Flickr, Instagram, etc *)
  | Custom of string  (** Other service types *)

(** An online service/identity. *)
type service = {
  url: string;                         (** Full URL (primary identifier) *)
  kind: service_kind option;           (** Optional service categorization *)
  handle: string option;               (** Optional short handle/username *)
  label: string option;                (** Human description: "Cambridge GitLab", "Work account" *)
  range: Sortal_schema_temporal.range option; (** Temporal validity *)
  primary: bool;                       (** Is this the primary/preferred service of its kind? *)
}

type email_type = Work | Personal | Other

type email = {
  address: string;
  type_: email_type option;
  range: Sortal_schema_temporal.range option;  (** Validity period *)
  note: string option;                  (** Context note, e.g., "NetApp position" *)
}

type organization = {
  name: string;
  title: string option;
  department: string option;
  range: Sortal_schema_temporal.range option;  (** Employment period *)
  email: string option;                 (** Work email during this period *)
  url: string option;                   (** Work homepage during this period *)
}

type url_entry = {
  url: string;
  label: string option;                 (** Human-readable label *)
  range: Sortal_schema_temporal.range option;  (** Validity period *)
}

type t = {
  version: int;                         (** Schema version (always 1 for V1) *)
  kind: contact_kind;                   (** Type of entity (Person, Organization, etc) *)
  handle: string;                       (** Unique identifier *)
  names: string list;                   (** Names, first is primary *)

  (* Temporal fields *)
  emails: email list;                   (** Email addresses with temporal validity *)
  organizations: organization list;     (** Employment/affiliation history *)
  urls: url_entry list;                 (** URLs with optional temporal validity *)
  services: service list;               (** Online services/identities *)

  (* Simple fields - rarely change over time *)
  icon: string option;                  (** Avatar URL *)
  thumbnail: string option;             (** Local thumbnail path *)
  orcid: string option;                 (** ORCID identifier *)

  (* Other *)
  feeds: Sortal_schema_feed.t list option;     (** Feed subscriptions *)
}

(** {1 Construction} *)

(** [make ~handle ~names ?kind ?emails ?organizations ?urls ?services
         ?icon ?thumbnail ?orcid ?feeds ()]
    creates a new V1 contact.

    The [version] field is automatically set to [1].
    The [kind] defaults to [Person] if not specified. *)
val make :
  handle:string ->
  names:string list ->
  ?kind:contact_kind ->
  ?emails:email list ->
  ?organizations:organization list ->
  ?urls:url_entry list ->
  ?services:service list ->
  ?icon:string ->
  ?thumbnail:string ->
  ?orcid:string ->
  ?feeds:Sortal_schema_feed.t list ->
  unit ->
  t

(** {1 Email Helpers} *)

(** [make_email ?type_ ?from ?until ?note address] creates an email entry.

    @param type_ Email type (Work, Personal, Other)
    @param from Start date of validity
    @param until End date of validity (exclusive)
    @param note Contextual note *)
val make_email :
  ?type_:email_type ->
  ?from:Sortal_schema_temporal.date ->
  ?until:Sortal_schema_temporal.date ->
  ?note:string ->
  string ->
  email

(** [email_of_string s] creates a simple always-valid personal email. *)
val email_of_string : string -> email

(** {1 Organization Helpers} *)

(** [make_org ?title ?department ?from ?until ?email ?url name]
    creates an organization entry. *)
val make_org :
  ?title:string ->
  ?department:string ->
  ?from:Sortal_schema_temporal.date ->
  ?until:Sortal_schema_temporal.date ->
  ?email:string ->
  ?url:string ->
  string ->
  organization

(** {1 URL Helpers} *)

(** [make_url ?label ?from ?until url] creates a URL entry. *)
val make_url :
  ?label:string ->
  ?from:Sortal_schema_temporal.date ->
  ?until:Sortal_schema_temporal.date ->
  string ->
  url_entry

(** [url_of_string s] creates a simple always-valid URL. *)
val url_of_string : string -> url_entry

(** {1 Service Helpers} *)

(** [make_service ?kind ?handle ?label ?from ?until ?primary url]
    creates a service entry.

    @param kind Optional service categorization
    @param handle Optional short handle/username
    @param label Optional description (e.g., "Work account", "Cambridge GitLab")
    @param from Start date of validity
    @param until End date of validity (exclusive)
    @param primary Whether this is the primary service of its kind
    @param url Full URL to the service (required) *)
val make_service :
  ?kind:service_kind ->
  ?handle:string ->
  ?label:string ->
  ?from:Sortal_schema_temporal.date ->
  ?until:Sortal_schema_temporal.date ->
  ?primary:bool ->
  string ->
  service

(** [service_of_url url] creates a simple always-valid service from just a URL. *)
val service_of_url : string -> service

(** {1 Accessors} *)

val version_of : t -> int
val kind : t -> contact_kind
val handle : t -> string
val names : t -> string list
val name : t -> string
val primary_name : t -> string
val emails : t -> email list
val organizations : t -> organization list
val urls : t -> url_entry list
val services : t -> service list
val icon : t -> string option
val thumbnail : t -> string option
val orcid : t -> string option
val feeds : t -> Sortal_schema_feed.t list option

(** {1 Service Convenience Accessors}

    These accessors provide easy access to common service types. *)

(** [github t] returns the GitHub service entry if present. *)
val github : t -> service option

(** [github_handle t] returns the GitHub username if present. *)
val github_handle : t -> string option

(** [twitter t] returns the Twitter/X service entry if present. *)
val twitter : t -> service option

(** [twitter_handle t] returns the Twitter/X username if present. *)
val twitter_handle : t -> string option

(** [mastodon t] returns the Mastodon service entry if present. *)
val mastodon : t -> service option

(** [mastodon_handle t] returns the Mastodon handle if present. *)
val mastodon_handle : t -> string option

(** [bluesky t] returns the Bluesky service entry if present. *)
val bluesky : t -> service option

(** [bluesky_handle t] returns the Bluesky handle if present. *)
val bluesky_handle : t -> string option

(** {1 Temporal Queries} *)

(** [email_at t ~date] returns the primary email valid at [date]. *)
val email_at : t -> date:Sortal_schema_temporal.date -> string option

(** [emails_at t ~date] returns all emails valid at [date]. *)
val emails_at : t -> date:Sortal_schema_temporal.date -> email list

(** [current_email t] returns the current primary email. *)
val current_email : t -> string option

(** [organization_at t ~date] returns the organization at [date]. *)
val organization_at : t -> date:Sortal_schema_temporal.date -> organization option

(** [current_organization t] returns the current organization. *)
val current_organization : t -> organization option

(** [url_at t ~date] returns the primary URL valid at [date]. *)
val url_at : t -> date:Sortal_schema_temporal.date -> string option

(** [current_url t] returns the current primary URL. *)
val current_url : t -> string option

(** [all_email_addresses t] returns all email addresses (any period). *)
val all_email_addresses : t -> string list

(** [best_url t] returns the best available URL (current URL or service fallback). *)
val best_url : t -> string option

(** {1 Service Queries} *)

(** [services_of_kind t kind] returns all services matching the given kind. *)
val services_of_kind : t -> service_kind -> service list

(** [services_at t ~date] returns all services valid at [date]. *)
val services_at : t -> date:Sortal_schema_temporal.date -> service list

(** [current_services t] returns all currently valid services. *)
val current_services : t -> service list

(** [primary_service t kind] returns the primary service of the given kind. *)
val primary_service : t -> service_kind -> service option

(** {1 Modification} *)

val add_feed : t -> Sortal_schema_feed.t -> t
val remove_feed : t -> string -> t

(** {1 Comparison and Display} *)

val compare : t -> t -> int
val pp : Format.formatter -> t -> unit

(** {1 JSON Encoding} *)

(** [json_t] is the jsont encoder/decoder for V1 contacts.

    The schema includes a [version] field that is always encoded and
    must equal [1] when decoded. *)
val json_t : t Jsont.t

(** {1 Type Utilities} *)

val contact_kind_to_string : contact_kind -> string
val contact_kind_of_string : string -> contact_kind option

val activitypub_variant_to_string : activitypub_variant -> string
val activitypub_variant_of_string : string -> activitypub_variant

val service_kind_to_string : service_kind -> string
val service_kind_of_string : string -> service_kind option

val email_type_to_string : email_type -> string
val email_type_of_string : string -> email_type option

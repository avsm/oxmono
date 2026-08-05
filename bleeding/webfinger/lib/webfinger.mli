(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** RFC 7033 WebFinger and RFC 7565 acct URI scheme.

    This module implements the WebFinger protocol as specified in
    {{:https://datatracker.ietf.org/doc/html/rfc7033}RFC 7033}, providing
    type-safe JSON Resource Descriptor (JRD) encoding/decoding and an
    HTTP client for WebFinger queries. It also implements the acct URI
    scheme as specified in {{:https://datatracker.ietf.org/doc/html/rfc7565}RFC 7565}.

    {2 Example}
    {[
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let session = Requests.create ~sw env in
      let acct = Webfinger.Acct.of_string_exn "acct:user@example.com" in
      match Webfinger.query_acct session acct () with
      | Ok jrd ->
          Format.printf "%a@." Webfinger.Jrd.pp jrd;
          begin match Webfinger.Jrd.find_link ~rel:"self" jrd with
          | Some link -> Format.printf "ActivityPub: %s@." (Option.get (Webfinger.Link.href link))
          | None -> ()
          end
      | Error e ->
          Format.eprintf "Error: %a@." Webfinger.pp_error e
    ]}

    {2 References}
    {ul
    {- {{:https://datatracker.ietf.org/doc/html/rfc7033}RFC 7033} - WebFinger}
    {- {{:https://datatracker.ietf.org/doc/html/rfc7565}RFC 7565} - The 'acct' URI Scheme}
    {- {{:https://datatracker.ietf.org/doc/html/rfc6415}RFC 6415} - Web Host Metadata}} *)

(** {1 Error Types} *)

type error =
  | Invalid_resource of string
      (** The resource parameter is missing or malformed. *)
  | Http_error of { status : int; body : string }
      (** HTTP request failed with the given status code. *)
  | Json_error of string
      (** Failed to parse JRD JSON response. *)
  | Https_required
      (** WebFinger requires HTTPS but HTTP was requested. *)
  | Not_found
      (** The server has no information about the requested resource. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error fmt e] pretty-prints an error. *)

val error_to_string : error -> string
(** [error_to_string e] converts an error to a human-readable string. *)

exception Webfinger_error of error
(** Exception raised by [*_exn] functions. *)

(** {1 Acct URI}

    The acct URI scheme as specified in
    {{:https://datatracker.ietf.org/doc/html/rfc7565}RFC 7565}. *)

module Acct : sig
  type t
  (** An acct URI identifying a user account at a service provider.

      Per {{:https://datatracker.ietf.org/doc/html/rfc7565#section-4}RFC 7565 Section 4},
      an acct URI is used for identification only, not interaction. *)

  val make : userpart:string -> host:string -> t
  (** [make ~userpart ~host] creates an acct URI.

      The [userpart] should be the raw (unencoded) account name. Any characters
      requiring percent-encoding (such as [@] in email addresses) will be
      automatically encoded by {!to_string}.

      @raise Invalid_argument if [userpart] or [host] is empty. *)

  val of_string : string -> (t, error) result
  (** [of_string s] parses an acct URI string.

      Accepts URIs of the form ["acct:userpart\@host"]. The userpart may contain
      percent-encoded characters which are decoded. *)

  val of_string_exn : string -> t
  (** [of_string_exn s] is like {!of_string} but raises {!Webfinger_error}. *)

  val to_string : t -> string
  (** [to_string acct] serializes an acct URI to string form.

      Characters in the userpart that require encoding per RFC 7565 are
      percent-encoded. *)

  val userpart : t -> string
  (** [userpart acct] returns the decoded userpart (account name). *)

  val host : t -> string
  (** [host acct] returns the host (service provider domain). *)

  val equal : t -> t -> bool
  (** [equal a b] compares two acct URIs using case normalization and
      percent-encoding normalization as specified in
      {{:https://datatracker.ietf.org/doc/html/rfc3986#section-6.2.2}RFC 3986 Section 6.2.2}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp fmt acct] pretty-prints an acct URI. *)
end

(** {1 Link}

    Link relation object as specified in
    {{:https://datatracker.ietf.org/doc/html/rfc7033#section-4.4.4}RFC 7033 Section 4.4.4}. *)

module Link : sig
  type t
  (** A link relation in a JRD. *)

  val make :
    rel:string ->
    ?type_:string ->
    ?href:string ->
    ?titles:(string * string) list ->
    ?properties:(string * string option) list ->
    unit -> t
  (** [make ~rel ?type_ ?href ?titles ?properties ()] creates a link. *)

  val rel : t -> string
  (** [rel link] returns the link relation type. *)

  val type_ : t -> string option
  (** [type_ link] returns the media type. *)

  val href : t -> string option
  (** [href link] returns the target URI. *)

  val titles : t -> (string * string) list
  (** [titles link] returns all title/language pairs. *)

  val properties : t -> (string * string option) list
  (** [properties link] returns all link properties. *)

  val title : ?lang:string -> t -> string option
  (** [title ?lang link] returns the title for [lang] (default "und"). *)

  val property : uri:string -> t -> string option
  (** [property ~uri link] returns the property value for [uri]. *)

  val jsont : t Jsont.t
  (** JSON type descriptor for links. *)

  val pp : Format.formatter -> t -> unit
  (** [pp fmt link] pretty-prints a link. *)
end

(** {1 JRD}

    JSON Resource Descriptor as specified in
    {{:https://datatracker.ietf.org/doc/html/rfc7033#section-4.4}RFC 7033 Section 4.4}. *)

module Jrd : sig
  type t
  (** A JSON Resource Descriptor. *)

  val make :
    ?subject:string ->
    ?aliases:string list ->
    ?properties:(string * string option) list ->
    ?links:Link.t list ->
    unit -> t
  (** [make ?subject ?aliases ?properties ?links ()] creates a JRD. *)

  val subject : t -> string option
  (** [subject jrd] returns the subject URI. *)

  val aliases : t -> string list
  (** [aliases jrd] returns the list of alias URIs. *)

  val properties : t -> (string * string option) list
  (** [properties jrd] returns subject properties. *)

  val links : t -> Link.t list
  (** [links jrd] returns all links. *)

  val find_link : rel:string -> t -> Link.t option
  (** [find_link ~rel jrd] returns the first link with relation [rel]. *)

  val find_links : rel:string -> t -> Link.t list
  (** [find_links ~rel jrd] returns all links with relation [rel]. *)

  val property : uri:string -> t -> string option
  (** [property ~uri jrd] returns the property value for [uri]. *)

  val jsont : t Jsont.t
  (** JSON type descriptor for JRD. *)

  val of_string : string -> (t, error) result
  (** [of_string s] parses a JRD from JSON. *)

  val to_string : t -> string
  (** [to_string jrd] serializes a JRD to JSON. *)

  val pp : Format.formatter -> t -> unit
  (** [pp fmt jrd] pretty-prints a JRD. *)
end

(** {1 Common Link Relations} *)

module Rel : sig
  val activitypub : string
  (** ["self"] - ActivityPub actor profile. *)

  val openid : string
  (** OpenID Connect issuer. *)

  val profile : string
  (** Profile page. *)

  val avatar : string
  (** Avatar image. *)

  val feed : string
  (** Atom/RSS feed. *)

  val portable_contacts : string
  (** Portable Contacts. *)

  val oauth_authorization : string
  (** OAuth 2.0 authorization endpoint. *)

  val oauth_token : string
  (** OAuth 2.0 token endpoint. *)

  val subscribe : string
  (** Subscribe to resource (OStatus). *)

  val salmon : string
  (** Salmon endpoint (legacy). *)

  val magic_key : string
  (** Magic public key (legacy). *)
end

(** {1 URL Construction} *)

val webfinger_url : resource:string -> ?rels:string list -> string -> string
(** [webfinger_url ~resource ?rels host] constructs the WebFinger query URL.

    The [resource] is the URI to query (typically an acct: or https: URI).
    Optional [rels] filters the response to only include matching link relations. *)

val webfinger_url_acct : Acct.t -> ?rels:string list -> unit -> string
(** [webfinger_url_acct acct ?rels ()] constructs the WebFinger query URL for an acct URI.

    The host is automatically extracted from the acct URI. *)

val host_of_resource : string -> (string, error) result
(** [host_of_resource resource] extracts the host from an acct: or https: URI.

    For acct: URIs, returns the host portion after the [@].
    For https: URIs, returns the host from the URI. *)

(** {1 HTTP Client} *)

val query :
  Requests.t ->
  resource:string ->
  ?rels:string list ->
  unit -> (Jrd.t, error) result
(** [query session ~resource ?rels ()] performs a WebFinger query.

    Per {{:https://datatracker.ietf.org/doc/html/rfc7033#section-4}RFC 7033 Section 4}:
    - Queries use HTTPS
    - The Accept header requests application/jrd+json
    - 200 OK returns a JRD
    - 404 means no information available *)

val query_exn :
  Requests.t ->
  resource:string ->
  ?rels:string list ->
  unit -> Jrd.t
(** [query_exn session ~resource ?rels ()] is like {!query} but raises
    {!Webfinger_error} on failure. *)

val query_acct :
  Requests.t ->
  Acct.t ->
  ?rels:string list ->
  unit -> (Jrd.t, error) result
(** [query_acct session acct ?rels ()] performs a WebFinger query for an acct URI.

    This is the preferred way to query for user accounts as it ensures
    the resource is a valid acct URI per RFC 7565. *)

val query_acct_exn :
  Requests.t ->
  Acct.t ->
  ?rels:string list ->
  unit -> Jrd.t
(** [query_acct_exn session acct ?rels ()] is like {!query_acct} but raises
    {!Webfinger_error} on failure. *)

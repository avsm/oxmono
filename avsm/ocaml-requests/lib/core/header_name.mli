(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP Header Names as Polymorphic Variants

    This module provides type-safe HTTP header names using polymorphic variants.
    All standard headers have dedicated variants, with [`Other] for non-standard
    or unknown headers.

    {2 Usage}

    {[
      (* Use standard headers directly *)
      let headers = Headers.empty
        |> Headers.set `Content_type "application/json"
        |> Headers.set `Accept "text/html"

      (* Use custom headers with `Other *)
      let headers = headers
        |> Headers.set (`Other "X-Custom-Header") "value"

      (* Pattern match on headers *)
      match Headers.get `Content_type headers with
      | Some ct -> print_endline ct
      | None -> ()
    ]}

    Header names are case-insensitive per
    {{:https://datatracker.ietf.org/doc/html/rfc9110#section-5.1}RFC 9110 Section 5.1}.

    Header definitions are based on the IANA HTTP Field Name Registry:
    {{:https://www.iana.org/assignments/http-fields/http-fields.xhtml}IANA HTTP Field Name Registry} *)

(** {1 Types} *)

(** Standard HTTP header names.

    These cover headers defined in:
    - {{:https://datatracker.ietf.org/doc/html/rfc9110}RFC 9110} (HTTP Semantics)
    - {{:https://datatracker.ietf.org/doc/html/rfc9111}RFC 9111} (HTTP Caching)
    - {{:https://datatracker.ietf.org/doc/html/rfc9112}RFC 9112} (HTTP/1.1)
    - {{:https://datatracker.ietf.org/doc/html/rfc6455}RFC 6455} (WebSocket Protocol)
    - {{:https://datatracker.ietf.org/doc/html/rfc9421}RFC 9421} (HTTP Message Signatures)
    - {{:https://datatracker.ietf.org/doc/html/rfc9530}RFC 9530} (Digest Fields)
    - {{:https://fetch.spec.whatwg.org/}Fetch Standard} (CORS and Security)
    - Various other RFCs as noted *)
type standard = [
  (* RFC 9110: HTTP Semantics - Content Headers *)
  | `Accept
  | `Accept_encoding
  | `Accept_language
  | `Accept_ranges
  | `Allow
  | `Content_encoding
  | `Content_language
  | `Content_length
  | `Content_location
  | `Content_range
  | `Content_type

  (* RFC 9110: HTTP Semantics - Request Context *)
  | `Expect
  | `From
  | `Host
  | `Max_forwards
  | `Range
  | `Referer
  | `Te
  | `User_agent

  (* RFC 9110: HTTP Semantics - Response Context *)
  | `Location
  | `Retry_after
  | `Server

  (* RFC 9110: HTTP Semantics - Validators *)
  | `Etag
  | `Last_modified
  | `Vary

  (* RFC 9110: HTTP Semantics - Conditional Requests *)
  | `If_match
  | `If_modified_since
  | `If_none_match
  | `If_range
  | `If_unmodified_since

  (* RFC 9110: HTTP Semantics - Authentication *)
  | `Authorization
  | `Authentication_info
  | `Proxy_authenticate
  | `Proxy_authentication_info
  | `Proxy_authorization
  | `Www_authenticate

  (* RFC 9110: HTTP Semantics - Connection Management *)
  | `Connection
  | `Upgrade
  | `Via

  (* RFC 9110: HTTP Semantics - Date *)
  | `Date

  (* RFC 9111: HTTP Caching *)
  | `Age
  | `Cache_control
  | `Expires
  | `Pragma
  | `Cache_status

  (* RFC 9112: HTTP/1.1 *)
  | `Keep_alive
  | `Trailer
  | `Transfer_encoding

  (* Cookies - RFC 6265bis *)
  | `Cookie
  | `Set_cookie

  (* Link Relations - RFC 8288 *)
  | `Link

  (* CORS Headers - Fetch Standard *)
  | `Access_control_allow_credentials
  | `Access_control_allow_headers
  | `Access_control_allow_methods
  | `Access_control_allow_origin
  | `Access_control_expose_headers
  | `Access_control_max_age
  | `Access_control_request_headers
  | `Access_control_request_method
  | `Origin

  (* Cross-Origin Policy Headers - HTML Standard *)
  | `Cross_origin_embedder_policy
  | `Cross_origin_embedder_policy_report_only
  | `Cross_origin_opener_policy
  | `Cross_origin_opener_policy_report_only
  | `Cross_origin_resource_policy

  (* Fetch Metadata Headers - W3C *)
  | `Sec_fetch_dest
  | `Sec_fetch_mode
  | `Sec_fetch_site
  | `Sec_fetch_user

  (* Security Headers *)
  | `Content_security_policy
  | `Content_security_policy_report_only
  | `Strict_transport_security
  | `X_content_type_options
  | `X_frame_options
  | `Referrer_policy

  (* RFC 8053: Interactive Authentication *)
  | `Optional_www_authenticate
  | `Authentication_control

  (* RFC 9449: OAuth 2.0 DPoP *)
  | `Dpop
  | `Dpop_nonce

  (* RFC 9530: Digest Fields *)
  | `Content_digest
  | `Repr_digest
  | `Want_content_digest
  | `Want_repr_digest

  (* RFC 9421: HTTP Message Signatures *)
  | `Signature
  | `Signature_input
  | `Accept_signature

  (* RFC 6455: WebSocket Protocol *)
  | `Sec_websocket_key
  | `Sec_websocket_accept
  | `Sec_websocket_protocol
  | `Sec_websocket_version
  | `Sec_websocket_extensions
]

(** Complete header name type including non-standard headers.

    Use [`Other name] for headers not in the standard set.
    The name should be provided in its canonical form (e.g., "X-Custom-Header"). *)
type t = [ standard | `Other of string ]

(** {1 Conversion} *)

val to_string : t -> string
(** [to_string name] converts a header name to its canonical wire format.

    Standard headers use their canonical capitalization (e.g., [`Content_type]
    becomes ["Content-Type"]). [`Other] headers are returned as-is. *)

val of_string : string -> t
(** [of_string s] parses a string into a header name.

    Performs case-insensitive matching against known headers. Unknown headers
    are wrapped in [`Other]. *)

val to_lowercase_string : t -> string
(** [to_lowercase_string name] returns the lowercase form for internal use. *)

(** {1 Comparison} *)

val compare : t -> t -> int
(** [compare a b] compares two header names case-insensitively. *)

val equal : t -> t -> bool
(** [equal a b] checks equality of two header names case-insensitively. *)

(** {1 Pretty Printing} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf name] pretty-prints a header name. *)

(** {1 Header Categories} *)

val hop_by_hop_headers : t list
(** Default hop-by-hop headers per
    {{:https://datatracker.ietf.org/doc/html/rfc9110#section-7.6.1}RFC 9110 Section 7.6.1}.

    These headers MUST be removed before forwarding a message:
    Connection, Keep-Alive, Proxy-Authenticate, Proxy-Authorization,
    TE, Trailer, Transfer-Encoding, Upgrade, Via. *)

val forbidden_trailer_headers : t list
(** Headers that MUST NOT appear in trailers per
    {{:https://datatracker.ietf.org/doc/html/rfc9110#section-6.5.1}RFC 9110 Section 6.5.1}.

    Includes: Transfer-Encoding, Content-Length, Host, Content-Encoding,
    Content-Type, Trailer. *)

val cors_response_headers : t list
(** CORS response headers that control cross-origin access.
    @see <https://fetch.spec.whatwg.org/#http-responses> Fetch Standard *)

val cors_request_headers : t list
(** CORS request headers used in preflight requests.
    @see <https://fetch.spec.whatwg.org/#http-requests> Fetch Standard *)

val security_headers : t list
(** Headers related to web security policies. *)

val fetch_metadata_headers : t list
(** Browser-set headers providing request context.
    @see <https://www.w3.org/TR/fetch-metadata/> Fetch Metadata *)

val websocket_headers : t list
(** Headers used during WebSocket upgrade.
    @see <https://www.rfc-editor.org/rfc/rfc6455> RFC 6455 *)

val is_hop_by_hop : t -> bool
(** [is_hop_by_hop name] returns [true] if [name] is a hop-by-hop header. *)

val is_forbidden_trailer : t -> bool
(** [is_forbidden_trailer name] returns [true] if [name] is forbidden in trailers. *)

val is_cors_response : t -> bool
(** [is_cors_response name] returns [true] if [name] is a CORS response header. *)

val is_cors_request : t -> bool
(** [is_cors_request name] returns [true] if [name] is a CORS request header. *)

val is_security : t -> bool
(** [is_security name] returns [true] if [name] is a security header. *)

val is_fetch_metadata : t -> bool
(** [is_fetch_metadata name] returns [true] if [name] is a fetch metadata header. *)

val is_websocket : t -> bool
(** [is_websocket name] returns [true] if [name] is a WebSocket header. *)

(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP Header Names as Polymorphic Variants

    This module provides type-safe HTTP header names using polymorphic variants.
    All standard headers have dedicated variants, with [`Other] for non-standard
    or unknown headers. Header names are case-insensitive per RFC 9110 Section 5.1.

    Header definitions are based on the IANA HTTP Field Name Registry:
    {{:https://www.iana.org/assignments/http-fields/http-fields.xhtml} IANA HTTP Field Name Registry}

    @see <https://www.rfc-editor.org/rfc/rfc9110> RFC 9110: HTTP Semantics
    @see <https://www.rfc-editor.org/rfc/rfc9111> RFC 9111: HTTP Caching
    @see <https://www.rfc-editor.org/rfc/rfc9112> RFC 9112: HTTP/1.1 *)

(** {1 Standard HTTP Headers}

    These cover headers defined in:
    - RFC 9110 (HTTP Semantics)
    - RFC 9111 (HTTP Caching)
    - RFC 9112 (HTTP/1.1)
    - RFC 6455 (WebSocket Protocol)
    - RFC 9421 (HTTP Message Signatures)
    - RFC 9530 (Digest Fields)
    - Fetch Standard (CORS and Security)
    - Various other RFCs as noted *)
type standard = [
  (* {2 RFC 9110: HTTP Semantics - Content Headers} *)

  | `Accept
  (** @see <https://www.rfc-editor.org/rfc/rfc9110#section-12.5.1> RFC 9110 Section 12.5.1 *)
  | `Accept_encoding
  (** @see <https://www.rfc-editor.org/rfc/rfc9110#section-12.5.3> RFC 9110 Section 12.5.3 *)
  | `Accept_language
  (** @see <https://www.rfc-editor.org/rfc/rfc9110#section-12.5.4> RFC 9110 Section 12.5.4 *)
  | `Accept_ranges
  (** Indicates whether server supports range requests.
      @see <https://www.rfc-editor.org/rfc/rfc9110#section-14.3> RFC 9110 Section 14.3 *)
  | `Allow
  (** Lists HTTP methods supported by target resource.
      @see <https://www.rfc-editor.org/rfc/rfc9110#section-10.2.1> RFC 9110 Section 10.2.1 *)
  | `Content_encoding
  (** @see <https://www.rfc-editor.org/rfc/rfc9110#section-8.4> RFC 9110 Section 8.4 *)
  | `Content_language
  (** Natural language(s) of the intended audience.
      @see <https://www.rfc-editor.org/rfc/rfc9110#section-8.5> RFC 9110 Section 8.5 *)
  | `Content_length
  (** @see <https://www.rfc-editor.org/rfc/rfc9110#section-8.6> RFC 9110 Section 8.6 *)
  | `Content_location
  (** URI reference for the representation.
      @see <https://www.rfc-editor.org/rfc/rfc9110#section-8.7> RFC 9110 Section 8.7 *)
  | `Content_range
  (** Indicates which part of representation is enclosed (206 responses).
      @see <https://www.rfc-editor.org/rfc/rfc9110#section-14.4> RFC 9110 Section 14.4 *)
  | `Content_type
  (** @see <https://www.rfc-editor.org/rfc/rfc9110#section-8.3> RFC 9110 Section 8.3 *)

  (* {2 RFC 9110: HTTP Semantics - Request Context} *)

  | `Expect
  (** @see <https://www.rfc-editor.org/rfc/rfc9110#section-10.1.1> RFC 9110 Section 10.1.1 *)
  | `From
  (** Email address of the human user controlling the user agent.
      @see <https://www.rfc-editor.org/rfc/rfc9110#section-10.1.2> RFC 9110 Section 10.1.2 *)
  | `Host
  (** @see <https://www.rfc-editor.org/rfc/rfc9110#section-7.2> RFC 9110 Section 7.2 *)
  | `Max_forwards
  (** Limits forwarding of TRACE/OPTIONS requests.
      @see <https://www.rfc-editor.org/rfc/rfc9110#section-7.6.2> RFC 9110 Section 7.6.2 *)
  | `Range
  (** @see <https://www.rfc-editor.org/rfc/rfc9110#section-14.2> RFC 9110 Section 14.2 *)
  | `Referer
  (** URI of the resource from which request URI was obtained.
      Note: Header name is intentionally misspelled (historical).
      @see <https://www.rfc-editor.org/rfc/rfc9110#section-10.1.3> RFC 9110 Section 10.1.3 *)
  | `Te
  (** @see <https://www.rfc-editor.org/rfc/rfc9110#section-10.1.4> RFC 9110 Section 10.1.4 *)
  | `User_agent
  (** @see <https://www.rfc-editor.org/rfc/rfc9110#section-10.1.5> RFC 9110 Section 10.1.5 *)

  (* {2 RFC 9110: HTTP Semantics - Response Context} *)

  | `Location
  (** @see <https://www.rfc-editor.org/rfc/rfc9110#section-10.2.2> RFC 9110 Section 10.2.2 *)
  | `Retry_after
  (** @see <https://www.rfc-editor.org/rfc/rfc9110#section-10.2.3> RFC 9110 Section 10.2.3 *)
  | `Server
  (** Information about the origin server software.
      @see <https://www.rfc-editor.org/rfc/rfc9110#section-10.2.4> RFC 9110 Section 10.2.4 *)

  (* {2 RFC 9110: HTTP Semantics - Validators} *)

  | `Etag
  (** @see <https://www.rfc-editor.org/rfc/rfc9110#section-8.8.3> RFC 9110 Section 8.8.3 *)
  | `Last_modified
  (** @see <https://www.rfc-editor.org/rfc/rfc9110#section-8.8.2> RFC 9110 Section 8.8.2 *)
  | `Vary
  (** @see <https://www.rfc-editor.org/rfc/rfc9110#section-12.5.5> RFC 9110 Section 12.5.5 *)

  (* {2 RFC 9110: HTTP Semantics - Conditional Requests} *)

  | `If_match
  (** @see <https://www.rfc-editor.org/rfc/rfc9110#section-13.1.1> RFC 9110 Section 13.1.1 *)
  | `If_modified_since
  (** @see <https://www.rfc-editor.org/rfc/rfc9110#section-13.1.3> RFC 9110 Section 13.1.3 *)
  | `If_none_match
  (** @see <https://www.rfc-editor.org/rfc/rfc9110#section-13.1.2> RFC 9110 Section 13.1.2 *)
  | `If_range
  (** Makes Range request conditional on representation unchanged.
      @see <https://www.rfc-editor.org/rfc/rfc9110#section-13.1.5> RFC 9110 Section 13.1.5 *)
  | `If_unmodified_since
  (** @see <https://www.rfc-editor.org/rfc/rfc9110#section-13.1.4> RFC 9110 Section 13.1.4 *)

  (* {2 RFC 9110: HTTP Semantics - Authentication} *)

  | `Authorization
  (** @see <https://www.rfc-editor.org/rfc/rfc9110#section-11.6.2> RFC 9110 Section 11.6.2 *)
  | `Authentication_info
  (** Server sends after successful auth (e.g., nextnonce for Digest).
      @see <https://www.rfc-editor.org/rfc/rfc9110#section-11.6.3> RFC 9110 Section 11.6.3 *)
  | `Proxy_authenticate
  (** @see <https://www.rfc-editor.org/rfc/rfc9110#section-11.7.1> RFC 9110 Section 11.7.1 *)
  | `Proxy_authentication_info
  (** Proxy sends after successful auth.
      @see <https://www.rfc-editor.org/rfc/rfc9110#section-11.7.3> RFC 9110 Section 11.7.3 *)
  | `Proxy_authorization
  (** @see <https://www.rfc-editor.org/rfc/rfc9110#section-11.7.2> RFC 9110 Section 11.7.2 *)
  | `Www_authenticate
  (** @see <https://www.rfc-editor.org/rfc/rfc9110#section-11.6.1> RFC 9110 Section 11.6.1 *)

  (* {2 RFC 9110: HTTP Semantics - Connection Management} *)

  | `Connection
  (** @see <https://www.rfc-editor.org/rfc/rfc9110#section-7.6.1> RFC 9110 Section 7.6.1 *)
  | `Upgrade
  (** @see <https://www.rfc-editor.org/rfc/rfc9110#section-7.8> RFC 9110 Section 7.8 *)
  | `Via
  (** Records intermediate protocols and recipients (proxies/gateways).
      @see <https://www.rfc-editor.org/rfc/rfc9110#section-7.6.3> RFC 9110 Section 7.6.3 *)

  (* {2 RFC 9110: HTTP Semantics - Date} *)

  | `Date
  (** @see <https://www.rfc-editor.org/rfc/rfc9110#section-6.6.1> RFC 9110 Section 6.6.1 *)

  (* {2 RFC 9111: HTTP Caching} *)

  | `Age
  (** @see <https://www.rfc-editor.org/rfc/rfc9111#section-5.1> RFC 9111 Section 5.1 *)
  | `Cache_control
  (** @see <https://www.rfc-editor.org/rfc/rfc9111#section-5.2> RFC 9111 Section 5.2 *)
  | `Expires
  (** @see <https://www.rfc-editor.org/rfc/rfc9111#section-5.3> RFC 9111 Section 5.3 *)
  | `Pragma
  (** Deprecated but widely used for HTTP/1.0 compatibility.
      @see <https://www.rfc-editor.org/rfc/rfc9111#section-5.4> RFC 9111 Section 5.4
      @deprecated Use Cache-Control instead *)
  | `Cache_status
  (** Structured field indicating cache handling (hit/miss/etc).
      @see <https://www.rfc-editor.org/rfc/rfc9211> RFC 9211 *)

  (* {2 RFC 9112: HTTP/1.1} *)

  | `Keep_alive
  (** @see <https://www.rfc-editor.org/rfc/rfc2068#section-19.7.1> RFC 2068 Section 19.7.1 *)
  | `Trailer
  (** @see <https://www.rfc-editor.org/rfc/rfc9110#section-6.6.2> RFC 9110 Section 6.6.2 *)
  | `Transfer_encoding
  (** @see <https://www.rfc-editor.org/rfc/rfc9112#section-6.1> RFC 9112 Section 6.1 *)

  (* {2 Cookies - RFC 6265bis} *)

  | `Cookie
  (** @see <https://www.rfc-editor.org/rfc/rfc6265> RFC 6265 *)
  | `Set_cookie
  (** @see <https://www.rfc-editor.org/rfc/rfc6265> RFC 6265 *)

  (* {2 Link Relations - RFC 8288} *)

  | `Link
  (** @see <https://www.rfc-editor.org/rfc/rfc8288> RFC 8288 *)

  (* {2 CORS Headers - Fetch Standard}

      Cross-Origin Resource Sharing headers for controlling cross-origin requests.
      @see <https://fetch.spec.whatwg.org/> Fetch Standard *)

  | `Access_control_allow_credentials
  (** Whether response can be shared when credentials mode is "include".
      @see <https://fetch.spec.whatwg.org/#http-access-control-allow-credentials> Fetch *)
  | `Access_control_allow_headers
  (** Headers allowed in actual request.
      @see <https://fetch.spec.whatwg.org/#http-access-control-allow-headers> Fetch *)
  | `Access_control_allow_methods
  (** HTTP methods allowed for actual request.
      @see <https://fetch.spec.whatwg.org/#http-access-control-allow-methods> Fetch *)
  | `Access_control_allow_origin
  (** Whether response can be shared, by origin.
      @see <https://fetch.spec.whatwg.org/#http-access-control-allow-origin> Fetch *)
  | `Access_control_expose_headers
  (** Headers that can be exposed to the requesting script.
      @see <https://fetch.spec.whatwg.org/#http-access-control-expose-headers> Fetch *)
  | `Access_control_max_age
  (** How long preflight results can be cached.
      @see <https://fetch.spec.whatwg.org/#http-access-control-max-age> Fetch *)
  | `Access_control_request_headers
  (** Headers to be used in actual request (preflight).
      @see <https://fetch.spec.whatwg.org/#http-access-control-request-headers> Fetch *)
  | `Access_control_request_method
  (** Method to be used in actual request (preflight).
      @see <https://fetch.spec.whatwg.org/#http-access-control-request-method> Fetch *)
  | `Origin
  (** Origin of the request.
      @see <https://www.rfc-editor.org/rfc/rfc6454> RFC 6454 *)

  (* {2 Cross-Origin Policy Headers - HTML Standard} *)

  | `Cross_origin_embedder_policy
  (** Controls cross-origin embedding.
      @see <https://html.spec.whatwg.org/multipage/origin.html#coep> HTML *)
  | `Cross_origin_embedder_policy_report_only
  (** Report-only mode for COEP.
      @see <https://html.spec.whatwg.org/multipage/origin.html#coep> HTML *)
  | `Cross_origin_opener_policy
  (** Controls browsing context group sharing.
      @see <https://html.spec.whatwg.org/multipage/origin.html#cross-origin-opener-policies> HTML *)
  | `Cross_origin_opener_policy_report_only
  (** Report-only mode for COOP.
      @see <https://html.spec.whatwg.org/multipage/origin.html#cross-origin-opener-policies> HTML *)
  | `Cross_origin_resource_policy
  (** Controls no-cors cross-origin requests.
      @see <https://fetch.spec.whatwg.org/#cross-origin-resource-policy-header> Fetch *)

  (* {2 Fetch Metadata Headers - W3C}

      Request headers providing context about the request initiator.
      @see <https://www.w3.org/TR/fetch-metadata/> Fetch Metadata Request Headers *)

  | `Sec_fetch_dest
  (** Request destination (document, image, script, etc.).
      @see <https://www.w3.org/TR/fetch-metadata/#sec-fetch-dest-header> Fetch Metadata *)
  | `Sec_fetch_mode
  (** Request mode (cors, navigate, no-cors, same-origin, websocket).
      @see <https://www.w3.org/TR/fetch-metadata/#sec-fetch-mode-header> Fetch Metadata *)
  | `Sec_fetch_site
  (** Relationship between initiator and target (cross-site, same-origin, etc.).
      @see <https://www.w3.org/TR/fetch-metadata/#sec-fetch-site-header> Fetch Metadata *)
  | `Sec_fetch_user
  (** Whether navigation was user-activated.
      @see <https://www.w3.org/TR/fetch-metadata/#sec-fetch-user-header> Fetch Metadata *)

  (* {2 Security Headers} *)

  | `Content_security_policy
  (** Controls resources user agent is allowed to load.
      @see <https://www.w3.org/TR/CSP3/> Content Security Policy Level 3 *)
  | `Content_security_policy_report_only
  (** Report-only mode for CSP.
      @see <https://www.w3.org/TR/CSP3/> Content Security Policy Level 3 *)
  | `Strict_transport_security
  (** Instructs browser to only use HTTPS (HSTS).
      @see <https://www.rfc-editor.org/rfc/rfc6797> RFC 6797 *)
  | `X_content_type_options
  (** Prevents MIME type sniffing. Value: "nosniff".
      @see <https://fetch.spec.whatwg.org/#x-content-type-options-header> Fetch *)
  | `X_frame_options
  (** Controls whether page can be displayed in frame/iframe.
      @see <https://html.spec.whatwg.org/multipage/browsing-the-web.html#the-x-frame-options-header> HTML *)
  | `Referrer_policy
  (** Controls how much referrer info is included.
      @see <https://www.w3.org/TR/referrer-policy/> Referrer Policy *)

  (* {2 RFC 8053: Interactive Authentication} *)

  | `Optional_www_authenticate
  (** Offers authentication without requiring it (HTTP 200 with auth option).
      @see <https://www.rfc-editor.org/rfc/rfc8053#section-3> RFC 8053 Section 3 *)
  | `Authentication_control
  (** Controls authentication UI behavior.
      @see <https://www.rfc-editor.org/rfc/rfc8053#section-4> RFC 8053 Section 4 *)

  (* {2 RFC 9449: OAuth 2.0 DPoP} *)

  | `Dpop
  (** Demonstrating Proof of Possession token.
      @see <https://www.rfc-editor.org/rfc/rfc9449> RFC 9449 *)
  | `Dpop_nonce
  (** Server-provided nonce for DPoP.
      @see <https://www.rfc-editor.org/rfc/rfc9449> RFC 9449 *)

  (* {2 RFC 9530: Digest Fields} *)

  | `Content_digest
  (** Digest of message content (after content-coding).
      @see <https://www.rfc-editor.org/rfc/rfc9530#section-2> RFC 9530 Section 2 *)
  | `Repr_digest
  (** Digest of representation (before content-coding).
      @see <https://www.rfc-editor.org/rfc/rfc9530#section-3> RFC 9530 Section 3 *)
  | `Want_content_digest
  (** Request for Content-Digest in response.
      @see <https://www.rfc-editor.org/rfc/rfc9530#section-4> RFC 9530 Section 4 *)
  | `Want_repr_digest
  (** Request for Repr-Digest in response.
      @see <https://www.rfc-editor.org/rfc/rfc9530#section-4> RFC 9530 Section 4 *)

  (* {2 RFC 9421: HTTP Message Signatures} *)

  | `Signature
  (** Cryptographic signature over message components.
      @see <https://www.rfc-editor.org/rfc/rfc9421#section-4.2> RFC 9421 Section 4.2 *)
  | `Signature_input
  (** Metadata for signatures (components, algorithm, key ID, etc.).
      @see <https://www.rfc-editor.org/rfc/rfc9421#section-4.1> RFC 9421 Section 4.1 *)
  | `Accept_signature
  (** Indicates client can process signatures.
      @see <https://www.rfc-editor.org/rfc/rfc9421#section-5.1> RFC 9421 Section 5.1 *)

  (* {2 RFC 6455: WebSocket Protocol}

      Headers used during WebSocket HTTP Upgrade handshake.
      @see <https://www.rfc-editor.org/rfc/rfc6455> RFC 6455 *)

  | `Sec_websocket_key
  (** Client's base64-encoded 16-byte random nonce.
      @see <https://www.rfc-editor.org/rfc/rfc6455#section-4.1> RFC 6455 Section 4.1 *)
  | `Sec_websocket_accept
  (** Server's proof of handshake (SHA-1 of key + GUID, base64).
      @see <https://www.rfc-editor.org/rfc/rfc6455#section-4.2.2> RFC 6455 Section 4.2.2 *)
  | `Sec_websocket_protocol
  (** Subprotocol negotiation.
      @see <https://www.rfc-editor.org/rfc/rfc6455#section-11.3.4> RFC 6455 Section 11.3.4 *)
  | `Sec_websocket_version
  (** WebSocket protocol version (must be "13").
      @see <https://www.rfc-editor.org/rfc/rfc6455#section-4.1> RFC 6455 Section 4.1 *)
  | `Sec_websocket_extensions
  (** Extension negotiation (e.g., permessage-deflate).
      @see <https://www.rfc-editor.org/rfc/rfc6455#section-9> RFC 6455 Section 9 *)
]

(** Complete header name type including non-standard headers.

    Use [`Other name] for headers not in the standard set.
    The name should be provided in its canonical form (e.g., "X-Custom-Header"). *)
type t = [ standard | `Other of string ]

(** Convert a header name to its canonical wire format string.

    Standard headers are converted to their canonical capitalization.
    [`Other] headers are returned as-is. *)
let to_string : t -> string = function
  (* RFC 9110: Content *)
  | `Accept -> "Accept"
  | `Accept_encoding -> "Accept-Encoding"
  | `Accept_language -> "Accept-Language"
  | `Accept_ranges -> "Accept-Ranges"
  | `Allow -> "Allow"
  | `Content_encoding -> "Content-Encoding"
  | `Content_language -> "Content-Language"
  | `Content_length -> "Content-Length"
  | `Content_location -> "Content-Location"
  | `Content_range -> "Content-Range"
  | `Content_type -> "Content-Type"
  (* RFC 9110: Request Context *)
  | `Expect -> "Expect"
  | `From -> "From"
  | `Host -> "Host"
  | `Max_forwards -> "Max-Forwards"
  | `Range -> "Range"
  | `Referer -> "Referer"
  | `Te -> "TE"
  | `User_agent -> "User-Agent"
  (* RFC 9110: Response Context *)
  | `Location -> "Location"
  | `Retry_after -> "Retry-After"
  | `Server -> "Server"
  (* RFC 9110: Validators *)
  | `Etag -> "ETag"
  | `Last_modified -> "Last-Modified"
  | `Vary -> "Vary"
  (* RFC 9110: Conditional *)
  | `If_match -> "If-Match"
  | `If_modified_since -> "If-Modified-Since"
  | `If_none_match -> "If-None-Match"
  | `If_range -> "If-Range"
  | `If_unmodified_since -> "If-Unmodified-Since"
  (* RFC 9110: Authentication *)
  | `Authorization -> "Authorization"
  | `Authentication_info -> "Authentication-Info"
  | `Proxy_authenticate -> "Proxy-Authenticate"
  | `Proxy_authentication_info -> "Proxy-Authentication-Info"
  | `Proxy_authorization -> "Proxy-Authorization"
  | `Www_authenticate -> "WWW-Authenticate"
  (* RFC 9110: Connection *)
  | `Connection -> "Connection"
  | `Upgrade -> "Upgrade"
  | `Via -> "Via"
  (* RFC 9110: Date *)
  | `Date -> "Date"
  (* RFC 9111: Caching *)
  | `Age -> "Age"
  | `Cache_control -> "Cache-Control"
  | `Expires -> "Expires"
  | `Pragma -> "Pragma"
  | `Cache_status -> "Cache-Status"
  (* RFC 9112: HTTP/1.1 *)
  | `Keep_alive -> "Keep-Alive"
  | `Trailer -> "Trailer"
  | `Transfer_encoding -> "Transfer-Encoding"
  (* Cookies *)
  | `Cookie -> "Cookie"
  | `Set_cookie -> "Set-Cookie"
  (* Link *)
  | `Link -> "Link"
  (* CORS *)
  | `Access_control_allow_credentials -> "Access-Control-Allow-Credentials"
  | `Access_control_allow_headers -> "Access-Control-Allow-Headers"
  | `Access_control_allow_methods -> "Access-Control-Allow-Methods"
  | `Access_control_allow_origin -> "Access-Control-Allow-Origin"
  | `Access_control_expose_headers -> "Access-Control-Expose-Headers"
  | `Access_control_max_age -> "Access-Control-Max-Age"
  | `Access_control_request_headers -> "Access-Control-Request-Headers"
  | `Access_control_request_method -> "Access-Control-Request-Method"
  | `Origin -> "Origin"
  (* Cross-Origin Policy *)
  | `Cross_origin_embedder_policy -> "Cross-Origin-Embedder-Policy"
  | `Cross_origin_embedder_policy_report_only -> "Cross-Origin-Embedder-Policy-Report-Only"
  | `Cross_origin_opener_policy -> "Cross-Origin-Opener-Policy"
  | `Cross_origin_opener_policy_report_only -> "Cross-Origin-Opener-Policy-Report-Only"
  | `Cross_origin_resource_policy -> "Cross-Origin-Resource-Policy"
  (* Sec-Fetch *)
  | `Sec_fetch_dest -> "Sec-Fetch-Dest"
  | `Sec_fetch_mode -> "Sec-Fetch-Mode"
  | `Sec_fetch_site -> "Sec-Fetch-Site"
  | `Sec_fetch_user -> "Sec-Fetch-User"
  (* Security *)
  | `Content_security_policy -> "Content-Security-Policy"
  | `Content_security_policy_report_only -> "Content-Security-Policy-Report-Only"
  | `Strict_transport_security -> "Strict-Transport-Security"
  | `X_content_type_options -> "X-Content-Type-Options"
  | `X_frame_options -> "X-Frame-Options"
  | `Referrer_policy -> "Referrer-Policy"
  (* RFC 8053: Interactive Auth *)
  | `Optional_www_authenticate -> "Optional-WWW-Authenticate"
  | `Authentication_control -> "Authentication-Control"
  (* RFC 9449: DPoP *)
  | `Dpop -> "DPoP"
  | `Dpop_nonce -> "DPoP-Nonce"
  (* RFC 9530: Digest Fields *)
  | `Content_digest -> "Content-Digest"
  | `Repr_digest -> "Repr-Digest"
  | `Want_content_digest -> "Want-Content-Digest"
  | `Want_repr_digest -> "Want-Repr-Digest"
  (* RFC 9421: Signatures *)
  | `Signature -> "Signature"
  | `Signature_input -> "Signature-Input"
  | `Accept_signature -> "Accept-Signature"
  (* RFC 6455: WebSocket *)
  | `Sec_websocket_key -> "Sec-WebSocket-Key"
  | `Sec_websocket_accept -> "Sec-WebSocket-Accept"
  | `Sec_websocket_protocol -> "Sec-WebSocket-Protocol"
  | `Sec_websocket_version -> "Sec-WebSocket-Version"
  | `Sec_websocket_extensions -> "Sec-WebSocket-Extensions"
  (* Other *)
  | `Other s -> s

(** Convert a string to a header name.

    Performs case-insensitive matching against known headers.
    Unknown headers are wrapped in [`Other]. *)
let of_string s : t =
  match String.lowercase_ascii s with
  (* RFC 9110: Content *)
  | "accept" -> `Accept
  | "accept-encoding" -> `Accept_encoding
  | "accept-language" -> `Accept_language
  | "accept-ranges" -> `Accept_ranges
  | "allow" -> `Allow
  | "content-encoding" -> `Content_encoding
  | "content-language" -> `Content_language
  | "content-length" -> `Content_length
  | "content-location" -> `Content_location
  | "content-range" -> `Content_range
  | "content-type" -> `Content_type
  (* RFC 9110: Request Context *)
  | "expect" -> `Expect
  | "from" -> `From
  | "host" -> `Host
  | "max-forwards" -> `Max_forwards
  | "range" -> `Range
  | "referer" -> `Referer
  | "te" -> `Te
  | "user-agent" -> `User_agent
  (* RFC 9110: Response Context *)
  | "location" -> `Location
  | "retry-after" -> `Retry_after
  | "server" -> `Server
  (* RFC 9110: Validators *)
  | "etag" -> `Etag
  | "last-modified" -> `Last_modified
  | "vary" -> `Vary
  (* RFC 9110: Conditional *)
  | "if-match" -> `If_match
  | "if-modified-since" -> `If_modified_since
  | "if-none-match" -> `If_none_match
  | "if-range" -> `If_range
  | "if-unmodified-since" -> `If_unmodified_since
  (* RFC 9110: Authentication *)
  | "authorization" -> `Authorization
  | "authentication-info" -> `Authentication_info
  | "proxy-authenticate" -> `Proxy_authenticate
  | "proxy-authentication-info" -> `Proxy_authentication_info
  | "proxy-authorization" -> `Proxy_authorization
  | "www-authenticate" -> `Www_authenticate
  (* RFC 9110: Connection *)
  | "connection" -> `Connection
  | "upgrade" -> `Upgrade
  | "via" -> `Via
  (* RFC 9110: Date *)
  | "date" -> `Date
  (* RFC 9111: Caching *)
  | "age" -> `Age
  | "cache-control" -> `Cache_control
  | "expires" -> `Expires
  | "pragma" -> `Pragma
  | "cache-status" -> `Cache_status
  (* RFC 9112: HTTP/1.1 *)
  | "keep-alive" -> `Keep_alive
  | "trailer" -> `Trailer
  | "transfer-encoding" -> `Transfer_encoding
  (* Cookies *)
  | "cookie" -> `Cookie
  | "set-cookie" -> `Set_cookie
  (* Link *)
  | "link" -> `Link
  (* CORS *)
  | "access-control-allow-credentials" -> `Access_control_allow_credentials
  | "access-control-allow-headers" -> `Access_control_allow_headers
  | "access-control-allow-methods" -> `Access_control_allow_methods
  | "access-control-allow-origin" -> `Access_control_allow_origin
  | "access-control-expose-headers" -> `Access_control_expose_headers
  | "access-control-max-age" -> `Access_control_max_age
  | "access-control-request-headers" -> `Access_control_request_headers
  | "access-control-request-method" -> `Access_control_request_method
  | "origin" -> `Origin
  (* Cross-Origin Policy *)
  | "cross-origin-embedder-policy" -> `Cross_origin_embedder_policy
  | "cross-origin-embedder-policy-report-only" -> `Cross_origin_embedder_policy_report_only
  | "cross-origin-opener-policy" -> `Cross_origin_opener_policy
  | "cross-origin-opener-policy-report-only" -> `Cross_origin_opener_policy_report_only
  | "cross-origin-resource-policy" -> `Cross_origin_resource_policy
  (* Sec-Fetch *)
  | "sec-fetch-dest" -> `Sec_fetch_dest
  | "sec-fetch-mode" -> `Sec_fetch_mode
  | "sec-fetch-site" -> `Sec_fetch_site
  | "sec-fetch-user" -> `Sec_fetch_user
  (* Security *)
  | "content-security-policy" -> `Content_security_policy
  | "content-security-policy-report-only" -> `Content_security_policy_report_only
  | "strict-transport-security" -> `Strict_transport_security
  | "x-content-type-options" -> `X_content_type_options
  | "x-frame-options" -> `X_frame_options
  | "referrer-policy" -> `Referrer_policy
  (* RFC 8053: Interactive Auth *)
  | "optional-www-authenticate" -> `Optional_www_authenticate
  | "authentication-control" -> `Authentication_control
  (* RFC 9449: DPoP *)
  | "dpop" -> `Dpop
  | "dpop-nonce" -> `Dpop_nonce
  (* RFC 9530: Digest Fields *)
  | "content-digest" -> `Content_digest
  | "repr-digest" -> `Repr_digest
  | "want-content-digest" -> `Want_content_digest
  | "want-repr-digest" -> `Want_repr_digest
  (* RFC 9421: Signatures *)
  | "signature" -> `Signature
  | "signature-input" -> `Signature_input
  | "accept-signature" -> `Accept_signature
  (* RFC 6455: WebSocket *)
  | "sec-websocket-key" -> `Sec_websocket_key
  | "sec-websocket-accept" -> `Sec_websocket_accept
  | "sec-websocket-protocol" -> `Sec_websocket_protocol
  | "sec-websocket-version" -> `Sec_websocket_version
  | "sec-websocket-extensions" -> `Sec_websocket_extensions
  (* Other *)
  | _ -> `Other s

(** Convert to lowercase string for internal map keys. *)
let to_lowercase_string (name : t) : string =
  match name with
  | `Other s -> String.lowercase_ascii s
  | #standard as s -> String.lowercase_ascii (to_string s)

(** Compare two header names (case-insensitive). *)
let compare (a : t) (b : t) : int =
  String.compare (to_lowercase_string a) (to_lowercase_string b)

(** Check equality of two header names (case-insensitive). *)
let equal (a : t) (b : t) : bool =
  compare a b = 0

(** Pretty printer for header names. *)
let pp ppf name =
  Format.pp_print_string ppf (to_string name)

(** {1 Header Categories}

    Useful groupings for protocol handling. *)

(** Default hop-by-hop headers per RFC 9110 Section 7.6.1.

    These headers MUST be removed before forwarding a message. *)
let hop_by_hop_headers : t list = [
  `Connection;
  `Keep_alive;
  `Proxy_authenticate;
  `Proxy_authorization;
  `Te;
  `Trailer;
  `Transfer_encoding;
  `Upgrade;
  `Via;
]

(** Headers that MUST NOT appear in trailers per RFC 9110 Section 6.5.1. *)
let forbidden_trailer_headers : t list = [
  `Transfer_encoding;
  `Content_length;
  `Host;
  `Content_encoding;
  `Content_type;
  `Trailer;
]

(** CORS response headers.

    These headers control cross-origin access.
    @see <https://fetch.spec.whatwg.org/#http-responses> Fetch Standard *)
let cors_response_headers : t list = [
  `Access_control_allow_credentials;
  `Access_control_allow_headers;
  `Access_control_allow_methods;
  `Access_control_allow_origin;
  `Access_control_expose_headers;
  `Access_control_max_age;
]

(** CORS request headers.

    These headers are used in CORS preflight requests.
    @see <https://fetch.spec.whatwg.org/#http-requests> Fetch Standard *)
let cors_request_headers : t list = [
  `Access_control_request_headers;
  `Access_control_request_method;
  `Origin;
]

(** Security headers.

    Headers related to web security policies. *)
let security_headers : t list = [
  `Content_security_policy;
  `Content_security_policy_report_only;
  `Strict_transport_security;
  `X_content_type_options;
  `X_frame_options;
  `Referrer_policy;
  `Cross_origin_embedder_policy;
  `Cross_origin_embedder_policy_report_only;
  `Cross_origin_opener_policy;
  `Cross_origin_opener_policy_report_only;
  `Cross_origin_resource_policy;
]

(** Fetch metadata headers.

    Browser-set headers providing request context.
    @see <https://www.w3.org/TR/fetch-metadata/> Fetch Metadata *)
let fetch_metadata_headers : t list = [
  `Sec_fetch_dest;
  `Sec_fetch_mode;
  `Sec_fetch_site;
  `Sec_fetch_user;
]

(** WebSocket handshake headers.

    Headers used during WebSocket upgrade.
    @see <https://www.rfc-editor.org/rfc/rfc6455> RFC 6455 *)
let websocket_headers : t list = [
  `Sec_websocket_key;
  `Sec_websocket_accept;
  `Sec_websocket_protocol;
  `Sec_websocket_version;
  `Sec_websocket_extensions;
]

(** Check if a header is a hop-by-hop header. *)
let is_hop_by_hop (name : t) : bool =
  List.exists (equal name) hop_by_hop_headers

(** Check if a header is forbidden in trailers. *)
let is_forbidden_trailer (name : t) : bool =
  List.exists (equal name) forbidden_trailer_headers

(** Check if a header is a CORS response header. *)
let is_cors_response (name : t) : bool =
  List.exists (equal name) cors_response_headers

(** Check if a header is a CORS request header. *)
let is_cors_request (name : t) : bool =
  List.exists (equal name) cors_request_headers

(** Check if a header is a security header. *)
let is_security (name : t) : bool =
  List.exists (equal name) security_headers

(** Check if a header is a fetch metadata header. *)
let is_fetch_metadata (name : t) : bool =
  List.exists (equal name) fetch_metadata_headers

(** Check if a header is a WebSocket header. *)
let is_websocket (name : t) : bool =
  List.exists (equal name) websocket_headers

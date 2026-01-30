(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Redirect handling and cross-origin security utilities

    This module provides shared functions for handling HTTP redirects safely,
    including cross-origin detection and sensitive header stripping. *)

val src : Logs.src
(** Logs source for this module *)

(** {1 Cross-Origin Detection} *)

val same_origin : Uri.t -> Uri.t -> bool
(** [same_origin uri1 uri2] returns [true] if both URIs have the same origin.
    Same origin means same host with same scheme, or http->https upgrade.
    Used to determine if sensitive headers should be preserved during redirects. *)

(** {1 Sensitive Header Protection} *)

val strip_sensitive_headers : Headers.t -> Headers.t
(** [strip_sensitive_headers headers] removes sensitive headers that should not
    be sent to cross-origin destinations:
    - Authorization
    - Cookie
    - Proxy-Authorization
    - WWW-Authenticate *)

(** {1 Redirect URL Validation} *)

val allowed_schemes : string list
(** List of allowed URL schemes for redirects: ["http"; "https"] *)

val validate_url : string -> Uri.t
(** [validate_url location] validates that the redirect URL uses an allowed scheme.
    @raise Error.Invalid_redirect if scheme is not http or https *)

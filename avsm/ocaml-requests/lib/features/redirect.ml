(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Redirect handling and cross-origin security utilities

    This module provides shared functions for handling HTTP redirects safely,
    including cross-origin detection and sensitive header stripping. *)

let src = Logs.Src.create "requests.redirect" ~doc:"HTTP Redirect Handling"
module Log = (val Logs.src_log src : Logs.LOG)

(** {1 Cross-Origin Detection} *)

(** Get the effective port for a URI, using default ports for http/https.
    Per RFC 6454, the port is part of the origin tuple. *)
let effective_port uri =
  match Uri.port uri with
  | Some p -> p
  | None ->
      match Uri.scheme uri with
      | Some "https" -> 443
      | Some "http" | None -> 80
      | Some _ -> 80  (* Default for unknown schemes *)

(** Check if two URIs have the same origin for security purposes.
    Per RFC 6454 (Web Origin), origins are tuples of (scheme, host, port).
    Used to determine if sensitive headers (Authorization, Cookie) should be
    stripped during redirects. Following Python requests behavior:
    - Same host, same scheme, same port = same origin
    - http -> https upgrade on same host with default ports = allowed (more secure)
    TODO: Support .netrc for re-acquiring auth credentials on new hosts *)
let same_origin uri1 uri2 =
  let host1 = Uri.host uri1 |> Option.map String.lowercase_ascii in
  let host2 = Uri.host uri2 |> Option.map String.lowercase_ascii in
  let scheme1 = Uri.scheme uri1 |> Option.value ~default:"http" in
  let scheme2 = Uri.scheme uri2 |> Option.value ~default:"http" in
  let port1 = effective_port uri1 in
  let port2 = effective_port uri2 in
  match host1, host2 with
  | Some h1, Some h2 when String.equal h1 h2 ->
      if String.equal scheme1 scheme2 && port1 = port2 then
        (* Same scheme, host, and port = same origin *)
        true
      else if scheme1 = "http" && scheme2 = "https" && port1 = 80 && port2 = 443 then
        (* http->https upgrade on default ports is allowed (more secure) *)
        true
      else
        false
  | _ -> false

(** {1 Sensitive Header Protection} *)

(** Strip sensitive headers for cross-origin redirects to prevent credential leakage.
    Per Recommendation #1: Also strip Cookie, Proxy-Authorization, WWW-Authenticate *)
let strip_sensitive_headers headers =
  headers
  |> Headers.remove `Authorization
  |> Headers.remove `Cookie
  |> Headers.remove `Proxy_authorization
  |> Headers.remove `Www_authenticate

(** {1 Redirect URL Validation} *)

(** Allowed redirect URL schemes to prevent SSRF attacks.
    Per Recommendation #5: Only allow http:// and https:// schemes *)
let allowed_schemes = ["http"; "https"]

(** Validate redirect URL scheme to prevent SSRF attacks.
    Per Recommendation #5: Only allow http:// and https:// schemes.
    @raise Error.Invalid_redirect if scheme is not allowed *)
let validate_url location =
  let uri = Uri.of_string location in
  match Uri.scheme uri with
  | Some scheme when List.mem (String.lowercase_ascii scheme) allowed_schemes ->
      uri
  | Some scheme ->
      raise (Error.invalid_redirectf ~url:location "Disallowed redirect scheme: %s" scheme)
  | None ->
      uri  (* Relative URLs are OK - they will be resolved against current URL *)

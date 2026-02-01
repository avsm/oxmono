(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** JWT payload decoding for AT Protocol tokens.

    This module decodes JWT (JSON Web Token) payloads to check expiration times
    using the {{:https://github.com/avsm/ocaml-jsonwt}jsonwt} library. It does
    {b not} verify signatures - that is the server's responsibility.

    AT Protocol uses JWTs for access and refresh tokens. The access token
    typically has a short expiry (e.g., 2 hours) while the refresh token lasts
    longer (e.g., 90 days).

    {2 Usage}

    {[
      let five_minutes = Ptime.Span.of_int_s 300 in
      if Xrpc_jwt.is_expired ~leeway:five_minutes session.access_jwt then
        (* Token will expire within 5 minutes, refresh it *)
        refresh_session ()
    ]} *)

(** {1 JWT Payload} *)

type payload = {
  exp : Ptime.t option;  (** Expiration time. *)
  iat : Ptime.t option;  (** Issued-at time. *)
  sub : string option;  (** Subject (typically the DID). *)
  aud : string option;  (** Audience (typically the PDS URL). *)
  iss : string option;  (** Issuer (typically the PDS DID). *)
}
(** JWT payload fields. All fields are optional since JWTs may omit them. *)

(** {1 Decoding} *)

val decode_payload : string -> (payload, string) result
(** [decode_payload jwt] extracts and decodes the payload from a JWT string.

    Uses {{:https://datatracker.ietf.org/doc/html/rfc7519}RFC 7519} compliant
    parsing via the jsonwt library.

    Returns [Error msg] if the JWT cannot be parsed. *)

(** {1 Expiration Checking} *)

val is_expired : ?leeway:Ptime.Span.t -> string -> bool
(** [is_expired ?leeway jwt] returns [true] if the token has expired or will
    expire within [leeway].

    @param leeway
      Safety margin before actual expiry (default: 60 seconds). This allows time
      for the refresh request to complete before the token becomes invalid.

    Returns [true] if decoding fails (fail-safe behavior). *)

val get_expiration : string -> Ptime.t option
(** [get_expiration jwt] returns the expiration time if present. Returns [None]
    if the JWT cannot be decoded or has no [exp] claim. *)

val time_to_expiry : string -> Ptime.Span.t option
(** [time_to_expiry jwt] returns the time remaining until expiration. Returns
    [None] if the JWT cannot be decoded, has no [exp] claim, or is already
    expired. *)

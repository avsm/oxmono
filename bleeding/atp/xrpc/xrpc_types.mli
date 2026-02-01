(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Core XRPC data types.

    This module defines the fundamental types used in AT Protocol XRPC
    communication, with jsont codecs for JSON serialization.

    {2 Session}

    The {!type:session} type represents an authenticated user session. Sessions
    contain JWT tokens for authentication and metadata about the user.

    {2 Error Payload}

    XRPC error responses follow a standard format with an error code and
    optional message. The {!error_payload} type represents this structure. *)

(** {1 Session} *)

type session = {
  access_jwt : string;
      (** Access token (short-lived JWT, typically 2 hours). *)
  refresh_jwt : string;
      (** Refresh token (long-lived JWT, typically 90 days). *)
  did : string;  (** Decentralized Identifier (e.g., ["did:plc:abcd1234"]). *)
  handle : string;  (** User handle (e.g., ["alice.bsky.social"]). *)
  pds_uri : string option;
      (** Personal Data Server URI if different from service. *)
  email : string option;  (** Account email address. *)
  email_confirmed : bool option;  (** Whether email has been confirmed. *)
  email_auth_factor : bool option;  (** Whether email is used as auth factor. *)
  active : bool option;  (** Whether the account is active. *)
  status : string option;
      (** Account status (e.g., ["takendown"], ["suspended"]). *)
}
(** Authenticated session state from [com.atproto.server.createSession] or
    [com.atproto.server.refreshSession]. *)

val session_jsont : session Jsont.t
(** jsont codec for sessions. *)

(** {1 Error Payload} *)

type error_payload = {
  error : string;
      (** Error code (e.g., ["ExpiredToken"], ["InvalidHandle"]). *)
  message : string option;  (** Human-readable error description. *)
}
(** XRPC error response body. *)

val error_payload_jsont : error_payload Jsont.t
(** jsont codec for error payloads. *)

(** {1 Login Request} *)

type login_request = {
  identifier : string;  (** Handle or DID to authenticate as. *)
  password : string;  (** Account password or app password. *)
  auth_factor_token : string option;
      (** Two-factor authentication token if required. *)
}
(** Request body for [com.atproto.server.createSession]. *)

val login_request_jsont : login_request Jsont.t
(** jsont codec for login requests. *)

(** {1 Empty Response} *)

val empty_jsont : unit Jsont.t
(** jsont codec for empty responses (decodes any JSON to [()]). *)

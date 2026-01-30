(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP status codes per {{:https://datatracker.ietf.org/doc/html/rfc9110#section-15}RFC 9110 Section 15}

    This module provides types and functions for working with HTTP response
    status codes. Status codes are three-digit integers that indicate the
    result of an HTTP request.

    {2 Status Code Classes}

    - {b 1xx Informational}: Request received, continuing process
    - {b 2xx Success}: Request successfully received, understood, and accepted
    - {b 3xx Redirection}: Further action needed to complete the request
    - {b 4xx Client Error}: Request contains bad syntax or cannot be fulfilled
    - {b 5xx Server Error}: Server failed to fulfill a valid request *)

(** Log source for status code operations *)
val src : Logs.Src.t

(** {1 Status Categories} *)

type informational = [
  | `Continue  (** 100 - Client should continue with request *)
  | `Switching_protocols  (** 101 - Server is switching protocols *)
  | `Processing  (** 102 - Server has received and is processing the request *)
  | `Early_hints  (** 103 - Used to return some response headers before final HTTP message *)
]
(** 1xx Informational responses *)

type success = [
  | `OK  (** 200 - Standard response for successful HTTP requests *)
  | `Created  (** 201 - Request has been fulfilled; new resource created *)
  | `Accepted  (** 202 - Request accepted, processing pending *)
  | `Non_authoritative_information  (** 203 - Request processed, information may be from another source *)
  | `No_content  (** 204 - Request processed, no content returned *)
  | `Reset_content  (** 205 - Request processed, no content returned, reset document view *)
  | `Partial_content  (** 206 - Partial resource return due to request header *)
  | `Multi_status  (** 207 - XML, can contain multiple separate responses *)
  | `Already_reported  (** 208 - Results previously returned *)
  | `Im_used  (** 226 - Request fulfilled, response is instance-manipulations *)
]
(** 2xx Success responses *)

type redirection = [
  | `Multiple_choices  (** 300 - Multiple options for the resource delivered *)
  | `Moved_permanently  (** 301 - This and all future requests directed to the given URI *)
  | `Found  (** 302 - Temporary response to request found via alternative URI *)
  | `See_other  (** 303 - Response to request found via alternative URI *)
  | `Not_modified  (** 304 - Resource has not been modified since last requested *)
  | `Use_proxy  (** 305 - Content located elsewhere, retrieve from there (deprecated) *)
  | `Temporary_redirect  (** 307 - Connect again to different URI as provided *)
  | `Permanent_redirect  (** 308 - Connect again to a different URI using the same method *)
]
(** 3xx Redirection messages *)

type client_error = [
  | `Bad_request  (** 400 - Request cannot be fulfilled due to bad syntax *)
  | `Unauthorized  (** 401 - Authentication is possible but has failed *)
  | `Payment_required  (** 402 - Payment required, reserved for future use *)
  | `Forbidden  (** 403 - Server refuses to respond to request *)
  | `Not_found  (** 404 - Requested resource could not be found *)
  | `Method_not_allowed  (** 405 - Request method not supported by that resource *)
  | `Not_acceptable  (** 406 - Content not acceptable according to the Accept headers *)
  | `Proxy_authentication_required  (** 407 - Client must first authenticate itself with the proxy *)
  | `Request_timeout  (** 408 - Server timed out waiting for the request *)
  | `Conflict  (** 409 - Request could not be processed because of conflict *)
  | `Gone  (** 410 - Resource is no longer available and will not be available again *)
  | `Length_required  (** 411 - Request did not specify the length of its content *)
  | `Precondition_failed  (** 412 - Server does not meet request preconditions *)
  | `Payload_too_large  (** 413 - Request is larger than the server is willing or able to process *)
  | `Uri_too_long  (** 414 - URI provided was too long for the server to process *)
  | `Unsupported_media_type  (** 415 - Server does not support media type *)
  | `Range_not_satisfiable  (** 416 - Client has asked for unprovidable portion of the file *)
  | `Expectation_failed  (** 417 - Server cannot meet requirements of Expect request-header field *)
  | `I_m_a_teapot  (** 418 - I'm a teapot (RFC 2324) *)
  | `Misdirected_request  (** 421 - Request was directed at a server that is not able to produce a response *)
  | `Unprocessable_entity  (** 422 - Request unable to be followed due to semantic errors *)
  | `Locked  (** 423 - Resource that is being accessed is locked *)
  | `Failed_dependency  (** 424 - Request failed due to failure of a previous request *)
  | `Too_early  (** 425 - Server is unwilling to risk processing a request that might be replayed *)
  | `Upgrade_required  (** 426 - Client should switch to a different protocol *)
  | `Precondition_required  (** 428 - Origin server requires the request to be conditional *)
  | `Too_many_requests  (** 429 - User has sent too many requests in a given amount of time *)
  | `Request_header_fields_too_large  (** 431 - Server is unwilling to process the request *)
  | `Unavailable_for_legal_reasons  (** 451 - Resource unavailable for legal reasons *)
]
(** 4xx Client error responses *)

type server_error = [
  | `Internal_server_error  (** 500 - Generic error message *)
  | `Not_implemented  (** 501 - Server does not recognise method or lacks ability to fulfill *)
  | `Bad_gateway  (** 502 - Server received an invalid response from upstream server *)
  | `Service_unavailable  (** 503 - Server is currently unavailable *)
  | `Gateway_timeout  (** 504 - Gateway did not receive response from upstream server *)
  | `Http_version_not_supported  (** 505 - Server does not support the HTTP protocol version *)
  | `Variant_also_negotiates  (** 506 - Content negotiation for the request results in a circular reference *)
  | `Insufficient_storage  (** 507 - Server is unable to store the representation *)
  | `Loop_detected  (** 508 - Server detected an infinite loop while processing the request *)
  | `Not_extended  (** 510 - Further extensions to the request are required *)
  | `Network_authentication_required  (** 511 - Client needs to authenticate to gain network access *)
]
(** 5xx Server error responses *)

type standard = [
  | informational
  | success
  | redirection
  | client_error
  | server_error
]
(** All standard HTTP status codes *)

type t = [
  | `Code of int  (** Any status code as an integer *)
  | standard
]
(** HTTP status type *)

(** {1 Conversion Functions} *)

val to_int : t -> int
(** Convert status to its integer code *)

val of_int : int -> t
(** Convert an integer to a status *)

val to_string : t -> string
(** Get the string representation of a status code (e.g., "200", "404") *)

val reason_phrase : t -> string
(** Get the standard reason phrase for a status code (e.g., "OK", "Not Found") *)

(** {1 Classification Functions} *)

val is_informational : t -> bool
(** Check if status code is informational (1xx) *)

val is_success : t -> bool
(** Check if status code indicates success (2xx) *)

val is_redirection : t -> bool
(** Check if status code indicates redirection (3xx) *)

val is_client_error : t -> bool
(** Check if status code indicates client error (4xx) *)

val is_server_error : t -> bool
(** Check if status code indicates server error (5xx) *)

val is_error : t -> bool
(** Check if status code indicates any error (4xx or 5xx) *)

(** {1 Retry Policy} *)

val is_retryable : t -> bool
(** Check if a status code suggests the request could be retried.
    Returns true for:
    - 408 Request Timeout
    - 429 Too Many Requests
    - 502 Bad Gateway
    - 503 Service Unavailable
    - 504 Gateway Timeout
    - Any 5xx errors *)

val should_retry_on_different_host : t -> bool
(** Check if a status code suggests retrying on a different host might help.
    Returns true for:
    - 502 Bad Gateway
    - 503 Service Unavailable
    - 504 Gateway Timeout *)

(** {1 Pretty Printing} *)

val pp : Format.formatter -> t -> unit
(** Pretty printer for status codes *)

val pp_hum : Format.formatter -> t -> unit
(** Human-readable pretty printer that includes both code and reason phrase *)